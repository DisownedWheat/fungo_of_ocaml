open Core

let keyword_map =
  [ ("let", fun state -> Token.Let state)
  ; ("open", fun state -> Token.Import state)
  ; ("of", fun state -> Token.Of state)
  ; ("go", fun state -> Token.Go state)
  ; ("if", fun state -> Token.If state)
  ; ("then", fun state -> Token.Then state)
  ; ("else", fun state -> Token.Else state)
  ; ("true", fun state -> Token.True state)
  ; ("false", fun state -> Token.False state)
  ; ("match", fun state -> Token.Match state)
  ; ("function", fun state -> Token.FunctionMatch state)
  ; ("with", fun state -> Token.With state)
  ; ("when", fun state -> Token.When state)
  ; ("type", fun state -> Token.TypeKeyword state)
  ; ("mutable", fun state -> Token.Mut state)
  ; ("module", fun state -> Token.Module state)
  ; ("fun", fun state -> Token.Lambda state)
  ; ("for", fun state -> Token.For state)
  ; ("in", fun state -> Token.In state)
  ; ("while", fun state -> Token.While state)
  ; ("do", fun state -> Token.Do state)
  ; ("end", fun state -> Token.End state)
  ; ("rec", fun state -> Token.Rec state)
  ]
;;

type state =
  { input : char list
  ; tokens : Token.t list
  ; current_line : int
  ; current_column : int
  ; current_position : int
  ; buffer : char list
  }
[@@deriving show]

type lexer_error =
  | UnterminatedString of Token.position
  | UnexpectedError of state
  | InvalidNumber of Token.position
  | InvalidCharLiteral of Token.position
[@@deriving show]

type lexer_result =
  | Ok of state
  | Err of lexer_error
  | Finished of state

let op_chars =
  [ '\''
  ; '-'
  ; '='
  ; '+'
  ; '['
  ; ']'
  ; ':'
  ; ','
  ; '.'
  ; '/'
  ; '?'
  ; '~'
  ; '!'
  ; '@'
  ; '#'
  ; '%'
  ; '^'
  ; '&'
  ; '*'
  ; '$'
  ]
;;

let is_op_char c = List.find op_chars ~f:(Char.equal c) |> Option.is_some
let check_num_char c = Char.is_digit c || Char.equal c '.' || Char.equal c '_'
let check_ident_char c = Char.is_alphanum c || Char.equal '_' c

let check_keyword value =
  List.find keyword_map ~f:(fun (str, _) -> String.equal str value)
  |> Option.map ~f:Tuple.T2.get2
;;

let get_error_position_buffer state =
  state.current_position - List.length state.buffer, state.current_position
;;

let make_token state const value =
  { state with
    tokens =
      Token.make
        ~line:state.current_line
        ~column:state.current_column
        ~value
        ~position:(state.current_position - String.length value, state.current_position)
        ~const
      :: state.tokens
  }
;;

let collapse buffer = List.rev buffer |> String.of_list

let collapse_buffer state kind =
  let str = state.buffer |> List.rev |> String.of_list in
  let actual_kind = check_keyword str |> Option.value ~default:kind in
  let record =
    actual_kind
      { Token.line = state.current_line
      ; column = state.current_column
      ; position = state.current_position - String.length str, state.current_position
      ; value = str
      }
  in
  { state with buffer = []; tokens = record :: state.tokens }
;;

let move_state new_input state =
  { state with
    input = new_input
  ; current_position =
      state.current_position + (List.length state.input - List.length new_input)
  }
;;

let collapse_number state =
  match
    ( List.for_all state.buffer ~f:(fun x ->
        Char.is_digit x || Char.equal x '.' || Char.equal x '_')
    , List.find state.buffer ~f:(fun x -> Char.equal x '.') )
  with
  | false, _ -> Err (InvalidNumber (get_error_position_buffer state))
  | true, Some _ -> Ok (collapse_buffer state (fun s -> FloatLiteral s))
  | true, None -> Ok (collapse_buffer state (fun s -> IntLiteral s))
;;

let rec lex_number state =
  match state.input with
  | c :: rest when check_num_char c ->
    lex_number { state with buffer = c :: state.buffer; input = rest }
  | _ -> collapse_number state
;;

let rec lex_string state =
  match state.input with
  | '"' :: rest ->
    let new_state = collapse_buffer state (fun s -> StringLiteral s) in
    Ok
      { new_state with
        input = rest
      ; current_column = new_state.current_column + 1
      ; current_position = new_state.current_position + 1
      }
  | c :: rest ->
    lex_string
      { state with
        buffer = c :: state.buffer
      ; input = rest
      ; current_position = state.current_position + 1
      }
  | _ -> Err (UnterminatedString (get_error_position_buffer state))
;;

let rec lex_ident state =
  match state.input with
  | c :: rest when check_ident_char c ->
    move_state rest { state with buffer = c :: state.buffer } |> lex_ident
  | c :: '\'' :: ws :: rest when Char.is_whitespace ws ->
    Ok
      { (collapse_buffer
           { state with buffer = c :: '\'' :: state.buffer }
           (fun s -> Identifier s))
        with
        input = rest
      }
  | _ -> Ok (collapse_buffer state (fun s -> Identifier s))
;;

let lex_char state =
  match state.input with
  | '\'' :: char :: '\'' :: rest ->
    Ok (make_token state (fun s -> CharLiteral s) (Char.to_string char) |> move_state rest)
  | _ -> Err (InvalidCharLiteral (state.current_position, state.current_position + 1))
;;

let rec lex_op state =
  match state.buffer, state.input with
  | _, c :: rest when is_op_char c ->
    lex_op (move_state rest { state with buffer = c :: state.buffer })
  | [], '&' :: next :: rest when check_ident_char next ->
    Ok (move_state rest (make_token state (fun s -> Operator (Pointer, s)) "&"))
  | [], '*' :: next :: rest when check_ident_char next ->
    Ok (move_state rest (make_token state (fun s -> Operator (Deref, s)) "*"))
  | _ ->
    Ok
      (collapse_buffer state (fun s ->
         match s.value with
         | "." -> Operator (Dot, s)
         | "=" -> Operator (Assign, s)
         | ":" -> Operator (Colon, s)
         | ";" -> Operator (Semicolon, s)
         | "," -> Operator (Comma, s)
         | "|" -> Operator (Pipe, s)
         | str -> Operator (Other str, s)))
;;

let rec lex_comment state buffer =
  match state.input with
  | '/' :: '/' :: rest -> lex_comment (move_state rest state) buffer
  | '\n' :: rest ->
    Ok (move_state rest (make_token state (fun s -> Comment s) (collapse buffer)))
  | c :: rest -> lex_comment (move_state rest state) (c :: buffer)
  | [] -> Ok (make_token state (fun s -> Comment s) (collapse buffer))
;;

let rec inner_lex state_result =
  match state_result with
  | Ok state ->
    (match state.input with
     | c :: '[' :: rest when Char.is_whitespace c ->
       Ok (make_token state (fun s -> Operator (LBracket, s)) "[" |> move_state rest)
     | '[' :: rest ->
       Ok (make_token state (fun s -> Operator (IndexBracket, s)) "[" |> move_state rest)
     | ']' :: rest ->
       Ok (make_token state (fun s -> Operator (RBracket, s)) "]" |> move_state rest)
     | '(' :: rest ->
       Ok (make_token state (fun s -> Operator (LParen, s)) "(" |> move_state rest)
     | ')' :: rest ->
       Ok (make_token state (fun s -> Operator (RParen, s)) ")" |> move_state rest)
     | '{' :: rest ->
       Ok (make_token state (fun s -> Operator (LBrace, s)) "{" |> move_state rest)
     | '}' :: rest ->
       Ok (make_token state (fun s -> Operator (RBrace, s)) "}" |> move_state rest)
     | ';' :: rest ->
       Ok (make_token state (fun s -> Operator (Semicolon, s)) ";" |> move_state rest)
     | '"' :: rest -> lex_string { state with input = rest }
     | '\'' :: _ -> lex_char state
     | '/' :: '/' :: _ -> lex_comment state []
     | c :: _ when Char.is_digit c -> lex_number state
     | c :: _ when check_ident_char c -> lex_ident state
     | c :: _ when is_op_char c -> lex_op state
     | _ :: rest -> Ok { state with input = rest }
     | [] -> Finished state)
    |> inner_lex
  | _ -> state_result
;;

let lex_raw input =
  inner_lex
    (Ok
       { input = input |> String.to_list
       ; tokens = []
       ; current_line = 1
       ; current_column = 1
       ; current_position = 0
       ; buffer = []
       })
  |> function
  | Finished x -> Result.Ok (List.rev x.tokens)
  | Err e -> Error e
  | Ok s -> Error (UnexpectedError s)
;;

let lex file_name = In_channel.read_all file_name |> lex_raw
