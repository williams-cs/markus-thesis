open Angstrom
open Core

type t =
  | Noop
  | Command of string * string list
  | Series of t * t

let is_whitespace_within_line = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\x0b' | '\x0c' | '\r' -> true
  | _ -> false
;;

let is_special = function
  (* Backslash *)
  | '\\'
  (* Quote *)
  | '\"'
  | '\''
  (* Whitespace *)
  | ' '
  | '\t'
  | '\n'
  | '\x0b'
  | '\x0c'
  | '\r'
  (* Shell Special Characters *)
  | '$'
  | '#'
  | '='
  | '['
  | ']'
  | '!'
  | '>'
  | '<'
  | '|'
  | ';'
  | '{'
  | '}'
  | '('
  | ')'
  | '*'
  | '?'
  | '~'
  | '&' -> true
  | _ -> false
;;

let single_quote = char '\''
let double_quote = char '\"'
let whitespace_within_line = skip_while is_whitespace_within_line
let _whitespace = skip_while is_whitespace

let character_in_single_quotes =
  satisfy (function
      | '\\' | '\'' -> false
      | _ -> true)
  <|> char '\\' *> any_char
;;

let character_in_double_quotes =
  satisfy (function
      | '\\' | '\"' -> false
      | _ -> true)
  <|> char '\\' *> any_char
;;

let character_out_of_quotes =
  satisfy (fun x -> not (is_special x)) <|> char '\\' *> any_char
;;

let single_quoted_str = single_quote *> many character_in_single_quotes <* single_quote
let double_quoted_str = double_quote *> many character_in_double_quotes <* double_quote
let unquoted_string = many1 character_out_of_quotes

let str =
  map
    (many1 (unquoted_string <|> single_quoted_str <|> double_quoted_str))
    ~f:(fun cll -> cll |> List.map ~f:String.of_char_list |> String.concat)
;;

let token = whitespace_within_line *> str <* whitespace_within_line
let command = map (both token (many token)) ~f:(fun (name, args) -> Command (name, args))

let parse text : t Or_error.t =
  match parse_string ~consume:All command text with
  | Ok ast -> Ok ast
  | Error err -> Error (Error.of_string err)
;;
