open Angstrom
open Core

type t =
  | Noop
  | Command of token * token list
  | Series of t * t

and token_part =
  | Literal of string
  | Variable of string
  | Subshell of t

and token = token_part list

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
      | '\\' | '$' | '\'' -> false
      | _ -> true)
  <|> char '\\' *> any_char
;;

let character_in_double_quotes =
  satisfy (function
      | '\\' | '$' | '\"' -> false
      | _ -> true)
  <|> char '\\' *> any_char
;;

let ast =
  fix (fun inner ->
      let chars_to_literal cl = cl |> String.of_char_list |> fun l -> Literal l in
      let character_out_of_quotes =
        satisfy (fun x -> not (is_special x)) <|> char '\\' *> any_char
      in
      let variable =
        char '$' *> many1 character_out_of_quotes
        >>| fun cl -> cl |> String.of_char_list |> fun v -> Variable v
      in
      let subshell =
        char '$' *> char '(' *> (inner >>| fun ss -> Subshell ss) <* char ')'
      in
      let token_part_in_single_quotes =
        variable <|> subshell <|> (many1 character_in_single_quotes >>| chars_to_literal)
      in
      let token_part_in_double_quotes =
        variable <|> subshell <|> (many1 character_in_double_quotes >>| chars_to_literal)
      in
      let token_part_unquoted =
        variable <|> subshell <|> (many1 character_out_of_quotes >>| chars_to_literal)
      in
      let single_quoted_str =
        single_quote *> many token_part_in_single_quotes <* single_quote
      in
      let double_quoted_str =
        double_quote *> many token_part_in_double_quotes <* double_quote
      in
      let unquoted_string = many1 token_part_unquoted in
      let token =
        many1 (unquoted_string <|> single_quoted_str <|> double_quoted_str)
        >>| List.concat
      in
      let token_with_whitespace =
        whitespace_within_line *> token <* whitespace_within_line
      in
      let command =
        map
          (both token_with_whitespace (many token_with_whitespace))
          ~f:(fun (name, args) -> Command (name, args))
      in
      let noop = whitespace_within_line *> return Noop in
      command <|> noop)
;;

let parse text : t Or_error.t =
  match parse_string ~consume:All ast text with
  | Ok ast -> Ok ast
  | Error err -> Error (Error.of_string err)
;;
