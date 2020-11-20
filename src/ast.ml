open Angstrom_extended
open Core

type t =
  | Command of token list
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

let is_newline = function
  | '\n' | '\r' -> true
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
  | '`'
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
let _whitespace = skip_while is_whitespace

let character_in_single_quotes =
  satisfy (function
      | '\'' -> false
      | _ -> true)
;;

let character_in_double_quotes =
  char '\\'
  *> satisfy (function
         | '\\' | '$' | '\"' | '`' | '\n' -> true
         | _ -> false)
  <|> satisfy (function
          | '$' | '\"' -> false
          | _ -> true)
;;

let field_split =
  let word =
    many1 (satisfy (fun x -> (not (is_whitespace_within_line x)) && not (is_newline x)))
    >>| String.of_char_list
  in
  let delimiter = skip_while (fun x -> is_whitespace_within_line x || is_newline x) in
  let delimited_word = word <* delimiter in
  delimiter *> many delimited_word
;;

let ast =
  (* Grammar from: https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_10 *)
  fix_state ~init:0 (fun inner within_backtick ->
      let some x = Some x in
      let newline = char '\r' <|> char '\n' in
      let newline_list = many1 newline *> return None in
      let linebreak = many newline in
      let separator_op = char '&' <|> char ';' in
      let _separator = separator_op >>| some <* linebreak <|> newline_list in
      let _sequential_sep = char ';' >>| some <|> newline_list in
      (* Quoting *)
      let chars_to_literal cl = cl |> String.of_char_list |> fun l -> Literal l in
      let maybe_chars_to_literal mcl = mcl |> List.filter_opt |> chars_to_literal in
      let character_out_of_quotes ~within_backtick =
        satisfy (fun x -> not (is_special x))
        >>| some
        <|> char '\\' *> newline *> return None
        <|> char '\\'
            *> (satisfy (fun x ->
                    match within_backtick with
                    | 0 -> true
                    | _ -> not (Char.equal x '`'))
               >>| some)
      in
      let variable =
        char '$' *> many1 (character_out_of_quotes ~within_backtick)
        >>| fun mcl ->
        mcl |> List.filter_opt |> String.of_char_list |> fun v -> Variable v
      in
      let backtick_command_substitution =
        match within_backtick with
        | 1 ->
          char '\\' *> char '`' *> (inner 2 >>| fun ss -> Subshell ss)
          <* char '\\'
          <* char '`'
        | 0 -> char '`' *> (inner 1 >>| fun ss -> Subshell ss) <* char '`'
        | _ -> fail "Cannot have more than two nested backtick escapes!"
      in
      let dollar_command_substitution =
        char '$' *> char '(' *> (inner within_backtick >>| fun ss -> Subshell ss)
        <* char ')'
      in
      let command_substitution =
        backtick_command_substitution <|> dollar_command_substitution
      in
      let token_part_in_single_quotes =
        many1 character_in_single_quotes >>| chars_to_literal
      in
      let token_part_in_double_quotes =
        variable
        <|> command_substitution
        <|> (many1 character_in_double_quotes >>| chars_to_literal)
      in
      let token_part_unquoted =
        variable
        <|> command_substitution
        <|> (many1 (character_out_of_quotes ~within_backtick) >>| maybe_chars_to_literal)
      in
      let single_quoted_str =
        single_quote *> many token_part_in_single_quotes <* single_quote
      in
      let double_quoted_str =
        double_quote *> many token_part_in_double_quotes <* double_quote
      in
      let unquoted_string = many1 token_part_unquoted in
      let word =
        many1 (unquoted_string <|> single_quoted_str <|> double_quoted_str)
        >>| List.concat
      in
      let comment = char '#' *> skip_while (fun x -> not (is_newline x)) in
      let delimiter = skip_while is_whitespace_within_line <* option () comment in
      let delimited_word = word <* delimiter in
      let command = delimiter *> many delimited_word >>| fun args -> Command args in
      command)
;;

let parse text : t Or_error.t =
  match parse_string ~consume:All ast text with
  | Ok ast -> Ok ast
  | Error err -> Error (Error.of_string err)
;;

let parse_field_split text =
  match parse_string ~consume:All field_split text with
  | Ok fields -> fields
  | Error err -> Error.raise (Error.of_string err)
;;
