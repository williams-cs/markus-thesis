open Angstrom_extended
open Core

type t = (and_or_list * separator) list

and token_part =
  | Literal of string
  | Variable of string
  | Command_substitution of t

and token = token_part list

and simple_command = token list

and case_item = string list * t

and command =
  | Simple_command of simple_command
  | Subshell of t

(* | For_clause of string * string list * t
| Case_clause of string * case_item list
| If_clause of (t * t) list * t option
| While_clause of t * t
| Until_clause of t * t *)
and pipeline = bang option * command list

and and_or_list = pipeline * (and_or * pipeline) list

and bang = Bang

and and_or =
  | And
  | Or

and separator =
  | Ampersand
  | Semicolon
[@@deriving sexp]

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

let some x = Some x

let unshift a l b =
  let a_end, l_end =
    List.fold l ~init:(a, []) ~f:(fun (a_accum, l_accum) (bx, ax) ->
        ax, (a_accum, bx) :: l_accum)
  in
  List.rev ((a_end, b) :: l_end)
;;

let ast : t Angstrom_extended.t =
  (* Grammar from: https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_10 *)
  (* Definitions starting with the prefix "g_" are part of the predefined grammar*)
  fix_state ~init:0 (fun inner within_backtick ->
      let g_newline = char '\r' <|> char '\n' in
      let g_newline_list = many1 g_newline *> return None in
      let g_linebreak = many g_newline in
      let g_separator_op =
        char '&' *> return Ampersand <|> char ';' *> return Semicolon
      in
      let g_separator = g_separator_op >>| some <* g_linebreak <|> g_newline_list in
      let _g_sequential_sep = char ';' >>| some <|> g_newline_list in
      (* Quoting *)
      let chars_to_literal cl = cl |> String.of_char_list |> fun l -> Literal l in
      let maybe_chars_to_literal mcl = mcl |> List.filter_opt |> chars_to_literal in
      let character_out_of_quotes ~within_backtick =
        satisfy (fun x -> not (is_special x))
        >>| some
        <|> char '\\' *> g_newline *> return None
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
          char '\\' *> char '`' *> (inner 2 >>| fun ss -> Command_substitution ss)
          <* char '\\'
          <* char '`'
        | 0 -> char '`' *> (inner 1 >>| fun ss -> Command_substitution ss) <* char '`'
        | _ -> fail "Cannot have more than two nested backtick escapes!"
      in
      let dollar_command_substitution =
        char '$'
        *> char '('
        *> (inner within_backtick >>| fun ss -> Command_substitution ss)
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
      let g_simple_command : command Angstrom_extended.t =
        delimiter *> many1 delimited_word >>| fun x -> Simple_command x
      in
      let token s = string s <* delimiter in
      let optional_token s = option false (token s >>| fun _s -> true) in
      let and_or_if = token "||" *> return Or <|> token "&&" *> return And in
      let g_and_or : and_or_list Angstrom_extended.t =
        fix (fun inner_and_or ->
            let g_term =
              both
                inner_and_or
                (many
                   (both
                      (g_separator
                      >>| fun maybe_sep -> Option.value ~default:Semicolon maybe_sep)
                      inner_and_or))
            in
            let g_compound_list : command Angstrom_extended.t =
              many g_newline_list *> both g_term (option None g_separator)
              >>| (fun ((first, later), maybe_sep) ->
                    let sep = maybe_sep |> Option.value ~default:Semicolon in
                    unshift first later sep)
              >>| fun x -> Subshell x
            in
            let g_subshell : command Angstrom_extended.t =
              token "(" *> g_compound_list <* token ")"
            in
            let g_command = g_simple_command <|> g_subshell in
            let g_pipe_sequence : command list Angstrom_extended.t =
              both g_command (many (token "|" *> g_linebreak *> g_command))
              >>| fun (c, cl) -> c :: cl
            in
            let g_pipeline : pipeline Angstrom_extended.t =
              both
                (optional_token "!" >>| fun x -> if x then Some Bang else None)
                g_pipe_sequence
            in
            both g_pipeline (many (both (and_or_if <* g_linebreak) g_pipeline)))
      in
      let g_list = both g_and_or (many (both g_separator_op g_and_or)) in
      let g_complete_command =
        both g_list (option None g_separator)
        >>| fun ((first, later), maybe_sep) ->
        let sep = maybe_sep |> Option.value ~default:Semicolon in
        unshift first later sep
      in
      g_complete_command <|> delimiter *> return [])
;;

let parse text : t Or_error.t =
  match parse_string ~consume:All ast text with
  | Ok ast ->
    (* Print AST for debugging: *)
    (* sexp_of_t ast |> Sexp.to_string_hum |> print_endline; *)
    Ok ast
  | Error err -> Error (Error.of_string err)
;;

let parse_field_split text =
  match parse_string ~consume:All field_split text with
  | Ok fields -> fields
  | Error err -> Error.raise (Error.of_string err)
;;
