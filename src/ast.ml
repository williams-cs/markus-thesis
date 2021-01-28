open Angstrom_extended
open Core

(* Set to true to turn on remote extensions *)
let remote_extensions = true

type t = (and_or_list * separator) list

and token_part =
  | Literal of string
  | Variable of string
  | Command_substitution of t

and token = token_part list

and io_file_op =
  (* < *)
  | Less
  (* <& *)
  | Lessand
  (* > *)
  | Great
  (* >& *)
  | Greatand
  (* >> *)
  | Dgreat
  (* <> *)
  | Lessgreat
  (* >| *)
  | Clobber

and io_here_op =
  (* << *)
  | Dless
  (* <<- *)
  | Dlessdash

and io_redirect_part =
  | Io_file of io_file_op * token
  | Io_here of io_here_op * string

and io_redirect = int option * io_redirect_part

and assignment = string * token

and simple_command = token list * assignment list * io_redirect list

and case_item = token list * t option

and remote_command =
  | Remote_subshell of t
  | Remote_name of string

and command =
  | Simple_command of simple_command
  | If_clause of (t * t) list * t option
  (* | While_clause of t * t
  | Until_clause of t * t
  | Brace_group of t
  | For_clause of string * token list * t
  | Case_clause of token * case_item list *)
  | Subshell of t
  (* Remote extension *)
  | Remote_command of remote_command * string

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

let is_special ~eq_special = function
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
  (* | '['
  | ']' *)
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
  (* | '?' *)
  | '~'
  | '&' -> true
  | '=' -> eq_special
  | '@' -> remote_extensions
  | _ -> false
;;

let reserved_words =
  [ "!"
  ; "{"
  ; "}"
  ; "case"
  ; "do"
  ; "done"
  ; "elif"
  ; "else"
  ; "esac"
  ; "fi"
  ; "for"
  ; "if"
  ; "in"
  ; "then"
  ; "until"
  ; "while"
  ]
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

module Subshell_state = struct
  type t =
    { within_backtick : int
    ; allow_empty : bool
    }
  [@@deriving fields]
end

let ast : t Angstrom_extended.t =
  (* Grammar from: https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_10 *)
  (* Definitions starting with the prefix "g_" are part of the predefined grammar*)
  (* let g_compound_list = *)
  fix_state_2
    ~init:{ within_backtick = 0; allow_empty = true }
    (fun inner subshell_state ->
      let comment = char '#' *> skip_while (fun x -> not (is_newline x)) in
      let delimiter = skip_while is_whitespace_within_line <* option () comment in
      let token s = string s <* delimiter in
      let optional_token s = option false (token s >>| fun _s -> true) in
      let g_newline = token "\r" <|> token "\n" in
      let g_newline_list = many1 g_newline *> return None in
      let g_linebreak = many g_newline in
      let g_separator_op =
        token "&" *> return Ampersand <|> token ";" *> return Semicolon
      in
      let g_separator = g_separator_op >>| some <* g_linebreak <|> g_newline_list in
      let _g_sequential_sep = token ";" >>| some <|> g_newline_list in
      (* Quoting *)
      let chars_to_literal cl = cl |> String.of_char_list |> fun l -> Literal l in
      let maybe_chars_to_literal mcl = mcl |> List.filter_opt |> chars_to_literal in
      let non_special_character ~eq_special =
        satisfy (fun x -> not (is_special ~eq_special x))
      in
      let character_out_of_quotes ~within_backtick ~eq_special =
        non_special_character ~eq_special
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
        let within_backtick = Subshell_state.within_backtick subshell_state in
        char '$' *> many1 (character_out_of_quotes ~within_backtick ~eq_special:true)
        >>| fun mcl ->
        mcl |> List.filter_opt |> String.of_char_list |> fun v -> Variable v
      in
      let backtick_command_substitution =
        match Subshell_state.within_backtick subshell_state with
        | 1 ->
          char '\\'
          *> char '`'
          *> (inner { within_backtick = 2; allow_empty = true }
             >>| fun ss -> Command_substitution ss)
          <* char '\\'
          <* char '`'
        | 0 ->
          char '`'
          *> (inner { within_backtick = 1; allow_empty = true }
             >>| fun ss -> Command_substitution ss)
          <* char '`'
        | _ -> fail "Cannot have more than two nested backtick escapes!"
      in
      let dollar_command_substitution =
        string "$("
        *> delimiter
        *> (inner { subshell_state with allow_empty = true }
           >>| fun ss -> Command_substitution ss)
        <* delimiter
        <* string ")"
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
      let token_part_unquoted ~reserved_special ~eq_special =
        let reserved = if reserved_special then reserved_words else [] in
        choice (List.map reserved ~f:(fun word -> string word))
        <!|>
        let within_backtick = Subshell_state.within_backtick subshell_state in
        variable
        <|> command_substitution
        <|> (many1 (character_out_of_quotes ~within_backtick ~eq_special)
            >>| maybe_chars_to_literal)
      in
      let single_quoted_str =
        single_quote *> many token_part_in_single_quotes <* single_quote
      in
      let double_quoted_str =
        double_quote *> many token_part_in_double_quotes <* double_quote
      in
      let unquoted_string ~reserved_special ~eq_special =
        both
          (token_part_unquoted ~reserved_special ~eq_special)
          (many (token_part_unquoted ~reserved_special:false ~eq_special))
        >>| fun (x, xs) -> x :: xs
      in
      let name = many1 (non_special_character ~eq_special:true) >>| String.of_char_list in
      let word ~reserved_special ~eq_special =
        both
          (unquoted_string ~reserved_special ~eq_special
          <|> single_quoted_str
          <|> double_quoted_str)
          (many
             (unquoted_string ~reserved_special:false ~eq_special
             <|> single_quoted_str
             <|> double_quoted_str))
        >>| fun (x, xs) -> x :: xs |> List.concat
      in
      let delimited_word ~reserved_special =
        delimiter *> word ~reserved_special ~eq_special:false <* delimiter
      in
      let assignment =
        both (name <* string "=") (word ~reserved_special:false ~eq_special:true)
      in
      let assignment_word = delimiter *> assignment <* delimiter in
      let g_filename = unquoted_string ~reserved_special:false ~eq_special:false in
      let io_less = token "<" *> return Less in
      let io_lessand = token "<&" *> return Lessand in
      let io_great = token ">" *> return Great in
      let io_greatand = token ">&" *> return Greatand in
      let io_dgreat = token ">>" *> return Dgreat in
      let io_lessgreat = token "<>" *> return Lessgreat in
      let io_clobber = token ">|" *> return Clobber in
      let g_io_file =
        both
          (io_less
          <|> io_lessand
          <|> io_great
          <|> io_greatand
          <|> io_dgreat
          <|> io_lessgreat
          <|> io_clobber)
          g_filename
        >>| fun (file_op, filename) -> Io_file (file_op, filename)
      in
      let io_dless = token "<<" *> return Dless in
      let io_dlessdash = token "<<-" *> return Dlessdash in
      (* TODO *)
      let g_here_end = fail "TODO here_end" in
      let g_io_here =
        both (io_dless <|> io_dlessdash) g_here_end
        >>| fun (here_op, here_end) -> Io_here (here_op, here_end)
      in
      let integer =
        take_while1 (function
            | '0' .. '9' -> true
            | _ -> false)
        >>| Int.of_string
        >>| some
      in
      let g_io_number = integer in
      let g_io_redirect = both (option None g_io_number) (g_io_file <|> g_io_here) in
      let cmd_prefix_part =
        g_io_redirect
        >>| (fun r -> `Redirect r)
        <|> (assignment_word >>| fun w -> `Assignment w)
      in
      let g_cmd_prefix_1 = many1 cmd_prefix_part in
      let g_cmd_suffix_1 ~reserved_special =
        both
          (g_io_redirect
          >>| (fun r -> `Redirect r)
          <|> (delimited_word ~reserved_special >>| fun w -> `Token w))
          (many
             (g_io_redirect
             >>| (fun r -> `Redirect r)
             <|> (delimited_word ~reserved_special:false >>| fun w -> `Token w)))
        >>| fun (x, xs) -> x :: xs
      in
      let g_simple_command : command Angstrom_extended.t =
        let merge_fold (tokens, assignments, io_redirects) = function
          | `Token t -> t :: tokens, assignments, io_redirects
          | `Assignment a -> tokens, a :: assignments, io_redirects
          | `Redirect r -> tokens, assignments, r :: io_redirects
        in
        let merge_simple_command (l1, l2) =
          let trev, arev, rrev = List.fold ~init:([], [], []) ~f:merge_fold (l1 @ l2) in
          Simple_command (List.rev trev, List.rev arev, List.rev rrev)
        in
        delimiter
        *> (both g_cmd_prefix_1 (g_cmd_suffix_1 ~reserved_special:false)
           <|> both g_cmd_prefix_1 (return [])
           <|> both (return []) (g_cmd_suffix_1 ~reserved_special:true))
        >>| merge_simple_command
      in
      let and_or_if = token "||" *> return Or <|> token "&&" *> return And in
      let subshell =
        token "(" *> inner { subshell_state with allow_empty = false } <* token ")"
      in
      let g_subshell : command Angstrom_extended.t = subshell >>| fun x -> Subshell x in
      let text_token str = token str in
      let g_else_part =
        both
          (many
             (both
                (text_token "elif" *> inner subshell_state)
                (text_token "then" *> inner subshell_state)))
          (option None (text_token "else" *> inner subshell_state >>| fun x -> Some x))
      in
      let g_if_clause =
        both
          (both
             (text_token "if" *> inner subshell_state)
             (text_token "then" *> inner subshell_state))
          g_else_part
        <* text_token "fi"
        >>| fun (if_block, (elif_blocks, maybe_else)) ->
        If_clause (if_block :: elif_blocks, maybe_else)
      in
      let g_compound_command = g_subshell <|> g_if_clause in
      let re_subshell = subshell >>| fun x -> Remote_subshell x in
      let re_name = name >>| fun x -> Remote_name x in
      let re_remote_command =
        if remote_extensions
        then
          both (re_subshell <|> re_name <* delimiter <* token "@@") name
          <* delimiter
          >>| fun (x, n) -> Remote_command (x, n)
        else fail "Remote extensions required"
      in
      let g_command = re_remote_command <|> g_simple_command <|> g_compound_command in
      let g_pipe_sequence : command list Angstrom_extended.t =
        both g_command (many (token "|" *> g_linebreak *> g_command))
        >>| fun (c, cl) -> c :: cl
      in
      let g_pipeline : pipeline Angstrom_extended.t =
        both
          (optional_token "!" >>| fun x -> if x then Some Bang else None)
          g_pipe_sequence
      in
      let g_and_or : and_or_list Angstrom_extended.t =
        both g_pipeline (many (both (and_or_if <* g_linebreak) g_pipeline))
      in
      let g_term =
        both
          g_and_or
          (many
             (both
                (g_separator
                >>| fun maybe_sep -> Option.value ~default:Semicolon maybe_sep)
                g_and_or))
      in
      let g_compound_list =
        many g_newline_list *> both g_term (option None g_separator)
        >>| fun ((first, later), maybe_sep) ->
        let sep = maybe_sep |> Option.value ~default:Semicolon in
        unshift first later sep
      in
      let g_list = both g_and_or (many (both g_separator_op g_and_or)) in
      let g_complete_command =
        both g_list (option None g_separator)
        >>| fun ((first, later), maybe_sep) ->
        let sep = maybe_sep |> Option.value ~default:Semicolon in
        unshift first later sep
      in
      let empty = delimiter *> return [] in
      ( (if Subshell_state.allow_empty subshell_state
        then g_compound_list <|> empty
        else g_compound_list)
      , g_complete_command <|> empty ))
;;

let parse_partial ?state text =
  let text =
    match text with
    | "" | "\n" -> `Eof
    | s -> `String s
  in
  match state with
  | None ->
    let state = Buffered.parse ast in
    Buffered.feed state text
  | Some state -> Buffered.feed state text
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
