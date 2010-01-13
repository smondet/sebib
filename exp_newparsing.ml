

module Sx = Sexplib.Sexp

exception Parse_error of string

(* Error when an Atom is found instead of a list. *)
let fail_atom s =
  let msg =
    sprintf "Parsing Error unxepected atom \"%s\" there should be a list\n" s in
  raise (Parse_error msg)    

let parse_entry field_sexp_list =
  ("fake2010identifier", 
   (Ls.map field_sexp_list
      ~f:(fun sexp -> (Sx.to_string_hum sexp))))

let rec parse str =
  (* let sexps = Sx.load_sexps str in ---> only for file names *)
  let valid_and_parse_entry sexp =
    match sexp with
    | Sx.Atom s -> fail_atom s
    | Sx.List l ->
        let field_sexp_list =
          Ls.map l
            ~f:(function
                | Sx.Atom s -> fail_atom s
                | Sx.List l -> Sx.List l) in
        parse_entry field_sexp_list

  in

  let whitespace_regexp = 
    Pcre.regexp ~flags:[ `MULTILINE ] "[ \t\n\r]*" in
  let is_white s = Pcre.pmatch ~rex:whitespace_regexp s in

  let string_abstract pos max_len s =
    if lei max_len ((Str.length s) - pos) then
      (Str.sub s pos max_len) ^ "    [...]"
    else 
      (Str.sub s pos ((Str.length s) - pos))
  in

  let pos = ref 0 in
  let res = Ht.create 42 in
  let the_end = Str.length str  in
  while !pos < the_end do
    (* printf "pos: %d, the_end: %d\n" !pos the_end; *)
    try 
      match Sx.parse ~pos:!pos str with
      | Sx.Cont (state, _) ->
          if is_white (Str.sub str !pos (the_end - !pos)) && state then
            pos := the_end
          else 
            let msg =
              sprintf "Parsing Error (sexplib, unfinished s-expression?) \
                         for string: \"%s\""
                (string_abstract !pos 80 str) in
            raise (Parse_error msg)
      | Sx.Done (sx, parse_pos) ->
          pos := parse_pos.Sx.buf_pos;
          let id, entry = valid_and_parse_entry sx in
          Ht.add res id entry;
    with
      Sx.Parse_error pe ->
        let msg =
          sprintf "Parsing Error (sexplib): %s,  at \
                 character %d, i.e., %d-th of string: \"%s\""
            pe.Sx.err_msg pe.Sx.parse_state.Sx.parse_pos.Sx.buf_pos
            (pe.Sx.parse_state.Sx.parse_pos.Sx.text_char - 1)
            (string_abstract !pos 80 str)
        in
        raise (Parse_error msg)
  done;
  Ls.rev (Ht.value_list res)

(*
  (* This also works but, error messages are far from perfect: *)
  let sexp = 
    try Sx.of_string (sprintf "(%s)" str) 
    with Failure msg ->
      raise (ParseError (sprintf "Syntax Error (sexplib): %s" msg))
  in
  match sexp with
  | Sx.Atom s -> fail_atom s
  | Sx.List l ->
      Ls.map l
        ~f:(function
            | Sx.Atom s -> fail_atom s
            | Sx.List l -> parse_entry l)
*)

(* ========================================================================== *)

let valid1 = "
(
    (id utf8test)
    (authors (Jéţø Möûdæ) (Éşçölœ Ñéèßïù) )
    (title \"A Title with ACRONYM, And uppercase Ütf8, also Löwercæsê\")
    (how \"It is F$#@^!|£%kin \\£aT€X (©), which will nœvør bî üţf8 compliant\")
    (comment main \"bouh\")
)     
";;
let valid2 = (valid1 ^ "\n\n( (id booo) (comment \"main one\") )");;
let valid3 = (valid2 ^ "((id truc)) ;skdj\n   \t   \n  \r \n");;
let inval1 = "( (bouh )";;
let inval2 = "( () )  )  fd";;
let inval3 = (valid3 ^ inval1 ^ valid3);;
let inval4 = " TheAtom "
let inval5 = "(TheAtom)"
let inval6 = "((TheAtom \"))"
let inval7 = "((TheAtom ;\"-\"))"

let test f arg =
  printf "\n========\n%s\n" arg;
  try f arg
  with
  | Parse_error msg -> printf "Error: %s\n" msg; []
  | e -> printf "Exn: %s\n" (Printexc.to_string e) ; [];;

printf "\n\n====================================================\n\n";;
test parse valid1;;
test parse valid2;;
test parse valid3;;
test parse inval1;;
test parse inval2;;
test parse inval3;;
test parse inval4;;
test parse inval5;;
test parse inval6;;
test parse inval7;;

