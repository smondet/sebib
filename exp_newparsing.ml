

module Sx = Sexplib.Sexp

exception Parse_error of string

(* Error when an Atom is found instead of a list. *)
let fail_atom s =
  let msg =
    sprintf "Parsing Error unxepected atom \"%s\" there should be a list\n" s in
  raise (Parse_error msg)    

let fail_empty s_opt =
  let msg =
    sprintf "Parsing Error: empty field %s" 
      (Opt.map_default (fun s ->
                          sprintf "\"%s\"" (Sx.to_string_hum s))
         "()" s_opt)
  in
  raise (Parse_error msg)    

let fail_bad_field field = 
  let msg =
    sprintf "Parsing Error ununderstandable data for field: \"%s\""
      (Str.concat " " (Ls.map (fun s -> Sx.to_string_hum s) field)) in
  raise (Parse_error msg)

let fail_bad_author sexp =
  let msg =
    sprintf "Parsing Error ununderstandable author: \"%s\""
      (Sx.to_string_hum sexp) in
  raise (Parse_error msg)

let fail_bad_atom_list kind sexp =
  let msg =
    sprintf "Parsing Error ununderstandable %s: \"%s\""
      kind (Sx.to_string_hum sexp) in
  raise (Parse_error msg)

let fail_no_id fields = 
  let msg =
    sprintf "Parsing Error entry has no ID: \"(%s)\""
      (Str.concat " " (Ls.map (fun s -> Sx.to_string_hum (Sx.List s)) fields))
  in
  raise (Parse_error msg)

let parse_authors sexp_list =
  Ls.map sexp_list 
    ~f:(function
        | Sx.List ((Sx.Atom first) :: (Sx.Atom last) :: []) -> (first, last)
        | Sx.Atom one_name -> ("", one_name)
        | sexp -> fail_bad_author sexp)

let parse_atom_list kind sexp_list =
  Ls.map sexp_list 
    ~f:(function
        | Sx.Atom name -> name
        | sexp -> fail_bad_atom_list kind sexp)

let parse_entry field_sexp_list =
  let its_id = ref None in
  let its_fields =
    Ls.map field_sexp_list
      ~f:(function
          | [] -> fail_empty None
          | one :: [] -> fail_empty (Some one)
          | (Sx.Atom h1) :: (Sx.Atom h2) :: [] as l ->
              begin match h1 with
              | "id" -> its_id := Some h2 ; (`id h2)
              | "title" -> (`title h2)
              | "how" -> (`how h2)
              | "year" -> (`year h2)
              | "url" -> (`url h2)
              | "pdfurl" -> (`pdfurl h2)
              | "bibtex" -> (`bibtex h2)
              | "note" -> (`note h2)
              | "abstract" -> (`abstract h2)
              | "doi" -> (`doi h2)
              | "citation" -> (`citation h2)
              | "comment" -> (`comment ("main", h2))
              | "authors" | "author"  -> (`authors [ ("", h2) ])
                  (* one single-named author *)
              | s -> fail_bad_field l
              end
          | (Sx.Atom "comment") :: (Sx.Atom h2) :: (Sx.Atom h3) :: [] ->
              (`comment (h2, h3))
          | (Sx.Atom h) :: t as l ->
              begin match h with
              | "authors" | "author" -> (`authors (parse_authors t))
              | "tags" -> (`tags (parse_atom_list "tag" t))
              | "keywords" -> (`keywords (parse_atom_list "keyword" t))
              | s -> fail_bad_field l
              end
          | l -> fail_bad_field l)
  in
  match !its_id with
  | None -> fail_no_id field_sexp_list
  | Some id -> (id, its_fields)

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
                | Sx.List l ->  l) in
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
    (comment \"main one\")
    (tags one_tag another_tag)
    (keywords \"A Key Word\" Sebib)
)     
";;
let valid2 = (valid1 ^ "\n\n( (id booo) (comment short \"one sentence\") )");;
let valid3 = (valid2 ^ "((id truc)) ;skdj\n   \t   \n  \r \n");;
let valid4 = "( (id theID) (author \"Aleph One\") )";;

let invalids = [
  "( (bouh )";
  "( () )  )  fd";
  (valid3 ^ ")" ^ valid3);
  " TheAtom ";
  "(TheAtom)";
  "((TheAtom \"))";
  "((TheAtom ;\"-\"))";
  " ( (authors no_id) )";
  " ( (id id_but) (tags tag1 (bad tag) ) )";
  " ( (id id_but) (tags tag1 (bad tag) ) )";
];;


let test f arg  =
  printf "\n========\n%s\n" arg;
  try (f arg)   (*: Sebib.Biblio.set*)
  with
  | Parse_error msg -> printf "Error: %s\n" msg; []
  | e -> printf "Exn: %s\n" (Printexc.to_string e) ; [];;

printf "\n\n====================================================\n\n";;
test parse valid1;;
test parse valid2;;
test parse valid3;;
test parse valid4;;

Ls.map invalids ~f:(fun inval -> test parse inval);;

(*  inval1;; *)
(* test parse inval2;; *)
(* test parse inval3;; *)
(* test parse inval4;; *)
(* test parse inval5;; *)
(* test parse inval6;; *)
(* test parse inval7;; *)
(* test parse inval8;; *)
(* test parse inval9;; *)

