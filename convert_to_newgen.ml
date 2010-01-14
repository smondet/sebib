TYPE_CONV_PATH "Sebib"

(* 


ocamlfind ocamlc -package sexplib.syntax,pcre,extlib -linkpkg -syntax camlp4o -I _build/lib/ sebib.cma convert_to_newgen.ml -o sebib-leg2ng

 *)


open Sebib_std

module Info = struct
  let version = "0"
  let version_string = sprintf "SeBib v. %s" version

end

module AuthorList = struct
  type author = string * string with sexp
  type t = author list with sexp

  type style = [ `comas_and | `acm | `bibtex | `comas | `et_al ] with sexp

  let to_string ?(style:style=`comas) authors = (
    match style with
    | `bibtex ->
        String.concat " and "
          (Ls.map (fun (first, last) ->
                     sprintf "%s, %s" last first) authors)
    | `comas_and ->
        let lgth = Ls.length authors in
        String.concat ", "
          (Ls.mapi (fun i (first, last) ->
                      let strand =
                        if i = lgth - 1 && i <> 0 then "and " else "" in
                      sprintf "%s%s %s" strand first last) authors)
    | `comas -> 
        String.concat ", "
          (Ls.map (fun (first, last) ->
                     sprintf "%s %s" first last) authors)
    | `acm -> 
        let lgth = Ls.length authors in
        String.concat ", "
          (Ls.mapi (fun i (first, last) ->
                      let strand =
                        if i = lgth - 1 && i <> 0 then "and " else "" in
                      let initial =
                        if String.length first = 0 
                        then '_' else first.[0] in
                      sprintf "%s%s, %c." strand last initial) authors)
    | `et_al ->
        match authors with
        | [] -> ""
        | [(onef,onel)] -> onel
        | [(onef,onel); (twof,twol) ] -> sprintf "%s and %s" onel twol
        | (onef,onel) :: l -> onel ^ "et al."

  )
end

module Biblio = struct

  type field = [
  | `id of string
  | `authors of AuthorList.t
  | `title of string
  | `how of string
  | `date of string
  | `year of int
  | `url of string
  | `pdfurl of string
  | `comments of string
  | `bibtex of string
  | `note of string
  | `abstract of string
  | `doi of string
  | `citation of string
  | `tags of string list
  | `keywords of string list
  | `more of string
  ]
  with sexp

  type field_name = [
  | `id 
  | `authors 
  | `title 
  | `how 
  | `date 
  | `year 
  | `url 
  | `pdfurl 
  | `comments 
  | `bibtex 
  | `note 
  | `abstract 
  | `doi 
  | `citation 
  | `tags 
  | `keywords 
  | `more
  ] with sexp
  type entry = field list with sexp

  type set = entry list with sexp


  let is_valid set   = (
    let invalids =
      Ls.find_all
        (fun entry ->
           not (Ls.exists
                  (function `id _ -> true | _ -> false) entry))
        set in
    match invalids with
    | [] -> `yes
    | l -> `no l
  )

  let is_bibtexable set = (
    let has_bibtex =
      Ls.exists ~f:(function `bibtex _ -> true | _ -> false) in
    let is_miscable e =
      (Ls.exists (function `id _ -> true | _ -> false) e) &&
        (Ls.exists (function `title _ -> true | _ -> false) e) &&
        (* (Ls.exists (function `authors _ -> true | _ -> false) e) && *)
        (Ls.exists (function `how _ -> true | _ -> false) e)
        (* (Ls.exists (function `year _ -> true | _ -> false) e) *)
    in
    let invalids =
      Ls.find_all
        (fun entry -> not ((has_bibtex entry) || (is_miscable entry)))
        set in
    match invalids with
    | [] -> `yes
    | l -> `no l
  )

  let set_of_string str = 
    set_of_sexp (Sexplib.Sexp.of_string ("(" ^ str ^ ")"))

  let string_of_set set  =
    let s = sexp_of_set set in 
    Sexplib.Sexp.to_string s

  let find_field (field:field_name) (entry:entry) = (
    let f  = Ls.find_opt in
    match field with
    | `id        -> (f (function `id       v -> true | _ -> false) entry)
    | `authors   -> (f (function `authors  v -> true | _ -> false) entry)
    | `title     -> (f (function `title    v -> true | _ -> false) entry)
    | `how       -> (f (function `how      v -> true | _ -> false) entry)
    | `date      -> (f (function `date     v -> true | _ -> false) entry)
    | `year      -> (f (function `year     v -> true | _ -> false) entry)
    | `url       -> (f (function `url      v -> true | _ -> false) entry)
    | `pdfurl    -> (f (function `pdfurl   v -> true | _ -> false) entry)
    | `comments  -> (f (function `comments v -> true | _ -> false) entry)
    | `bibtex    -> (f (function `bibtex   v -> true | _ -> false) entry)
    | `note      -> (f (function `note     v -> true | _ -> false) entry)
    | `abstract  -> (f (function `abstract v -> true | _ -> false) entry)
    | `doi       -> (f (function `doi      v -> true | _ -> false) entry)
    | `citation  -> (f (function `citation v -> true | _ -> false) entry)
    | `tags      -> (f (function `tags     v -> true | _ -> false) entry)
    | `keywords  -> (f (function `keywords v -> true | _ -> false) entry)
    | `more      -> (f (function `more     v -> true | _ -> false) entry)
  )

    
  let field_or_empty ?(authors_style=`comas) (fi:field_name) entry = 
    match find_field fi entry with
    | Some (`authors al) -> AuthorList.to_string ~style:authors_style al
    | Some (`title tit) -> tit
    | Some (`id id) -> id
    | Some (`how how) -> how
    | Some (`year y) -> string_of_int y
    | Some (`note n) -> n
    | Some (`date      s) -> s
    | Some (`url       s) -> s
    | Some (`pdfurl    s) -> s
    | Some (`comments  s) -> s
    | Some (`bibtex    s) -> s
    | Some (`abstract  s) -> s
    | Some (`doi       s) -> s
    | Some (`citation  s) -> s
    | Some (`tags      l) -> String.concat ", " l
    | Some (`keywords  l) -> String.concat ", " l
    | Some (`more      s) -> s
    | _ -> ""

  let field_name_of_string = function
    | "id" -> `id 
    | "authors" -> `authors 
    | "title" -> `title 
    | "how" -> `how 
    | "date" -> `date 
    | "year" -> `year 
    | "url" -> `url 
    | "pdfurl" -> `pdfurl 
    | "comments" -> `comments 
    | "bibtex" -> `bibtex 
    | "note" -> `note 
    | "abstract" -> `abstract 
    | "doi" -> `doi 
    | "citation" -> `citation 
    | "tags" -> `tags 
    | "keywords" -> `keywords 
    | "more" -> `more
    | s -> failwith ("field name unrecognizable: " ^ s)

  let sort ?(by=`id) bibset = 
    let cmp ea eb =
      let authors_style = `acm in
      String.compare
        (field_or_empty ~authors_style by ea)
        (field_or_empty ~authors_style by eb) in
    Ls.sort ~cmp bibset

end
let field_name = function
  | `id ->         "id"
  | `authors ->    "authors"
  | `title ->      "title"
  | `how ->        "how" 
  | `date ->       "date"
  | `year ->       "year"
  | `url ->        "url" 
  | `pdfurl ->     "pdfurl"
  | `comments ->   "comments"
  | `bibtex ->     "bibtex" 
  | `note ->       "note" 
  | `abstract ->   "abstract"
  | `doi ->        "doi" 
  | `citation ->   "citation"
  | `tags ->       "tags" 
  | `keywords ->   "keywords"
  | `more->        "more"

let rex = Pcre.regexp "^[A-Za-z0-9\\?\\!\\-\\:\\/\\.\\,\\~]+$"
let out_str s =
  let escape = 
    Str.replace_chars (function '\\' -> "\\\\" 
                         | '"' -> "\\\"" | c -> Str.of_char c) in
  if Pcre.pmatch ~rex s then s else sprintf "\"%s\"" (escape s)

let out_field ?change_name entry field =
  match Biblio.field_or_empty field entry with
  | "" -> ()
  | str ->
      let str_field = 
        Opt.default (field_name field) change_name in
      printf "    (%s %s)\n" str_field (out_str str)

let out_authors entry = 
  match Biblio.find_field `authors entry with
  | Some (`authors l) ->
      let f (first, last) =
        sprintf "        (%s %s)" (out_str first) (out_str last) in
      printf "    (authors \n%s)\n" (Str.concat "\n" (Ls.map l ~f))
  | _ -> ()

let out_list ?add_tag entry field =
  match Biblio.find_field field entry with
  | Some (`tags l) ->
      let ll =
        Opt.map_default (fun c -> l @ [c]) l add_tag in
      printf "    (tags %s)" (Str.concat " " (Ls.map ll ~f:out_str))
  | Some (`keywords l) ->
      printf "    (keywords %s)\n" (Str.concat " " (Ls.map l ~f:out_str))
  | _ -> ()

let find_by_id set id =
  Ls.find set ~f:(fun e -> id =$= (Biblio.field_or_empty `id e))

let () =
  (* eprintf "Old to new Sebib\n"; *)

  let file_in = Sys.argv.(1) in
  let file_support = Sys.argv.(2) in
  (* let file_out = Sys.argv.(3) in *)

  let b =
    Biblio.set_of_string (Io.read_all (Io.open_in file_in)) in
  let b_sup = 
    Biblio.set_of_string (Io.read_all (Io.open_in file_support)) in
  
  Ls.iter b
    ~f:(fun e ->
          let id = (Biblio.field_or_empty `id e) in
          printf "(   (id %s)\n" id
          ;out_field e  `title 
          ;out_authors e
          ;out_field e  `how 
          ;out_field e  `year 
          ;out_field e  `note 
          ;out_field e  `url 
          ;out_field e  `doi 
          ;out_field e  `pdfurl 
          ;out_field e  `bibtex 
          ;out_field e  `abstract 
          ;out_field e  `citation 
          ;out_list e  `keywords
          ;out_field e ~change_name:"comment"  `comments 
          ;          
          begin try
            let e_sup = find_by_id b_sup id in
            out_field ~change_name:"comment short" e_sup `more;
            out_list e ~add_tag:"difidam" `tags;
          with Not_found -> 
            out_list e `tags;
          end;
          printf ")\n";
       );
  ()
    
