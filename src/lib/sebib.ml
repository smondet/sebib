(******************************************************************************)
(*      Copyright (c) 2009, 2010, Sebastien MONDET                            *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)




(** The toplevel {i Sebib} module. *)



open Sebib_std

(** Information about the library (version). *)
module Info = struct

  (** The version number string. *)
  let version = "1.1.0"

  (** The version string with the name of the library. *)
  let version_string = sprintf "SeBib v. %s" version

  (** The license string. *)  
  let license = "\
Copyright (c) 2008, 2009, 2010, 2011 Sebastien MONDET\n\
\n\
Permission is hereby granted, free of charge, to any person        \n\
obtaining a copy of this software and associated documentation     \n\
files (the \"Software\"), to deal in the Software without            \n\
restriction, including without limitation the rights to use,       \n\
copy, modify, merge, publish, distribute, sublicense, and/or sell  \n\
copies of the Software, and to permit persons to whom the          \n\
Software is furnished to do so, subject to the following           \n\
conditions:                                                        \n\
\n\
The above copyright notice and this permission notice shall be     \n\
included in all copies or substantial portions of the Software.    \n\
\n\
THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,    \n\
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES    \n\
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND           \n\
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT        \n\
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,       \n\
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING       \n\
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR      \n\
OTHER DEALINGS IN THE SOFTWARE.                                    \n\
"

end

(** Module managing the lists of authors. *)
module AuthorList = struct

  (** The type of authors: [(first_name, family_name)] *)
  type author = string * string

  (** The type of the author lists. *)
  type t = author list

  (** The formatting style available. *)
  type style = [ `comas_and | `acm | `bibtex | `comas | `et_al ]

  (**/**)
  let initials first_name =
    if Str.length first_name = 0 then None else
      Some ((Str.concat ". "
               (Ls.map ~f:(fun s -> Str.sub s 0 1)
                  (Ls.filter ~f:(fun s -> Str.length s > 0)
                     (Str.nsplit first_name " ")))) ^ ".")
  (**/**)
          
  (** Convert a list of authors to a formatted string. *)
  let to_string ?(style:style=`comas) (authors:t) =
    match style with
    | `bibtex ->
        String.concat " and "
          (Ls.map (function
                   | ("", last) -> sprintf "{%s}" last
                   |(first, last) -> sprintf " {%s}, {%s}" last first)
             authors)
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
                      let initials_str =
                        match initials first with
                        | None -> ""
                        | Some s -> ", " ^ s in
                      sprintf "%s%s%s" strand last initials_str) authors)
    | `et_al ->
        match authors with
        | [] -> ""
        | [(onef,onel)] -> onel
        | [(onef,onel); (twof,twol) ] -> sprintf "%s and %s" onel twol
        | (onef,onel) :: l -> onel ^ " et al."

end

(** Module handling the whole bibliography. *)
module Biblio = struct

  (** The type of {i fields}. *)
  type field = [
  | `id of string
  | `authors of AuthorList.t
  | `title of string
  | `how of string
  | `year of string
  | `url of string
  | `pdfurl of string
  | `comment of string * string
  | `bibtex of string
  | `note of string
  | `abstract of string
  | `doi of string
  | `citation of string
  | `tags of string list
  | `keywords of string list
  ]

  (** The names of the fields (useful for querying). *)
  type field_name = [
  | `id 
  | `authors 
  | `title 
  | `how 
  | `year 
  | `url 
  | `pdfurl 
  | `comment of string
  | `bibtex 
  | `note 
  | `abstract 
  | `doi 
  | `citation 
  | `tags 
  | `keywords 
  ]

  (** One entry is a list of fields. *)
  type entry = field list

  (** A bibliography is a list of entries. *)
  type set = entry list

  (** Check the validity of the set (i.e. that each entry has an [`id]
      field, if not it returns the list of entries that are wrong. *)
  let is_valid (set:set) : [ `yes | `no of (entry list)] =
    let invalids =
      Ls.find_all
        (fun entry ->
           not (Ls.exists
                  (function `id _ -> true | _ -> false) entry))
        set in
    match invalids with
    | [] -> `yes
    | l -> `no l
        
  (** Check that the bibliography is {i compatible} with BibTeX
      generation, i.e. that it either has a [`bibtex] field, or has an
      [`id], a [`title], and a [`how] (in order to build a '\@misc'
      BibTeX entry). *)
  let is_bibtexable (set:set) : [ `yes | `no of (entry list)] =
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
    

  (** Find a field in an entry. *)
  let find_field (field:field_name) (entry:entry) =
    let f  = Ls.find_opt in
    match field with
    | `id        -> (f (function `id       v -> true | _ -> false) entry)
    | `authors   -> (f (function `authors  v -> true | _ -> false) entry)
    | `title     -> (f (function `title    v -> true | _ -> false) entry)
    | `how       -> (f (function `how      v -> true | _ -> false) entry)
    | `year      -> (f (function `year     v -> true | _ -> false) entry)
    | `url       -> (f (function `url      v -> true | _ -> false) entry)
    | `pdfurl    -> (f (function `pdfurl   v -> true | _ -> false) entry)
    | `bibtex    -> (f (function `bibtex   v -> true | _ -> false) entry)
    | `note      -> (f (function `note     v -> true | _ -> false) entry)
    | `abstract  -> (f (function `abstract v -> true | _ -> false) entry)
    | `doi       -> (f (function `doi      v -> true | _ -> false) entry)
    | `citation  -> (f (function `citation v -> true | _ -> false) entry)
    | `tags      -> (f (function `tags     v -> true | _ -> false) entry)
    | `keywords  -> (f (function `keywords v -> true | _ -> false) entry)
    | `comment key ->
        (f (function `comment (s,v) when s =$= key -> true 
            | _ -> false) entry)
            
  (** For a given entry, retrieve a field convert to a string. *)
  let field_or_empty
      ?(title_style : [`none | `punct ] = `none)
      ?(authors_style=`comas)
      (fi:field_name) (entry:entry) = 
    match find_field fi entry with
    | Some (`authors al) -> AuthorList.to_string ~style:authors_style al
    | Some (`title tit) ->
        begin match title_style with
        | `none -> tit
        | `punct ->
            begin match tit.[Str.length tit - 1] with
            | '?' | '.' | '!' -> tit
            | _ -> tit ^ "."
            end
        end
    | Some (`id id) -> id
    | Some (`how how) -> how
    | Some (`year y) -> y
    | Some (`note n) -> n
    | Some (`url       s) -> s
    | Some (`pdfurl    s) -> s
    | Some (`comment (_, s)) -> s
    | Some (`bibtex    s) -> s
    | Some (`abstract  s) -> s
    | Some (`doi       s) -> s
    | Some (`citation  s) -> s
    | Some (`tags      l) -> String.concat ", " l
    | Some (`keywords  l) -> String.concat ", " l
    | _ -> ""


  (** "Parse" a field name. *)
  let field_name_of_string: string -> field_name = function
    | "id" -> `id 
    | "authors" -> `authors 
    | "title" -> `title 
    | "how" -> `how 
    | "year" -> `year 
    | "url" -> `url 
    | "pdfurl" -> `pdfurl 
    | "comment" -> `comment "main"
    | s when (Str.length s >= 8) && (Str.sub s 0 8 =$= "comment-") ->
        let lgth = Str.length s in
        let opt = (Str.sub s 8 (lgth - 8)) in
        (`comment opt)
    | "bibtex" -> `bibtex 
    | "note" -> `note 
    | "abstract" -> `abstract 
    | "doi" -> `doi 
    | "citation" -> `citation 
    | "tags" -> `tags 
    | "keywords" -> `keywords 
    | s -> failwith ("field name unrecognizable: " ^ s)

  (** Sort the bibliography set (it is functional, the original one is
      not modified). *)
  let sort ?(by=`id) ?(reverse=false) (bibset:set) : set =
    let may_reverse = 
      if reverse then fun f a b -> f b a else fun f a b -> f a b in
    let cmp ea eb =
      let authors_style = `acm in
      may_reverse
        String.compare
        (field_or_empty ~authors_style by ea)
        (field_or_empty ~authors_style by eb) in
    Ls.sort ~cmp bibset

  (** Remove redundant entries (by default, using [compare]). *)
  let unique ?(cmp:(entry -> entry -> bool) option) (s: set) =
    Ls.unique ?cmp s

  (** Compare two entries by a given field, if the field is not
  present in both it returns [false]. *)
  let compare_by_field field a b =
    match find_field  field a with
    | Some i -> Some i =@= (find_field field b)
    | None -> false 


end

(** Parsing of the {i Sebib file format}. *)
module Parsing = struct
  (**/**)

  module Sx = Sexplib.Sexp
  (**/**)
    
  exception Parse_error of string

  (**/**)

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
        (Str.concat ", " (Ls.map (fun s -> Sx.to_string_hum s) field)) in
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

  let parse_authors sexp_list : AuthorList.t =
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
              | (Sx.Atom h) :: t as l 
                  when (h =$= "tags") || (h =$= "author") 
                    || (h =$= "authors") || (h =$= "keywords") ->
                  begin match h with
                  | "authors" | "author" -> (`authors (parse_authors t))
                  | "tags" -> (`tags (parse_atom_list "tag" t))
                  | "keywords" -> (`keywords (parse_atom_list "keyword" t))
                  | s -> fail_bad_field l
                  end
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
                  | s -> fail_bad_field l
                  end
              | (Sx.Atom "comment") :: (Sx.Atom h2) :: (Sx.Atom h3) :: [] ->
                  (`comment (h2, h3))

              | l -> fail_bad_field l)
    in
    match !its_id with
    | None -> fail_no_id field_sexp_list
    | Some id -> (id, (its_fields : Biblio.entry))

  (**/**)


  (** Convert a string to a bibliography. *)
  let rec parse str : Biblio.set =
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

    let pos = ref None in
    let res = ref [] in
    let terminate = ref false in
    let the_end = Str.length str  in
    while not !terminate do
      (* printf "pos: %d, the_end: %d\n" !pos the_end; *)
      try 
        match Sx.parse ?parse_pos:!pos str with
        | Sx.Cont (state, _) ->
          let cur_pos =
            match !pos with | None -> 0 | Some p -> p.Sx.Parse_pos.buf_pos in
          if is_white (Str.sub str cur_pos (the_end - cur_pos)) && state then
            terminate := true
          else 
            let msg =
              sprintf "Parsing Error (sexplib, unfinished s-expression?) \
                         for string: \"%s\""
                (string_abstract cur_pos 80 str) in
            raise (Parse_error msg)
        | Sx.Done (sx, parse_pos) ->
          pos := Some parse_pos;
          let id, entry = valid_and_parse_entry sx in
          res := entry :: !res;
      with
      | Sx.Parse_error _ -> (* (`Annot pe) | Sx.Parse_error (`Sexp pe) -> *)
        let cur_pos, cur_line =
          match !pos with
          | None -> 0, 0 | Some p -> p.Sx.Parse_pos.buf_pos, p.Sx.Parse_pos.text_line in
        let msg =
(*          sprintf "Parsing Error (sexplib): %s,  at \
                 character %d, i.e., %d-th of string: \"%s\""
            pe.Sx.err_msg pe.Sx.parse_state.Sx.parse_pos.Sx.buf_pos
            (pe.Sx.parse_state.Sx.parse_pos.Sx.text_char - 1) *)
          sprintf "Parse Error: XXX line: %d, pos: %d, sexp: \"%s\""
            cur_line cur_pos (string_abstract cur_pos 80 str)
        in
        raise (Parse_error msg)
    done;
    Ls.rev !res

end

(** Output of {i Sebib} files. *)    
module Printing = struct
  (**/**)

  let field_name : Biblio.field_name -> string = function
    | `id ->         "id"
    | `authors ->    "authors"
    | `title ->      "title"
    | `how ->        "how" 
    | `year ->       "year"
    | `url ->        "url" 
    | `pdfurl ->     "pdfurl"
    | `comment s -> (sprintf "comments %s" s) 
    | `bibtex ->     "bibtex" 
    | `note ->       "note" 
    | `abstract ->   "abstract"
    | `doi ->        "doi" 
    | `citation ->   "citation"
    | `tags ->       "tags" 
    | `keywords ->   "keywords"
  (**/**)


  (** Output a bibliography set. *)
  let print chan (bib_set: Biblio.set) =
    let rex = Pcre.regexp "^[A-Za-z0-9\\?\\!\\-\\:\\/\\.\\,\\~]+$" in
    let out_str s =
      let escape = 
        Str.replace_chars (function '\\' -> "\\\\" 
                           | '"' -> "\\\"" | c -> Str.of_char c) in
      if Pcre.pmatch ~rex s then s else sprintf "\"%s\"" (escape s) in
    let out_field chan entry field =
      match Biblio.field_or_empty field entry with
      | "" -> ()
      | str ->
          let str_field = (field_name field) in
          fprintf chan "    (%s %s)\n" str_field (out_str str) in
    
    let out_authors chan entry = 
      match Biblio.find_field `authors entry with
      | Some (`authors l) ->
          let f (first, last) =
            sprintf "        (%s %s)" (out_str first) (out_str last) in
          fprintf chan "    (authors \n%s)\n" (Str.concat "\n" (Ls.map l ~f))
      | _ -> () in

    let out_list chan entry field =
      match Biblio.find_field field entry with
      | Some (`tags l) ->
          fprintf chan "    (tags %s)\n" (Str.concat " " (Ls.map l ~f:out_str))
      | Some (`keywords l) ->
          fprintf chan "    (keywords %s)\n"
            (Str.concat " " (Ls.map l ~f:out_str))
      | _ -> () in

    Ls.iter bib_set
      ~f:(fun e ->
            let id = (Biblio.field_or_empty `id e) in
            fprintf chan "(   (id %s)\n" id;
            out_field chan e  `title;
            out_authors chan e;
            out_field chan e  `how;
            out_field chan e  `year;
            out_field chan e  `note;
            out_field chan e  `url;
            out_field chan e  `doi;
            out_field chan e  `pdfurl;
            out_field chan e  `bibtex;
            out_field chan e  `abstract;
            out_field chan e  `citation;
            out_list  chan e  `keywords;
            out_list  chan e  `tags;
            Ls.iter e
              ~f:(function
                  | `comment (k,v) ->
                      fprintf chan "    (comment %s %s)\n" (out_str k) (out_str v)
                  | _ -> ());
            fprintf chan  ")\n";
         );
    ()

end



(** Output BibTeX files. *)
module BibTeX = struct
  (**/**)
  let format_entry entry =
    (* The regexps are compiled at each call.
       TODO: module-local cache ?  *)
    let capitals_regexp = Pcre.regexp "[A-Z]+" in
    let leading_whitespace_regexp = 
      Pcre.regexp ~flags:[ `MULTILINE ] "^[ ]*" in
    let field fi entry = 
      Sebib_sanitize.utf8_to_latex (Biblio.field_or_empty fi entry) in
    let authors_field entry = 
      Sebib_sanitize.utf8_to_latex ~with_braces:false
        (Biblio.field_or_empty ~authors_style:`bibtex `authors entry) in
    let sanitize_title str =
      let subst s = sprintf "{%s}" s in
      Pcre.substitute ~rex:capitals_regexp ~subst str in
    match Biblio.find_field `bibtex entry with
    | Some (`bibtex b) ->
        Pcre.substitute ~rex:leading_whitespace_regexp
          ~subst:(fun s -> "") b
    | _ ->
        sprintf "@misc{%s,\n\
                author = {%s},\n\
                title = {%s},\n\
                howpublished = {%s},\n\
                year = {%s},\n\
                note = {%s}\n\
                }\n"
          (field `id entry)
          (authors_field entry)
          (sanitize_title (field `title entry))
          (field `how entry)
          (field `year entry)
          (field `note entry)
          
  (**/**)
          
  (** Convert a bibliography set to a BibTeX string *)
  let str (set:Biblio.set) =
    String.concat "\n\n" (List.map format_entry set)
  
end

(** The domain specific language of the "-select" option. *)
module Request = struct
  
  (** The type of the expressions. *)
  type t = [
  | `list_and of t list
  | `la of t list (** [`la] is a shortcut for [`list_and] *)
  | `list_or of t list
  | `lo of t list
  | `not of t
  | `matches of Biblio.field_name * string
  | `ids of string list
  | `tags of string list
  | `has of Biblio.field_name
  ]
  (**/**)
  module Sx = Sexplib.Sexp
  
  (**/**)
  
  exception Parse_error of string

  (** Parse the S-Expression reprenseting the request. *)
  let of_string str : t =
    let fail msg =
      raise (Parse_error (sprintf "Request Syntax Error: %s" msg)) in
    let atom_list l =
      Ls.map l ~f:(function Sx.Atom s -> s
                   | _ -> fail "Expecting list of atoms") in
    let rec parse_expr =
      function
      | Sx.Atom s -> fail (sprintf "Unexpected atom: %s" s)
      | Sx.List ((Sx.Atom "and") :: args) ->
          if args =@= [] then
            fail "'and' expects arguments"
          else
            (`list_and (Ls.map parse_expr args))
      | Sx.List ((Sx.Atom "or") :: args) ->
          if args =@= [] then
            fail "'or' expects arguments"
          else
            (`list_or (Ls.map parse_expr args))
      | Sx.List ((Sx.Atom "not") :: args) ->
          (`not (parse_expr (Sx.List args)))
      | Sx.List [(Sx.Atom "matches"); (Sx.Atom field); (Sx.Atom rex)] ->
          (`matches ((Biblio.field_name_of_string field), rex))
      | Sx.List ((Sx.Atom "ids") :: args) ->
          (`ids (atom_list args))
      | Sx.List ((Sx.Atom "tags") :: args) ->
          (`tags (atom_list args))
      | Sx.List [(Sx.Atom "has"); (Sx.Atom field)] ->
          (`has (Biblio.field_name_of_string field))
      | Sx.List ((Sx.List l) :: []) ->
          (parse_expr (Sx.List l))
      | s -> fail (sprintf 
                     "Can't parse expression %s" 
                     (Sx.to_string s))
    in
    let sexp = 
      try Sx.of_string (sprintf "(%s)" str) 
      with Failure msg ->
        raise (Parse_error (sprintf "Request Syntax Error (sexplib): %s" msg))
    in
    (parse_expr sexp)

  (**/**)
  let rec is_ok entry (req:t) =
    match req with
    | `list_and l | `la l -> Ls.for_all (is_ok entry) l
    | `list_or l | `lo l -> Ls.exists (is_ok entry) l
    | `not t -> not (is_ok entry t)
    | `matches (f,r) -> 
        let str = Biblio.field_or_empty f entry in
        let rex = Pcre.regexp r in
        (str <$> "") && (Pcre.pmatch ~rex str)
    | `ids l ->
        let idstr = Biblio.field_or_empty `id entry in
        Ls.exists ((=$=) idstr) l
    | `tags tags_request -> 
        begin match Biblio.find_field `tags entry with
        | Some (`tags tag_list) ->
            Ls.for_all
              (fun tag -> Ls.exists ((=$=) tag) tag_list)
              tags_request
        | _ -> false
        end
    | `has f -> 
        begin match Biblio.find_field f entry with
        | Some _ -> true
        | None -> false
        end

  (**/**)  

  (** Execute a request; i.e. filter the bibliography set. *)
  let exec req (set:Biblio.set) : Biblio.set = Ls.filter (fun e -> is_ok e req) set
    
  (** The help message about the syntax of the ([string]) requests. *)
  let help = "\
Syntax of the '-select' expressions (all parentheses are important):
    (ids <id1> <id2> <id3> ...)
        -> the items whose ids are <id1>, <id2>, ...
    (and <expr1> <expr2> ...)
        -> logical \"and\" between expressions
    (or <expr1> <expr2> ...)
        -> logical \"or\" between expressions
    (not <expr>)
        -> logical negation of an expression
    (tags <tag1> <tag2> <tag3>)
        -> look for the tags (it is an intersection, i.e. an \"and\")
    (matches <field> <regexp>)
        -> look if you find <regexp> in <field>
        The regexp syntax is Perl-Compatible
    (has <field>)
        -> the field is present
        functionally equivalent to (matches (<field> \"\"))
Examples:
    (or (has bibtex) (and (has id) (has authors) (has title) (has how)))
        -> selects entries which have a bibtex field, or at least, enough
        information to generate a @misc BibTeX entry.
    (matches title comp[a-z]+)
        -> selects entries whose title field exists and matches the regexp
        (e.g. \"The completion\" matches but \"The comp.\" does not).
"

end

(** The "-format" language. *)
module Format = struct

  (** The possible transformations/sanitizations that can be done on
  the output. *)
  type text_transformation = [
  | `no | `no_ws | `latex | `xml
  | `composition of text_transformation * text_transformation ]

  (**/**)
  let rex_ws = Pcre.regexp "[ \\t\\n\\r]+"
  let rec do_sanitization how what =
    match how with
    | `no -> what
    | `no_ws -> Pcre.substitute ~rex:rex_ws ~subst:(fun _ -> " ") what
    | `latex -> Sebib_sanitize.utf8_to_latex what
    | `xml -> Sebib_sanitize.for_xml what
    | `composition (ha, hb) -> 
        (do_sanitization ha (do_sanitization hb what))

  (**/**)

  (** Execute a pattern on a bibliography set with an optional
  transformation. *)
  let str ~pattern ?(transform_text:text_transformation= `no) (set:Biblio.set) =
    let rex_field = Pcre.regexp "@\\{[^\\}]+\\}" in

    let strfield ?authors_style ?title_style f e =
      let str = Biblio.field_or_empty ?authors_style ?title_style f e in
      do_sanitization transform_text str in
    let sub_eq s i o m =
      if String.length s < o + i then false else (String.sub s i o =$= m) in
    let is_write stack =
      Stack.is_empty stack || Stack.top stack =@= `write in
    let subs stack entry = function
      | "@{id}" when is_write stack ->  strfield `id entry
      | "@{authors}" when is_write stack -> strfield `authors entry
      | "@{authors-and}" when is_write stack ->
          strfield ~authors_style:`comas_and `authors entry 
      | "@{authors-bibtex}" when is_write stack ->
          strfield ~authors_style:`bibtex `authors entry 
      | "@{authors-acm}" when is_write stack -> 
          strfield ~authors_style:`acm `authors entry 
      | "@{authors-etal}" when is_write stack -> 
          strfield ~authors_style:`et_al `authors entry 
      | "@{title}" when is_write stack -> strfield `title entry 
      | "@{title-punct}" when is_write stack -> 
          strfield ~title_style:`punct `title entry 
      | "@{how}" when is_write stack -> strfield `how entry 
      | "@{year}" when is_write stack -> strfield `year entry 
      | "@{url}"      when is_write stack -> strfield `url       entry 
      | "@{pdfurl}"   when is_write stack -> strfield `pdfurl    entry 
      | "@{comment}" when is_write stack ->
          strfield (`comment "main") entry
      | s when sub_eq s 0 10 "@{comment-" ->
          let lgth = Str.length s in
          let opt = (Str.sub s 10 (lgth - 11)) in
          strfield (`comment opt) entry
      | "@{bibtex}"   when is_write stack -> BibTeX.format_entry entry 
      | "@{abstract}" when is_write stack -> strfield `abstract  entry 
      | "@{doi}"      when is_write stack -> strfield `doi       entry 
      | "@{citation}" when is_write stack -> strfield `citation  entry 
      | "@{tags}"     when is_write stack -> strfield `tags      entry 
      | "@{keywords}" when is_write stack -> strfield `keywords  entry 
      | "@{@}" when is_write stack -> "@" 
      | "@{n}" when is_write stack -> "\n" 
      | s when sub_eq s 0 4 "@{if" ->
          let lgth = String.length s in
          let expr = Request.of_string (String.sub s 4 (lgth - 5)) in
          begin match is_write stack, Request.is_ok entry expr with
          | true, true -> Stack.push `write stack
          | true, false -> Stack.push `no_write stack
          | false, _ -> Stack.push `silent stack
          end;
          ""
      | "@{else}" -> 
          begin try
            match Stack.pop stack with
            | `write -> Stack.push `no_write stack; ""
            | `no_write -> Stack.push `write stack; ""
            | `silent -> Stack.push `silent stack ; ""
          with e -> failwith "@{else} does not match any @{if ...}"
          end
      | "@{endif}" ->
          begin try
            let _ =  Stack.pop stack in ""
          with e -> failwith "@{endif} does not match any @{if ...}"
          end
      | s -> if is_write stack then s else ""
    in
    String.concat ""
      (Ls.concat 
         (Ls.map
            (fun entry ->
               let stack = Stack.create () in
               Ls.map (function
                       | Pcre.Text t -> if is_write stack then t else ""
                       | Pcre.Delim s -> subs stack entry s
                       | _ -> "")
                 (Pcre.full_split ~rex:rex_field pattern)) set))
      
  (** The help message about the syntax of the format strings. *)
    let help = "\
The format is a string with special patterns:
    @{id}             : id
    @{authors}        : authors (coma separated list)
    @{authors-and}    : authors (comas and a 'and' for the last one)
    @{authors-bibtex} : authors (BibTeX friendly format)
    @{authors-acm}    : authors (like ACM Ref, with initials)
    @{authors-etal}   : authors 
                        (Depending on the number of authors:
                            1: Lastname
                            2: Lastname1 and Lastname2
                            more: Lastname1 et al.)
    @{title}          : title
    @{title-punct}    : the title with a dot '.' 
                        if not already ending with '?', '.', or '!'
    @{how}            : how
    @{year}           : year
    @{note}           : note
    @{url}            : url
    @{pdfurl}         : pdfurl
    @{comment}        : comment tagged \"main\" (or non-tagged)
    @{comment-<tag>}  : comment tagged \"tag\"
    @{bibtex}         : The (maybe generated) BibTeX entry
                        (if there's no `bibtex' field, the entry is generated,
                        like for the '-bibtex' option)
    @{abstract}       : abstract
    @{doi}            : doi
    @{citation}       : citation
    @{tags}           : tags (coma separated list)
    @{keywords}       : keywords (coma separated list)
    @{@}              : the '@' character
    @{n}              : the new-line character
    @{if <expr>} AAA @{else} BBB @{endif} :
               Evaluates <expr> for the entry,
               if true, displays AAA, if not, BBB.
               (<expr> uses the same syntax as the '-select' option, 
               see 'sebib -help-select')
"

end





