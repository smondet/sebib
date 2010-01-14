TYPE_CONV_PATH "Sebib"

open Sebib_std

module Info = struct
    let version = "0"
    let version_string = sprintf "SeBib v. %s" version

end

module AuthorList = struct
    type author = string * string
    type t = author list

    type style = [ `comas_and | `acm | `bibtex | `comas | `et_al ]

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

    type field_name = [
        | `id 
        | `authors 
        | `title 
        | `how 
        | `date 
        | `year 
        | `url 
        | `pdfurl 
        | `comment
        | `bibtex 
        | `note 
        | `abstract 
        | `doi 
        | `citation 
        | `tags 
        | `keywords 
    ] with sexp

    type entry = field list

    type set = entry list


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
        | `comment   -> (f (function `comment  v -> true | _ -> false) entry)
        | `bibtex    -> (f (function `bibtex   v -> true | _ -> false) entry)
        | `note      -> (f (function `note     v -> true | _ -> false) entry)
        | `abstract  -> (f (function `abstract v -> true | _ -> false) entry)
        | `doi       -> (f (function `doi      v -> true | _ -> false) entry)
        | `citation  -> (f (function `citation v -> true | _ -> false) entry)
        | `tags      -> (f (function `tags     v -> true | _ -> false) entry)
        | `keywords  -> (f (function `keywords v -> true | _ -> false) entry)
    )

    
    let field_or_empty ?(authors_style=`comas) (fi:field_name) entry = 
        match find_field fi entry with
        | Some (`authors al) -> AuthorList.to_string ~style:authors_style al
        | Some (`title tit) -> tit
        | Some (`id id) -> id
        | Some (`how how) -> how
        | Some (`year y) -> y
        | Some (`note n) -> n
        | Some (`date      s) -> s
        | Some (`url       s) -> s
        | Some (`pdfurl    s) -> s
        | Some (`comment ("main", s)) -> s
        | Some (`bibtex    s) -> s
        | Some (`abstract  s) -> s
        | Some (`doi       s) -> s
        | Some (`citation  s) -> s
        | Some (`tags      l) -> String.concat ", " l
        | Some (`keywords  l) -> String.concat ", " l
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
        | "comment" -> `comment
        | "bibtex" -> `bibtex 
        | "note" -> `note 
        | "abstract" -> `abstract 
        | "doi" -> `doi 
        | "citation" -> `citation 
        | "tags" -> `tags 
        | "keywords" -> `keywords 
        | s -> failwith ("field name unrecognizable: " ^ s)

    let sort ?(by=`id) bibset = 
        let cmp ea eb =
            let authors_style = `acm in
            String.compare
                (field_or_empty ~authors_style by ea)
                (field_or_empty ~authors_style by eb) in
        Ls.sort ~cmp bibset

end

module Parsing = struct
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
    | Some id -> (id, (its_fields : Biblio.entry))

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
end





module BibTeX = struct

    (* 
     This function will always be work-in-progress...

     Special Thanks to Vim (www.vim.org) for easing this editing

     The Comprehensive LaTeX Symbol List
     www.stat.berkeley.edu/~sourav/symbols-a4.pdf 
     *)
    let latexify = function

        | "\\" -> "\\textbackslash{}"

        | "$" -> "\\$"
        | "%" -> "\\%"
        | "_" -> "\\_"
        | "}" -> "\\}"
        | "&" -> "\\&"
        | "#" -> "\\#"
        | "{" -> "\\{"
        | "^" -> "\\^{}"

        | "á" -> "\\'a"
        | "é" -> "\\'e"
        | "í" -> "\\'i"
        | "ó" -> "\\'o"
        | "ú" -> "\\'u"
        | "ý" -> "\\'y"

        | "Á" -> "\\'A"
        | "É" -> "\\'E"
        | "Í" -> "\\'I"
        | "Ó" -> "\\'O"
        | "Ú" -> "\\'U"
        | "Ý" -> "\\'Y"

        | "à" -> "\\`a"
        | "è" -> "\\`e"
        | "ì" -> "\\`i"
        | "ò" -> "\\`o"
        | "ù" -> "\\`u"
        
        | "À" -> "\\`A"
        | "È" -> "\\`E"
        | "Ì" -> "\\`I"
        | "Ò" -> "\\`O"
        | "Ù" -> "\\`U"

        | "â" -> "\\^a"
        | "ê" -> "\\^e"
        | "î" -> "\\^i"
        | "ô" -> "\\^o"
        | "û" -> "\\^u"

        | "Â" -> "\\^A"
        | "Ê" -> "\\^E"
        | "Î" -> "\\^I"
        | "Ô" -> "\\^O"
        | "Û" -> "\\^U"

        | "ä" -> "\\\"{a}"
        | "ë" -> "\\\"{e}"
        | "ï" -> "\\\"{i}"
        | "ö" -> "\\\"{o}"
        | "ü" -> "\\\"{u}"
        | "ÿ" -> "\\\"{y}"

        | "Ä" -> "\\\"{A}"
        | "Ë" -> "\\\"{E}"
        | "Ï" -> "\\\"{I}"
        | "Ö" -> "\\\"{O}"
        | "Ü" -> "\\\"{U}"
        | "Ÿ" -> "\\\"{Y}"

        | "ā" -> "\\={a}"
        | "ē" -> "\\={e}"
        | "ī" -> "\\={i}"
        | "ū" -> "\\={u}"

        | "ş" -> "\\c{s}"
        | "ç" -> "\\c{c}"
        | "ţ" -> "\\c{t}"

        | "ñ" -> "\\~{n}"
        | "Ñ" -> "\\~{N}"


        | "ø" -> "\\o{}"
        | "ß" -> "\\ss{}"
        | "Ø" -> "\\O{}"
        | "æ" -> "\\ae{}"
        | "œ" -> "\\oe{}"
        | "Æ" -> "\\AE{}"
        | "Œ" -> "\\OE{}"

        | "«" -> "\\guillemotleft{}"
        | "‹" -> "\\guilsinglleft{}"
        | "„" -> "\\quotedblbase{}"
        | "\"" -> "\\textquotedbl{}"
        | "»" -> "\\guillemotright{}"
        | "›" -> "\\guilsinglright{}"
        | "‚" -> "\\quotesinglbase{}"

        | "£" -> "\\pounds{}"
        | "€" -> "\\euro{}"
        | "¥" -> "\\textyen{}"
        | "₩" -> "\\textwon{}"

        | "©" -> "\\textcopyright{}"
        | "†" -> "\\textdagger{}"
        | "‘" -> "\\textquoteleft{}"
        | "’" -> "\\textquoteright{}"
        | "‡" -> "\\textdaggerdbl{}"
        | "®" -> "\\textregistered{}"

        | s -> s

    let sanitize_latex str = (
        let ascii_buff = Buffer.create 42 in
        let uchar_string uc =
            UTF8.init 1 (fun _ -> uc) in
        UTF8.iter 
            (fun unicode_char ->
                 let ministr = uchar_string unicode_char in
                 Buffer.add_string ascii_buff (latexify ministr);)
            ((*UTF8.of_string*) str);
        Buffer.contents ascii_buff
    )


    let format_entry entry = (
        (* The regexps are compiled at each call. TODO: module-local cache ?  *)
        let capitals_regexp = Pcre.regexp "[A-Z]+" in
        let leading_whitespace_regexp = 
            Pcre.regexp ~flags:[ `MULTILINE ] "^[ ]*" in
        let field fi entry = 
            sanitize_latex
                (Biblio.field_or_empty ~authors_style:`bibtex fi entry) in
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
                (field `authors entry)
                (sanitize_title (field `title entry))
                (field `how entry)
                (field `year entry)
                (field `note entry)
    )

    let str set = (
        String.concat "\n\n" (List.map format_entry set)
    )
end

(** The "-select" domain specific language *)
module Request = struct

    type t = [
        | `list_and of t list
        | `la of t list
        | `list_or of t list
        | `lo of t list
        | `not of t
        | `matches of Biblio.field_name * string
        | `ids of string list
        | `tags of string list
        | `has of Biblio.field_name
    ] with sexp

    let rec is_ok entry (req:t) = (
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

    )

    let exec req set = (
        Ls.filter (fun e -> is_ok e req) set
    )
    let of_string str = (
        t_of_sexp (Sexplib.Sexp.of_string str)
    )
    let help = "\
Syntax of the '-select' expressions (all parentheses are important):
    (ids (<id1> <id2> <id3> ...))
        -> the items whose ids are <id1>, <id2>, ...
    (list_and (<expr1> <expr2> ...))
        -> logical \"and\" between expressions
        (la ...) is a shortcut to (list_and ...)
    (list_or (<expr1> <expr2> ...))
        -> logical \"or\" between expressions
        (lo ...) is a shortcut to (list_or ...)
    (not <expr>)
        -> logical negation of an expression
    (tags (<tag1> <tag2> <tag3>))
        -> look for the tags (it is an intersection, an \"and\")
    (matches (<field> <regexp>))
        -> look if you find <regexp> in <field>
        The regexp syntax is Perl-Compatible
    (has <field>)
        -> the field is present
        functionally equivalent to (matches (<field> \"\"))
Examples:
    (lo ((has bibtex) (la ((has id) (has authors) (has title) (has how)))))
        -> selects entries which have a bibtex field, or at least, enough
        information to generate a @misc BibTeX entry.
    (matches (title comp[a-z]+))
        -> selects entries whose title field exists and matches the regexp
        (e.g. \"The completion\" matches but \"The comp.\" does not).
"

end

module Format = struct

    let str ~pattern set = (
        let rex = Pcre.regexp "@\\{[^\\}]+\\}" in
        let strfield = Biblio.field_or_empty in
        let sub_eq s i o m =
            if String.length s < o + i then
                false
            else
                (String.sub s i o =$= m) in
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
            | "@{how}" when is_write stack -> strfield `how entry 
            | "@{year}" when is_write stack -> strfield `year entry 
            | "@{date}"     when is_write stack -> strfield `date      entry 
            | "@{url}"      when is_write stack -> strfield `url       entry 
            | "@{pdfurl}"   when is_write stack -> strfield `pdfurl    entry 
            | "@{comment}" when is_write stack -> strfield `comment entry
            | "@{bibtex}"   when is_write stack -> BibTeX.format_entry entry 
            | "@{abstract}" when is_write stack -> strfield `abstract  entry 
            | "@{doi}"      when is_write stack -> strfield `doi       entry 
            | "@{citation}" when is_write stack -> strfield `citation  entry 
            | "@{tags}"     when is_write stack -> strfield `tags      entry 
            | "@{keywords}" when is_write stack -> strfield `keywords  entry 
            | "@{@}" when is_write stack -> "@" 
            | "@{n}" when is_write stack -> "\n" 
            | s when sub_eq s 0 4 "@{if" ->
                (* open String in *)
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
            (Ls.concat (Ls.map (fun entry ->
                let stack = Stack.create () in
                Ls.map (function
                    | Pcre.Text t -> if is_write stack then t else ""
                    | Pcre.Delim s -> subs stack entry s
                    | _ -> "")
                    (Pcre.full_split ~rex pattern)) set))
    )

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
    @{how}            : how
    @{year}           : year
    @{note}           : note
    @{date}           : date
    @{url}            : url
    @{pdfurl}         : pdfurl
    @{comment}        : \"main\" comment
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





