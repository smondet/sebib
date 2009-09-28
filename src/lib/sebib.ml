TYPE_CONV_PATH "Sebib"

open Sebib_std

module Info = struct
    let version = "0"
    let version_string = sprintf p"SeBib v. %s" version

end

module AuthorList = struct
    type author = string * string with sexp
    type t = author list with sexp

    type style = [ `comas_and | `acm | `bibtex | `comas | `et_al ] with sexp

    let to_string ?(style:style=`comas) authors = (
        match style with
        | `bibtex ->
            String.concat " and "
                (List.map (fun (first, last) ->
                    sprintf p"%s, %s" last first) authors)
        | `comas_and ->
            let lgth = List.length authors in
            String.concat ", "
                (List.mapi (fun i (first, last) ->
                    let strand =
                        if i = lgth - 1 && i <> 0 then "and " else "" in
                    sprintf p"%s%s %s" strand first last) authors)
        | `comas -> 
            String.concat ", "
                (List.map (fun (first, last) ->
                    sprintf p"%s %s" first last) authors)
        | `acm -> 
            let lgth = List.length authors in
            String.concat ", "
                (List.mapi (fun i (first, last) ->
                    let strand =
                        if i = lgth - 1 && i <> 0 then "and " else "" in
                    let initial =
                        if String.length first = 0 
                        then '_' else first.[0] in
                    sprintf p"%s%s, %c." strand last initial) authors)
        | `et_al ->
            match authors with
            | [] -> ""
            | [(onef,onel)] -> onel
            | [(onef,onel); (twof,twol) ] -> sprintf p"%s and %s" onel twol
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
    ]
    with sexp
    type entry = field list with sexp

    type set = entry list with sexp


    let is_valid set   = (
        let invalids =
            List.find_all
                (fun entry ->
                    not (List.exists
                        (function `id _ -> true | _ -> false) entry))
                set in
        match invalids with
        | [] -> `yes
        | l -> `no l
    )

    let is_bibtexable set = (
        let has_bibtex =
            List.exists (function `bibtex _ -> true | _ -> false) in
        let is_miscable e =
            (List.exists (function `id _ -> true | _ -> false) e) &&
            (List.exists (function `title _ -> true | _ -> false) e) &&
            (* (List.exists (function `authors _ -> true | _ -> false) e) && *)
            (List.exists (function `how _ -> true | _ -> false) e)
            (* (List.exists (function `year _ -> true | _ -> false) e) *)
        in
        let invalids =
            List.find_all
                (fun entry -> not ((has_bibtex entry) || (is_miscable entry)))
                set in
        match invalids with
        | [] -> `yes
        | l -> `no l
    )

    let set_of_string str = 
        Sexplib.Sexp.of_string ("(" ^ str ^ ")") |> set_of_sexp

    let string_of_set set  =
        let s = sexp_of_set set in 
        (* Sexplib.Sexp.to_string *)
        SExpr.to_string_hum ~indent:4 s

    let find_field (field:field_name) (entry:entry) = (
        let f  = List.Exceptionless.find in
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
        let rope = Rope.of_string str in
        let ascii_buff = Buffer.create 42 in
        let u2s u = Rope.to_string (Rope.of_uchar u) in

        Rope.iter (fun u ->
            Buffer.add_string ascii_buff (latexify (u2s u));
        ) rope;
        Buffer.contents ascii_buff
    )

    let format_entry entry = (
        let field fi entry = 
            Biblio.field_or_empty ~authors_style:`bibtex fi entry
                |> sanitize_latex in
        let sanitize_title str =
            let rgx = Str.regexp "\\([^\\]\\)\\([A-Z]+\\)" in
            Str.global_replace rgx "\\1{\\2}" str in
        match Biblio.find_field `bibtex entry with
        | Some (`bibtex b) ->
            let rgx = Str.regexp "^[ ]*" in
            Str.global_replace rgx "" b
        | _ ->
            sprintf p"@misc{%s,\n\
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
        | `list_and l | `la l -> List.for_all (is_ok entry) l
        | `list_or l | `lo l -> List.exists (is_ok entry) l
        | `not t -> not (is_ok entry t)
        | `matches (f,r) -> 
            let str = Biblio.field_or_empty f entry in
            let rgx = Str.regexp r in
            (str <$> "") &&
            (try Str.search_forward rgx str 0 >= 0 with Not_found -> false)
        | `ids l ->
            let idstr = Biblio.field_or_empty `id entry in
            List.exists ((=$=) idstr) l
        | `tags tags_request -> 
            begin match Biblio.find_field `tags entry with
            | Some (`tags tag_list) ->
                List.for_all
                    (fun tag -> List.exists ((=$=) tag) tag_list)
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
        List.filter (fun e -> is_ok e req) set
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
    (has <field>)
        -> the field is present
        functionally equivalent to (matches (<field> \"\"))
Examples:
    (lo ((has bibtex) (la ((has id) (has authors) (has title) (has how)))))
        -> selects entries which have a bibtex field, or at least, enough
        information to generate a @misc BibTeX entry.
    (matches (title comp[a-z]*))
        -> selects entries whose title field exists and matches the regexp
        (e.g. \"The completion\" matches but \"The comp.\" does not).
"

end

module Format = struct

    let str ~pattern set = (
        let rgx = Str.regexp "@{[^}]+}" in
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
            | "@{comments}" when is_write stack -> strfield `comments  entry 
            | "@{bibtex}"   when is_write stack -> BibTeX.format_entry entry 
            | "@{abstract}" when is_write stack -> strfield `abstract  entry 
            | "@{doi}"      when is_write stack -> strfield `doi       entry 
            | "@{citation}" when is_write stack -> strfield `citation  entry 
            | "@{tags}"     when is_write stack -> strfield `tags      entry 
            | "@{keywords}" when is_write stack -> strfield `keywords  entry 
            | "@{more}"     when is_write stack -> strfield `more      entry 
            | "@{@}" when is_write stack -> "@" 
            | "@{n}" when is_write stack -> "\n" 
            | s when sub_eq s 0 4 "@{if" ->
                open String in
                let lgth = length s in
                let expr = Request.of_string (sub s 4 (lgth - 5)) in
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
            (List.concat (List.map (fun entry ->
                let stack = Stack.create () in
                List.map (function
                    | Str.Text t -> if is_write stack then t else ""
                    | Str.Delim s -> subs stack entry s)
                    (Str.full_split rgx pattern)) set))
    )

    let help = "\
The format is a string with special patterns:
    @{id}             : id
    @{authors}        : authors (coma separated list)
    @{authors-and}    : authors (comas and a 'and' for the last one
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
    @{comments}       : comments
    @{bibtex}         : The (maybe generated) BibTeX entry
                        (if there's no `bibtex' field, the entry is generated,
                        like for the '-bibtex' option)
    @{abstract}       : abstract
    @{doi}            : doi
    @{citation}       : citation
    @{tags}           : tags (coma separated list)
    @{keywords}       : keywords (coma separated list)
    @{more}           : more
    @{@}              : the '@' character
    @{n}              : the new-line character
    @{if <expr>} AAA @{else} BBB @{endif} :
               Evaluates <expr> for the entry,
               if true, displays AAA, if not, BBB.
               (<expr> uses the same syntax as -select, see -help-select)
"

end


module WebGet = struct

    let make_sockaddr addr port = (
        let inet_addr = (Unix.gethostbyname addr).Unix.h_addr_list.(0) in
        Unix.ADDR_INET (inet_addr, port)
    )
    let http_get path host = (
        let get = sprintf p"GET %s HTTP/1.0\r\nHost: %s\r\n\r\n" path host in
        (* printf p"REQ: %s\n%!" get; *)
        let fi, fo =
            Unix.open_connection ~autoclose:false (make_sockaddr host 80) in
        (* printf p"Connection openned\n%!"; *)
        output_string fo get;
        flush fo;
        (* printf p"Req sent\n%!"; *)
        let r = (IO.read_all fi) in
        close_in fi;
        (* Unix.shutdown_connection fi; *)
        (* close_out fo; -> is the same FD !! *)
        r
    )

    let try_find ?err_msg regexp str = (
        match get (Str.search regexp str) with
        | None ->
            failwith (Option.default "`try_find regexp str` failed" err_msg)
        | Some (_, _, s) ->
            s
    )

    let from_PubZone id = (
        let pz_host = "www.pubzone.org" in
        let pz_path =
            sprintf 
                p"/pages/publications/showPublication.do?publicationId=%s" id
        in
        let response = http_get pz_path pz_host in
        (* printf p"%s\n" response; *)
        let rgx_dblp_url =
            Str.regexp
                "\\(http://dblp.uni-trier.de/\\)db/\
                \\([a-zA-Z/0-9]*\\)/[a-zA-Z0-9]+\\.html#\\([a-zA-Z/0-9]*\\)" in
        let dblp_url =
            try_find rgx_dblp_url response
                ~err_msg:"ERROR: Didn't find the DBLP URL in response" in
        (* printf p"MATCH: %s\n" dblp_url; *)
        let bibtex_path = 
            Str.global_replace rgx_dblp_url "/rec/bibtex/\\2/\\3" dblp_url in
        (* printf p"REPLACED: %s\n" bibtex_path; *)
        let bibtex_page = http_get bibtex_path "dblp.uni-trier.de" in
        (* printf p"BIBTEX PAGE: %-20s...\n" bibtex_page; *)
        let type_rgx = Str.regexp "<pre>\\(@[a-z]+\\){" in
        let bibtex_type =
            let catched =
                try_find type_rgx bibtex_page
                    ~err_msg:"ERROR: Didn't find BibTeX entry type" in
            String.sub catched 5 (String.length catched - 5) in
        (* printf p"BIBTEX TYPE: %s\n" bibtex_type; *)
        let bibtex_rest_rgx = 
            Str.regexp "author[ \t]*=[^<]*</pre>" in
        let bibtex_entry =
            let catched =
                try_find bibtex_rest_rgx bibtex_page
                    ~err_msg:"ERROR: Didn't find the contents of the\
                        BibTeX entry" in
            String.sub catched 0 (String.length catched - 6) in
        (* printf p"BIBTEX ENTRY: %s\n" bibtex_entry; *)
        
        let dblp_xml_path = bibtex_path ^ ".xml" in
        let xml_record =
            open String in
            let got = http_get dblp_xml_path "dblp.uni-trier.de" in
            let first = find got "<?xml" in
            sub got first (length got - first)
        in
        (* printf p"XML: %s\n" xml_record; *)
        let (authors, title, year, doi) =
            let clean_pcdata =
                let entity_rgx = Str.regexp "&#[0-9]+;" in
                let replace str =
                    let code = 
                        int_of_string
                            (String.sub str 2 (String.length str - 3)) in
                        UChar.chr code |> UTF8.of_char |> UTF8.to_string in
                (fun str ->
                    String.concat ""
                        (List.map (function
                            | Str.Text t -> t
                            | Str.Delim s -> replace s)
                            (Str.full_split entity_rgx str)))
            in
            let authors = ref [] in
            let title = ref "" in
            let year = ref "" in
            let doi = ref "" in
            open Xml in
            let ixml = parse_string xml_record in
            (* printf p"TAG: %S\n" (tag ixml); *)
            Ls.iter (children (List.hd (children ixml))) ~f:(fun xml ->
                match tag xml with
                | "author" ->
                    let totalname =
                        pcdata (List.hd (children xml)) |>clean_pcdata in
                    authors := (String.rsplit totalname " ") :: !authors;
                | "title" ->
                    title := pcdata (List.hd (children xml)) |>clean_pcdata
                | "ee" ->
                    doi := pcdata (List.hd (children xml)) |>clean_pcdata
                | "year" ->
                    year := pcdata (List.hd (children xml)) |>clean_pcdata
                | _ -> ()
            );
            (Ls.rev !authors, !title, int_of_string !year, !doi)
        in
        let id = 
            let name = (String.lowercase (snd (List.hd authors))) in
            let t =
                let lowtitle = String.lowercase title in
                let rec get_first str =
                    let word, rest = String.split str " " in
                    match word with
                    | "the" | "a" | "it" | "is" ->
                        get_first rest 
                    | s -> s
                in
                String.replace_chars (function
                    | 'a' .. 'z' as a -> string_of_char a
                    | _ -> "") 
                    (get_first lowtitle)
            in
            sprintf p"%s%d%s" name year t
        in
        [   (`id id);
            (`authors authors);
            (`title title);
            (`year year);
            (`url (sprintf p"http://%s%s" pz_host pz_path));
            (`doi doi);
            (`bibtex (sprintf p"%s%s,\n%s" bibtex_type id bibtex_entry))]
    )

end



