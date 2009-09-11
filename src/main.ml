TYPE_CONV_PATH "MainOfSebib"

open Print

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

    let set_of_string str = 
        set_of_sexp (Sexplib.Sexp.of_string ("(" ^ str ^ ")"))
    let string_of_set set =
        Sexplib.Sexp.to_string (sexp_of_set set)

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

module Format = struct



    let str ~pattern set = (
        let rgx = Str.regexp "@{[a-z-@]+}" in
        let strfield = Biblio.field_or_empty in
        let subs entry = function
            | "@{id}" -> strfield `id entry
            | "@{authors}" -> strfield `authors entry
            | "@{authors-and}" ->
                strfield ~authors_style:`comas_and `authors entry
            | "@{authors-bibtex}" ->
                strfield ~authors_style:`bibtex `authors entry
            | "@{authors-acm}" ->
                strfield ~authors_style:`acm `authors entry
            | "@{authors-etal}" ->
                strfield ~authors_style:`et_al `authors entry
            | "@{title}" -> strfield `title entry
            | "@{how}" -> strfield `how entry
            | "@{year}" -> strfield `year entry
            | "@{note}" -> strfield `note entry
            | "@{date}"     -> strfield `date      entry
            | "@{url}"      -> strfield `url       entry
            | "@{pdfurl}"   -> strfield `pdfurl    entry
            | "@{comments}" -> strfield `comments  entry
            | "@{bibtex}"   -> BibTeX.format_entry entry
            | "@{abstract}" -> strfield `abstract  entry
            | "@{doi}"      -> strfield `doi       entry
            | "@{citation}" -> strfield `citation  entry
            | "@{tags}"     -> strfield `tags      entry
            | "@{keywords}" -> strfield `keywords  entry
            | "@{more}"     -> strfield `more      entry
            | "@{@}" -> "@"
            | "@{n}" -> "\n"
            | s -> s
        in
        String.concat ""
            (List.concat (List.map (fun entry ->
                List.map (function
                    | Str.Text t -> t
                    | Str.Delim s -> subs entry s)
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
"

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
            (str <> "") &&
            (try Str.search_forward rgx str 0 >= 0 with Not_found -> false)
        | `ids l ->
            let idstr = Biblio.field_or_empty `id entry in
            List.exists ((=) idstr) l
        | `tags tags_request -> 
            begin match Biblio.find_field `tags entry with
            | Some (`tags tag_list) ->
                List.for_all
                    (fun tag -> List.exists ((=) tag) tag_list)
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



let testminimal () = (
    let sexp_set =
        Biblio.sexp_of_set [
            [`id "bouh";
                `authors [("Sebastien", "Mondet"); ("Mr", "Patate");];
                (`year 2031);];
            [`id "brout"; `how "Butterfly effect on processors"]
        ] in
    print_string (Sexplib.Sexp.to_string sexp_set);
)

let () = (
    (try if Sys.argv.(1) = "test" then (
        testminimal ();
        exit 0;) with e -> ());
    let do_validate = ref false in
    let read_stdin = ref false in
    let bibtex = ref "" in
    let out_format = ref "" in
    let request = ref "" in
    let usage = "usage: sebib [OPTIONS] file1.sebib file2.sebib ..." in
    let commands = [
        Arg.command
            ~doc:"\n\tValidate the files, continue if OK, exit 2 if not" 
            "-validate"
            (Arg.Set do_validate);
        Arg.command
            ~doc:"\n\tAlso use stdin as input" 
            "-stdin"
            (Arg.Set read_stdin);
        Arg.command
            ~doc:"<file>\n\tOutput a BibTeX file (- for stdout)" 
            "-bibtex"
            (Arg.Set_string bibtex);
        Arg.command
            ~doc:("<string>\n\
            \tOutput to stdout using the <string> format for each entry\n\
            \tsee -help-format")
            "-format"
            (Arg.Set_string out_format);
        Arg.command
            ~doc:("<s-expr>\n\
            \tFilter the bibliography with a query\n\
            \tsee -help-select")
            "-select"
            (Arg.Set_string request);
        Arg.command
            ~doc:("\n\
            \tHelp about the -format option")
            "-help-format"
            (Arg.Unit (fun () -> printf p"%s" Format.help));
        Arg.command
            ~doc:("\n\
            \tHelp about the -select option")
            "-help-select"
            (Arg.Unit (fun () -> printf p"%s" Request.help));
    ] in
    if Array.length Sys.argv = 1 then (
        printf p"%s\ntry `sebib -help`\n" usage;
    );
    let files = Arg.handle ~usage commands in

    let bibliography_str =
        let strs_from_files =
            List.map (fun file -> File.with_file_in file IO.read_all) files in
        String.concat "" 
            (if !read_stdin then 
                ((IO.read_all stdin) :: strs_from_files)
            else strs_from_files) in

    let biblio = 
        let b = Biblio.set_of_string bibliography_str in
        if !request = "" then b 
        else Request.exec (Request.of_string !request) b in

    if !do_validate then (
        match Biblio.is_valid biblio with
        | `yes ->
            (* printf p"Validation: OK\n"; *)
            ()
        | `no l ->
            printf p"Validation: Following are wrong:\n%s\n" 
                (Biblio.string_of_set l);
            exit 2;
    );

    begin match !bibtex with
    | "" -> ();
    | "-" -> printf p"%s\n" (BibTeX.str biblio);
    | f -> 
        File.with_file_out f (fun o -> fprintf o p"%s" (BibTeX.str biblio));
    end;


    if !out_format <> "" then (
        Format.str ~pattern:!out_format biblio |> printf p"%s";
    );

    (* printf p"END\n%!"; *)


)
