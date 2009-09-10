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

    let find_field field (entry:entry) = (
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
    )
end

module BibTeX = struct

    
    let authors_string = AuthorList.to_string ~style:`bibtex 

    let str set = (
        let field_or_empty fi entry = 
            match Biblio.find_field fi entry with
            | Some (`authors al) -> authors_string al
            | Some (`title tit) -> tit
            | Some (`id id) -> id
            | Some (`how how) -> how
            | Some (`year y) -> string_of_int y
            | Some (`note n) -> n
            | _ -> ""
        in
        String.concat "\n\n"
            (List.map (fun entry ->
                match Biblio.find_field `bibtex entry with
                | Some (`bibtex b) -> b
                | _ ->
                    sprintf p"@misc {%s,\n\
                        \    author = {%s},\n\
                        \    title = {%s},\n\
                        \    howpublished = {%s},\n\
                        \    year = {%s},\n\
                        \    note = {%s}\n\
                    }\n"
                    (field_or_empty `id entry)
                    (field_or_empty `authors entry)
                    (field_or_empty `title entry)
                    (field_or_empty `how entry)
                    (field_or_empty `year entry)
                    (field_or_empty `note entry)
            ) set)
    )
end

module Format = struct


    let field_or_empty ?(authors_style=`comas) fi entry = 
        match Biblio.find_field fi entry with
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
        | _ -> ""

    let str ~pattern set = (
        let rgx = Str.regexp "@{[a-z-@]+}" in
        let subs entry = function
            | "@{id}" -> field_or_empty `id entry
            | "@{authors}" -> field_or_empty `authors entry
            | "@{authors-and}" ->
                field_or_empty ~authors_style:`comas_and `authors entry
            | "@{authors-bibtex}" ->
                field_or_empty ~authors_style:`bibtex `authors entry
            | "@{authors-acm}" ->
                field_or_empty ~authors_style:`acm `authors entry
            | "@{authors-etal}" ->
                field_or_empty ~authors_style:`et_al `authors entry
            | "@{title}" -> field_or_empty `title entry
            | "@{how}" -> field_or_empty `how entry
            | "@{year}" -> field_or_empty `year entry
            | "@{note}" -> field_or_empty `note entry
            | "@{date}"     -> field_or_empty `date      entry
            | "@{url}"      -> field_or_empty `url       entry
            | "@{pdfurl}"   -> field_or_empty `pdfurl    entry
            | "@{comments}" -> field_or_empty `comments  entry
            | "@{bibtex}"   -> field_or_empty `bibtex    entry
            | "@{abstract}" -> field_or_empty `abstract  entry
            | "@{doi}"      -> field_or_empty `doi       entry
            | "@{citation}" -> field_or_empty `citation  entry
            | "@{tags}"     -> field_or_empty `tags      entry
            | "@{keywords}" -> field_or_empty `keywords  entry
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

    let help = "
\t\t@{id}             : id
\t\t@{authors}        : authors (coma separated list)
\t\t@{authors-and}    : authors (comas and a 'and' for the last one
\t\t@{authors-bibtex} : authors (BibTeX friendly format)
\t\t@{authors-acm}    : authors (like ACM Ref, with initials)
\t\t@{authors-etal}   : authors 
\t\t                    (Depending on the number of authors:
\t\t                        1: 'Last name'
\t\t                        2: 'Last name 1' and 'Last name 2'
\t\t                        more: 'Last name 1' et al.)
\t\t@{title}          : title
\t\t@{how}            : how
\t\t@{year}           : year
\t\t@{note}           : note
\t\t@{date}           : date
\t\t@{url}            : url
\t\t@{pdfurl}         : pdfurl
\t\t@{comments}       : comments
\t\t@{bibtex}         : bibtex
\t\t@{abstract}       : abstract
\t\t@{doi}            : doi
\t\t@{citation}       : citation
\t\t@{tags}           : tags (coma separated list)
\t\t@{keywords}       : keywords (coma separated list)
\t\t@{@}              : the '@' character
\t\t@{n}              : the new-line character
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
    if Sys.argv.(1) = "test" then (
        testminimal ();
        exit 0;
    );
    let do_validate = ref false in
    let read_stdin = ref false in
    let bibtex = ref "" in
    let out_format = ref "" in
    let usage = "sebib [OPTIONS] file1.sebib file2.sebib ..." in
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
            \tThe format is:" ^ Format.help)
            "-format"
            (Arg.Set_string out_format);
    ] in
    let files = Arg.handle ~usage commands in

    let bibliography_str =
        let strs_from_files =
            List.map (fun file -> File.with_file_in file IO.read_all) files in
        String.concat "" 
            (if !read_stdin then 
                ((IO.read_all stdin) :: strs_from_files)
            else strs_from_files) in

    let biblio = Biblio.set_of_string bibliography_str in

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
