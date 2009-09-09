TYPE_CONV_PATH "MainOfSebib"

open Print

module Biblio = struct
    type author = string * string with sexp

    type field = [
        | `id of string
        | `authors of author list
        | `title of string
        | `how of string
        | `date of string
        | `year of int
        | `url of string
        | `pdfurl of string
        | `image of string
        | `comments of string
        | `bibtex of string
        | `note of string
        | `abstract of string
        | `doi of string
        | `citation of string
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
        | `image     -> (f (function `image    v -> true | _ -> false) entry)
        | `comments  -> (f (function `comments v -> true | _ -> false) entry)
        | `bibtex    -> (f (function `bibtex   v -> true | _ -> false) entry)
        | `note      -> (f (function `note     v -> true | _ -> false) entry)
        | `abstract  -> (f (function `abstract v -> true | _ -> false) entry)
        | `doi       -> (f (function `doi      v -> true | _ -> false) entry)
        | `citation  -> (f (function `citation v -> true | _ -> false) entry)
    )
end

module BibTeX = struct

    
    let authors_string authors = (
        String.concat " and "
            (List.map (fun (last, first) ->
                sprintf p"%s, %s" last first) authors)
    )

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

let testminimal () = (
    let sexp_set =
        Biblio.sexp_of_set [
            [`id "bouh"; `authors [("Mondet", "Sebastien"); ("Patate",
                "Monsieur")]; (`year 2031);];
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
    let usage = "sebib [OPTIONS] file1.sebib file2.sebib ..." in
    let commands = [
        Arg.command
            ~doc:"Validate the files, continue if OK, exit 2 if not" 
            "-validate"
            (Arg.Set do_validate);
        Arg.command
            ~doc:"Also use stdin as input" 
            "-stdin"
            (Arg.Set read_stdin);
        Arg.command
            ~doc:"Output a BibTeX file (- for stdout)" 
            "-bibtex"
            (Arg.Set_string bibtex);
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
        | `yes -> printf p"Validation: OK\n";
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


    (* printf p"END\n%!"; *)


)
