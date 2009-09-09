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
    ] in
    let files = Arg.handle ~usage commands in

    let bibliography_str =
        String.concat "" 
            ((IO.read_all stdin) ::
                (List.map (fun file ->
                    File.with_file_in file (fun i -> IO.read_all i)
                ) files))
    in
    let biblio = Biblio.set_of_string bibliography_str in

    if !do_validate then (
        match Biblio.is_valid biblio with
        | `yes -> printf p"Validation: OK\n";
        | `no l ->
            printf p"Validation: Following are wrong:\n%s\n" 
                (Biblio.string_of_set l);
            exit 2;
    );


    printf p"END\n%!";


)
