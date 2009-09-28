
open Sebib
open Sebib_std

let testminimal () = (
    let sexp_set =
        Biblio.sexp_of_set [
            [`id "bouh";
                `authors [("Sebastien", "Mondet"); ("Mr", "Patate");];
                (`year 2031);];
            [`id "brout"; `how "Butterfly effect on processors"]
        ] in
    print_string (Sexplib.Sexp.to_string sexp_set);
    let mondet08streaming = WebGet.from_PubZone "504967" in
    printf p"testminimal : %s\n%!" (Biblio.string_of_set [mondet08streaming]);
)

let perform_validation name condition validation biblio errcode = (
    if condition then (
        match validation biblio with
        | `yes ->
            (* printf p"Validation: OK\n"; *)
            ()
        | `no l ->
            printf p"%s validation; the following items are wrong:\n%s\n" 
                name
                (Biblio.string_of_set l);
            exit errcode;
    );
)
let () = (
    if (try Sys.argv.(1) =$= "test" with e -> false) then (
        testminimal ();
        exit 30;
    );
    let do_validate = ref false in
    let do_bibtexable = ref false in
    let read_stdin = ref false in
    let bibtex = ref "" in
    let out_format = ref "" in
    let request = ref "" in
    let pubzone = ref [] in
    let usage = "usage: sebib [OPTIONS] file1.sebib file2.sebib ..." in
    let commands = [
        Arg.command
            ~doc:"\n\tValidate the files, continues if OK, exits(2) if not" 
            "-validate"
            (Arg.Set do_validate);
        Arg.command
            ~doc:"\n\tChecks that every entry has either a 'bibtex' field or\n\
            \tis able to build an acceptable @misc entry (id, title, how),\n\
            \tcontinues if OK, exits(3) if not" 
            "-bibtex-able"
            (Arg.Set do_bibtexable);
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
            ~doc:"\n\
            \tThis is a convenience shortcut for -format \"@{id} \""
            "-ids"
            (Arg.Unit (fun () -> out_format := "@{id} "));
        Arg.command
            ~doc:("<s-expr>\n\
            \tFilter the bibliography with a query\n\
            \tsee -help-select")
            "-select"
            (Arg.Set_string request);
        Arg.command
            ~doc:("<pub-id>\n\
            \tAtempt to get information from pubzone.org \
            (Experimental feature).\n\
            \tThe <pub-id> can be obtained from the URL, e.g. \
            \"?publicationId=1234388\"")
            "-pubzone"
            (Arg.String (fun s -> pubzone := s :: !pubzone));
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
        if !request =$= "" then b 
        else Request.exec (Request.of_string !request) b in

    perform_validation "Basic" !do_validate  Biblio.is_valid biblio 2;
    perform_validation "BbTeX-able" !do_bibtexable  Biblio.is_bibtexable biblio 3;

    begin match !bibtex with
    | "" -> ();
    | "-" -> printf p"%s\n" (BibTeX.str biblio);
    | f -> 
        File.with_file_out f (fun o -> fprintf o p"%s" (BibTeX.str biblio));
    end;

    if !pubzone <@> [] then (
        let entries = Ls.rev_map !pubzone ~f:WebGet.from_PubZone in 
        let sexpr = (Biblio.string_of_set entries) in
        String.sub sexpr 1 (String.length sexpr - 2) |> print_string
    );

    if !out_format <$> "" then (
        Format.str ~pattern:!out_format biblio |> printf p"%s";
    );

    (* printf p"END\n%!"; *)


)
