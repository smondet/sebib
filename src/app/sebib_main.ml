
open Sebib
open Sebib_std

let with_file_in file func =
    let i = IO.input_channel (open_in file) in
    func i

let with_file_out file func =
    let o = open_out file in
    func o

let perform_validation name condition validation biblio errcode = (
  if condition then (
    match validation biblio with
    | `yes ->
        (* printf p"Validation: OK\n"; *)
        ()
    | `no l ->
        printf "%s validation: KO" name;
        exit errcode;
  );
)

let () = (
    let do_validate = ref false in
    let do_bibtexable = ref false in
    let read_stdin = ref false in
    let bibtex = ref "" in
    let out_format = ref "" in
    let request = ref "" in
    let usage = "usage: sebib [OPTIONS] file1.sebib file2.sebib ..." in
    let sort_by = ref "" in
    let output_sebib = ref "" in
    let arg_cmd ~doc key spec = (key, spec, doc) in
    let commands = [
        arg_cmd
            ~doc:"\n\tValidate the files, continues if OK, exits(2) if not" 
            "-validate"
            (Arg.Set do_validate);
        arg_cmd
            ~doc:"\n\tChecks that every entry has either a 'bibtex' field or\n\
            \tis able to build an acceptable @misc entry (id, title, how),\n\
            \tcontinues if OK, exits(3) if not" 
            "-bibtex-able"
            (Arg.Set do_bibtexable);
        arg_cmd
            ~doc:"\n\tAlso use stdin as input" 
            "-stdin"
            (Arg.Set read_stdin);
        arg_cmd
            ~doc:"<file>\n\tOutput a BibTeX file (- for stdout)" 
            "-bibtex"
            (Arg.Set_string bibtex);
        arg_cmd
            ~doc:"<file>\n\tOutput a Sebib file (- for stdout)" 
            "-sebib"
            (Arg.Set_string output_sebib);
        arg_cmd
            ~doc:("<string>\n\
            \tOutput to stdout using the <string> format for each entry\n\
            \tsee -help-format")
            "-format"
            (Arg.Set_string out_format);
        arg_cmd
            ~doc:"\n\
            \tThis is a convenience shortcut for -format \"@{id} \""
            "-ids"
            (Arg.Unit (fun () -> out_format := "@{id} "));
        arg_cmd
            ~doc:("<s-expr>\n\
            \tFilter the bibliography with a query\n\
            \tsee -help-select")
            "-select"
            (Arg.Set_string request);
        arg_cmd
            ~doc:("<field>\n\
            \tSort the bibliography following alphabetical order on a given field")
            "-sort"
            (Arg.Set_string sort_by);
        arg_cmd
            ~doc:("\n\
            \tHelp about the -format option")
            "-help-format"
            (Arg.Unit (fun () -> printf "%s" Format.help));
        arg_cmd
            ~doc:("\n\
            \tHelp about the -select option")
            "-help-select"
            (Arg.Unit (fun () -> printf "%s" Request.help));
        arg_cmd
            ~doc:("\n\
            \tPrint version")
            "-version"
            (Arg.Unit (fun () -> printf "%s\n" Info.version_string));
    ] in
    if Array.length Sys.argv = 1 then (
        printf "%s\ntry `sebib -help`\n" usage;
    );
    let files =
        let anons = ref [] in
        let anon_fun s = anons := s :: !anons in
        Arg.parse commands anon_fun usage;
        Ls.rev !anons   in

    let bibliography_str =
        let strs_from_files =
            List.map (fun file -> with_file_in file IO.read_all) files in
        String.concat "" 
            (if !read_stdin then 
                ((IO.read_all (IO.input_channel stdin)) :: strs_from_files)
            else strs_from_files) in

    let biblio = 
        let b = Parsing.parse bibliography_str in
        let filtered =
            if !request =$= "" then b 
            else Request.exec (Request.of_string !request) b in
        let sorted =
            if !sort_by =$= "" then filtered
            else (
                let by = (Biblio.field_name_of_string !sort_by) in
                Biblio.sort ~by filtered
            ) in
        sorted in

    perform_validation "Basic" !do_validate  Biblio.is_valid biblio 2;
    perform_validation "BbTeX-able" !do_bibtexable  Biblio.is_bibtexable biblio 3;

    begin match !bibtex with
    | "" -> ();
    | "-" -> printf "%s\n" (BibTeX.str biblio);
    | f -> 
        with_file_out f (fun o -> fprintf o "%s" (BibTeX.str biblio));
    end;
    begin match !output_sebib with
    | "" -> ();
    | "-" ->
        (* let sexpr = (Biblio.string_of_set biblio) in *)
        (* print_string (String.sub sexpr 1 (String.length sexpr - 2)) *)
        failwith "Output Sebib Not Implemented Yet"
    | f -> 
        failwith "Output Sebib Not Implemented Yet"
        (*with_file_out f
          (fun o ->
             let sexpr = (Biblio.string_of_set biblio) in
          fprintf o "%s" (String.sub sexpr 1 (String.length sexpr - 2)); ); *)
    end;


    if !out_format <$> "" then (
        printf "%s" (Format.str ~pattern:!out_format biblio);
    );

    (* printf "END\n%!"; *)


)
