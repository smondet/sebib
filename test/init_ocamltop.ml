(* To be called as ocaml -init test/init_ocamltop.ml *)


#use "topfind";;
#require "extlib,pcre,sexplib";;
#directory "_build/lib/";;
#load "sebib.cma";;

open Sebib_std;;

printf "================================================================================\n";;
printf "Welcome to Sebib's TopLevel !\n\n";;
