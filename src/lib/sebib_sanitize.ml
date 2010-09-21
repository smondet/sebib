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

open Sebib_std


(* This function will always be work-in-progress...
   
   Special Thanks to Vim (www.vim.org) for easing this editing
   
   The Comprehensive LaTeX Symbol List
   www.stat.berkeley.edu/~sourav/symbols-a4.pdf 
*)

let latexify ?(with_braces=true) = function

  | "\\" -> "\\textbackslash{}"
  | "}" when with_braces -> "\\}"
  | "{" when with_braces -> "\\{"

  | "$" -> "\\$"
  | "%" -> "\\%"
  | "_" -> "\\_"
  | "&" -> "\\&"
  | "#" -> "\\#"
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

let utf8_to_latex ?with_braces str = (
  let ascii_buff = Buffer.create 42 in
  let uchar_string uc =
    UTF8.init 1 (fun _ -> uc) in
  UTF8.iter 
    (fun unicode_char ->
       let ministr = uchar_string unicode_char in
       Buffer.add_string ascii_buff (latexify ?with_braces ministr);)
    ((*UTF8.of_string*) str);
  Buffer.contents ascii_buff
)

let for_xml str =
  let buf = Buffer.create 42 in
  let s = Buffer.add_string buf in
  let c = Buffer.add_char buf in
  Str.iter
    (function
     | '<' -> s "&lt;"
     | '>' -> s "&gt;"
     | '&' -> s "&amp;"
     | har -> c har) str;
  (Buffer.contents buf)


