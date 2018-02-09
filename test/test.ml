(*---------------------------------------------------------------------------
   Copyright (c) 2018 The utext programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let log f = Format.printf (f ^^ "@.")

let u = Utext.str
let eq = Utext.equal
let assert_f f t0 t1 = assert (Utext.equal (f (u t0)) (u t1))
let assert_invalid_arg f x = try f x; assert false with Invalid_argument _ -> ()

let test_base () =
  log "Test Utext.{empty,is_empty,str,strf}";
  assert (Pvec.length Utext.empty = 0);
  assert Utext.(is_empty empty);
  assert (Utext.(not @@ is_empty @@ str "bla"));
  assert_invalid_arg Utext.str "\xFF";
  assert (eq (Utext.strf "%d" 1) (u "1"));
  ()

let test_case () =
  log "Test Utext.{lowercased,uppercased,capitalized,uncapitalized}";
  assert_f Utext.lowercased "abc" "abc";
  assert_f Utext.lowercased "aBc" "abc";
  assert_f Utext.uppercased "abc" "ABC";
  assert_f Utext.uppercased "aBc" "ABC";
  assert_f Utext.capitalized "abc" "Abc";
  assert_f Utext.capitalized "Abc" "Abc";
  assert_f Utext.uncapitalized "abc" "abc";
  assert_f Utext.uncapitalized "Abc" "abc";
  assert_f Utext.uppercased "ǳ" "Ǳ";
  assert_f Utext.capitalized "ǳ" "ǲ";
  ()

let test_case_insensitive () =
  log "Test case insensitive equalities";
  let assert_eq key t0 t1 = assert (eq (key (u t0)) (key (u t1))) in
  let assert_neq key t0 t1 = assert (not @@ eq (key (u t0)) (key (u t1))) in
  assert_eq Utext.casefolded "Ὀδυσσεύς" "ὈΔΥΣΣΕΎΣ";
  assert_neq Utext.casefolded "Å" (*U+00C5*) "Å" (*<U+0041,U+030A>*);
  assert_eq Utext.canonical_caseless_key "Å" "Å";
  assert_eq Utext.compatibility_caseless_key "Å" "Å";
  assert_neq Utext.casefolded "Ⅻ" (*U+216B*) "ⅹⅰⅰ" (*<U+2179,U+2171,U+2171>*);
  assert_neq Utext.canonical_caseless_key "Ⅻ" "ⅹⅰⅰ";
  assert_eq Utext.compatibility_caseless_key "Ⅻ" "ⅹⅰⅰ";
  assert_neq Utext.casefolded "℅" (*U+2105*) "C/O";
  assert_neq Utext.canonical_caseless_key "℅" (*U+2105*) "C/O";
  assert_eq Utext.compatibility_caseless_key "℅" (*U+2105*) "C/O";
  assert_neq Utext.casefolded "㎒" "MHZ";
  assert_neq Utext.canonical_caseless_key "㎒" "MHZ";
  assert_eq Utext.compatibility_caseless_key "㎒" "MHZ";
  ()

let test_identifiers () =
  log "Test Utext.is_identifier";
  assert (not @@ Utext.is_identifier (u "32x"));
  assert (not @@ Utext.is_identifier (u "\xCC\x81Ela"));
  assert (Utext.is_identifier (u "Éla"));
  assert (not @@ Utext.is_identifier (u "_Éla"));
  assert (not @@ Utext.is_identifier (u "\xF0\x9F\x90\xAB"));
  assert (not @@ Utext.is_identifier (u "a\xF0\x9F\x90\xAB"));
  ()

let test_lines () =
  log "Test Utext.lines";
  let assert_ls ?drop_empty ?newline src segs =
    assert (Pvec.equal ~eq
              (Utext.lines ?drop_empty ?newline (u src))
              (Pvec.of_list (List.map u segs)))
  in
  let drop_empty = true in
  assert_ls "" [""];
  assert_ls ~drop_empty "" [];
  assert_ls "\n" ["";""];
  assert_ls ~drop_empty "\n" [];
  assert_ls "fst\r\nsnd\ntrd" ["fst";"snd";"trd"];
  assert_ls "fst\r\nsnd\ntrd\n\r" ["fst";"snd";"trd";"";""];
  assert_ls ~drop_empty "fst\r\nsnd\ntrd\n\r" ["fst";"snd";"trd"];
  assert_ls "fst\r\nsnd\ntrd\r\n" ["fst";"snd";"trd";""];
  assert_ls ~drop_empty "fst\r\nsnd\ntrd\r\n" ["fst";"snd";"trd"];
  assert_ls "\n\rabc" [""; ""; "abc"];
  assert_ls ~drop_empty "\n\rabc" ["abc"];
  assert_ls "\r\nabc" [""; "abc"];
  assert_ls ~drop_empty "\n\rabc" ["abc"];
  ()

let test_paragraphs () =
  log "Test Utext.paragraphs";
  let assert_ps ?drop_empty src segs =
    assert (Pvec.equal ~eq
              (Utext.paragraphs ?drop_empty (u src))
              (Pvec.of_list (List.map u segs)))
  in
  let drop_empty = true in
  assert_ps "" [""];
  assert_ps ~drop_empty "" [];
  assert_ps "\n\n" ["";""];
  assert_ps "\n" ["\n"];
  assert_ps "\r\n" ["\r\n"];
  assert_ps "\np1\n" ["\np1\n"];
  assert_ps "\np1\n\xE2\x80\xA9bla" ["\np1\n"; "bla"];
  assert_ps "\np1\n\xE2\x80\xA9p2" ["\np1\n"; "p2"];
  assert_ps "\np1\r\n\xE2\x80\xA9p2" ["\np1\r\n"; "p2"];
  assert_ps "p1\r\n\r\np2" ["p1"; "p2"];
  assert_ps "p1\r\n\np2" ["p1"; "p2"];
  assert_ps "p1\n\np2\n\np3\n\n" ["p1"; "p2"; "p3"; ""];
  assert_ps ~drop_empty "p1\n\np2\n\np3\n\n" ["p1"; "p2"; "p3"];
  ()

let test_normalization () =
  log "Test Utext.{normalized,is_normalized}";
  (* These tests were taken from Uunf. *)
  let test src nf dst =
    let src = Pvec.of_list (List.map Uchar.of_int src) in
    let dst = Pvec.of_list (List.map Uchar.of_int dst) in
    assert (eq (Utext.normalized nf src) dst);
    assert (Utext.is_normalized nf dst);
  in
  test [0x1E69] `NFD [0x0073; 0x0323; 0x0307];
  test [0x1E69] `NFC [0x1E69];
  test [0x1E0B; 0x0323] `NFD [0x0064; 0x0323; 0x0307];
  test [0x1E0B; 0x0323] `NFC [0x1E0D; 0x0307];
  test [0xFB01] `NFD [0xFB01];
  test [0xFB01] `NFC [0xFB01];
  test [0xFB01] `NFKD [0x0066; 0x0069];
  test [0xFB01] `NFKC [0x0066; 0x0069];
  test [0x0032; 0x2075] `NFD [0x0032; 0x2075];
  test [0x0032; 0x2075] `NFC [0x0032; 0x2075];
  test [0x0032; 0x2075] `NFKD [0x0032; 0x0035];
  test [0x0032; 0x2075] `NFKC [0x0032; 0x0035];
  test [0x1E9B; 0x0323] `NFD [0x017F; 0x0323; 0x307];
  test [0x1E9B; 0x0323] `NFC [0x1E9B; 0x0323; ];
  test [0x1E9B; 0x0323] `NFKD [0x0073; 0x0323; 0x0307];
  test [0x1E9B; 0x0323] `NFKC [0x1E69];
  test [0x0041; 0x007A; 0x0335; 0x0327; 0x0324; 0x0301; 0x0041] `NFC
       [0x0041; 0x017A; 0x0335; 0x0327; 0x0324; 0x0041];
  test [0x01C6; 0x032D] `NFKC [0x0064; 0x017E; 0x032D];
  test [0xFF80; 0x1FD3; 0xFF9E; 0x1FD3;] `NFKC [0x30BF; 0x0390; 0x3099; 0x0390];
  test [0xC100; 0x20D2; 0x11C1; 0x11C1] `NFC [0xC100; 0x20D2; 0x11C1; 0x11C1];
  ()

let test_segmentation () =
  log "Text Utext.{segments,segment_count,boundaries,boundaries_mandatory}";
  let assert_vec ~eq v els = assert (Pvec.equal ~eq v (Pvec.of_list els)) in
  let assert_segs v els = assert_vec ~eq v (List.map u els) in
  let abla = u "Åbla" in
  let heyho = u "hey ho let's go" in
  assert_segs (Utext.segments `Grapheme_cluster Utext.empty) [];
  assert (Utext.segment_count `Grapheme_cluster Utext.empty = 0);
  assert_segs (Utext.segments `Word @@ u "") [];
  assert (Pvec.length abla = 5);
  assert_segs (Utext.segments `Grapheme_cluster abla) ["Å"; "b"; "l"; "a"];
  assert (Utext.segment_count `Grapheme_cluster abla = 4);
  assert_segs (Utext.segments `Word heyho)
    ["hey"; " "; "ho"; " "; "let's"; " "; "go"];
  assert_segs (Utext.segments `Word heyho)
    ["hey"; " "; "ho"; " "; "let's"; " "; "go"];
  assert_vec ~eq:(=) (Utext.boundaries `Grapheme_cluster abla) [0;2;3;4;5];
  assert_vec ~eq:(=) (Utext.boundaries_mandatory `Line_break (u "hey ho\nno"))
    [4, false; 7, true; 9, true];
  ()

let test_escapes () =
  log "Test Utext.{escape,unescape}";
  let unescaped_ok u = match Utext.unescaped u with
  | Ok u -> u | Error _ -> assert false
  in
  let trip unesc esc =
    assert_f Utext.escaped unesc esc;
    assert_f unescaped_ok esc unesc;
    ()
  in
  trip "\xC2\x98\n" "\\u{0098}\\n";
  trip "\xC2\x98\n" "\\u{0098}\\n";
  trip "a\x00\bla" "a\\u{0000}\\bla";
  trip "\b\t\n\r\"\\" "\\b\\t\\n\\r\\\"\\\\";
  assert_f unescaped_ok "\\u{1F42B}a" "\xF0\x9F\x90\xABa";
  assert (Utext.unescaped (u "a\nbc\\") = Error 4);
  assert (Utext.unescaped (u "a\\nbc\\") = Error 5);
  assert (Utext.unescaped (u "a\\n\\u{0}bc\\u") = Error 11);
  assert (Utext.unescaped (u "a\\n\\u{0}bc\\u{") = Error 11);
  assert (Utext.unescaped (u "a\\n\\u{0}bc\\u{}") = Error 11);
  assert (Utext.unescaped (u "ab\\{}}") = Error 2);
  assert (Utext.unescaped (u "ab\\u{}}") = Error 2);
  ()

let test_codec () =
  log "Test codec";
  let trip ?(first = 0) ?last s =
    let last = match last with None -> String.length s - 1 | Some l -> l in
    assert (Utext.(to_utf_8 @@ of_utf_8 ~first ~last s) =
            String.sub s first (last - first + 1))
  in
  trip ~first:4 ~last:6 "hop hap mop";
  trip "métallique";
  trip "矢量";
  ()


let test () =
  try
    Printexc.record_backtrace true;
    test_base ();
    test_case ();
    test_case_insensitive ();
    test_identifiers ();
    test_lines ();
    test_paragraphs ();
    test_normalization ();
    test_segmentation ();
    test_escapes ();
    test_codec ();
  with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      log "%s\n%s\n[FAIL] A test failed!"
        (Printexc.to_string e) (Printexc.raw_backtrace_to_string bt);
      exit 1

let () = test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The utext programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
