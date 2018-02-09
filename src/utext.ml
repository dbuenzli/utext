(*---------------------------------------------------------------------------
   Copyright (c) 2018 The utext programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let _strf = Printf.sprintf
let unicode_version = Uucp.unicode_version

type t = Uchar.t Pvec.t

let empty = Pvec.empty
let v = Pvec.v
let init = Pvec.init
let of_uchar = Pvec.singleton
let _of_ascii s = init ~len:(String.length s) (fun i -> Uchar.of_char s.[i])

(* Predicates and comparison. *)

let is_empty = Pvec.is_empty
let equal t0 t1 = Pvec.equal ~eq:Uchar.equal t0 t1
let compare t0 t1 = Pvec.compare ~cmp:Uchar.compare t0 t1

(* Normalization *)

type normalization = [ `NFC | `NFD | `NFKC | `NFKD ]

let normalized nf t =
  (* XXX would be interesting to do a direct reimplementation on pvec.
     and benchmark the diff (but on which corpus ?) also how would the
     case algorithms react. Also consider first checking using
     [is_normalized]. *)
  let rec add n acc v = match Uunf.add n v with
  | `Uchar u -> add n (Pvec.add_last acc u) `Await
  | `Await | `End -> acc
  in
  let add_uchar n acc u = add n acc (`Uchar u) in
  let n = Uunf.create nf in
  add n (Pvec.fold_left (add_uchar n) empty t) `End

let is_normalized nf t =
  (* FIXME this is ridiculous, do a direct reimplementation. *)
  equal t (normalized nf t)

(* Case mapping *)

let append_uchar_map map acc u = match map u with
| `Self -> Pvec.add_last acc u
| `Uchars l -> List.fold_left Pvec.add_last acc l

let lowercased t =
  Pvec.fold_left (append_uchar_map Uucp.Case.Map.to_lower) empty t

let uppercased t =
  Pvec.fold_left (append_uchar_map Uucp.Case.Map.to_upper) empty t

let _map_first_if_cased map t = match Pvec.first_el t with
| None -> t
| Some first ->
    match Uucp.Case.is_cased first with
    | false -> t
    | true ->
        match map first with
        | `Self -> t
        | `Uchars us ->
            let first_map = Pvec.of_list us in
            Pvec.(first_map ++ rem_first t)

let capitalized t = _map_first_if_cased Uucp.Case.Map.to_title t
let uncapitalized t = _map_first_if_cased Uucp.Case.Map.to_lower t

(* Case folding *)

let casefolded t =
  Pvec.fold_left (append_uchar_map Uucp.Case.Fold.fold) empty t

let uchar_app acc = function
| `Uchar u -> Pvec.add_last acc u
| `End -> acc
| `Await -> acc

let rec nf_app n app acc v = match Uunf.add n v with
| `Uchar u as v -> nf_app n app (app acc v) `Await
| `End -> app acc `End
| `Await -> acc

let map_app map app acc = function
| `Uchar u as v ->
    begin match map u with
    | `Self -> app acc v
    | `Uchars l -> List.fold_left (fun acc u -> app acc (`Uchar u)) acc l
    end
| `End -> app acc `End
| `Await -> acc

let fold_with_append append t =
  let add_uchar acc u = append acc (`Uchar u) in
  append (Pvec.fold_left add_uchar empty t) `End

let canonical_caseless_key t =
  (* XXX TUS mentions optims that could eschew the first NFD. *)
  (* This is [normalized `NFD @@ casefolded @@ normalized `NFD t] *)
  let append =
    nf_app (Uunf.create `NFD) @@ map_app Uucp.Case.Fold.fold @@
    nf_app (Uunf.create `NFD) @@ uchar_app
  in
  fold_with_append append t

let compatibility_caseless_key t =
  (* This is [normalized `NFKD @@ casefold @@ normalized `NFKD @@ casefold @@
              normalized NFD t] *)
  let append =
    nf_app (Uunf.create `NFD) @@ map_app Uucp.Case.Fold.fold @@
    nf_app (Uunf.create `NFKD) @@ map_app Uucp.Case.Fold.fold @@
    nf_app (Uunf.create `NFKD) uchar_app
  in
  fold_with_append append t

(* Identifiers *)

let is_identifier t = match Pvec.first_el t with
| None -> false
| Some u ->
    (* N.B. is_xid_continue => is_xid_start *)
    Uucp.Id.is_xid_start u && Pvec.for_all Uucp.Id.is_xid_continue t

let identifier_caseless_key t =
  (* This is [normalize `NFC @@ nfkc_casefold @@ normalize NFD t] *)
  let append =
    nf_app (Uunf.create `NFD) @@ map_app Uucp.Case.Nfkc_fold.fold @@
    nf_app (Uunf.create `NFC) @@ uchar_app
  in
  fold_with_append append t

(* Breaking lines and paragraphs *)

type newline = [ `ASCII | `NLF | `Readline ]

let lines ?(drop_empty = false)  ?(newline = `Readline) t =
  (* XXX range vs spans *)
  let add_line ls first last =
    let line = Pvec.range ~first ~last t in
    if drop_empty && Pvec.is_empty line then ls else Pvec.(add_last ls line)
  in
  let rec lines (ls, first as acc) i u = match Uchar.to_int u with
  | 0x000D (* CR *) ->
      let first' = match Pvec.el t (i + 1) with
      | Some u when Uchar.to_int u = 0x000A (* LF *) -> i + 2
      | None | Some _ -> i + 1
      in
      add_line ls first (i - 1), first'
  | 0x000A (* LF *) ->
      if first > i (* last i was CR *) then acc else
      add_line ls first (i - 1), i + 1
  | 0x0085 (* NEL *) when newline <> `ASCII ->
      add_line ls first (i - 1), i + 1
  | (0x000C (* FF *) | 0x2028 (* LS *)
    | 0x2029 (* PS *)) when newline = `Readline ->
      add_line ls first (i - 1), i + 1
  | _ -> acc
  in
  let ls, first = Pvec.foldi_left lines (empty, 0) t in
  add_line ls first (Pvec.length t - 1)

let paragraphs ?(drop_empty = false) t =
  let fst_half_break u = match Uchar.to_int u with
  | 0x000D | 0x000A | 0x0085 | 0x2028 | 0x2029 -> true | _ -> false
  in
  let snd_half_break bstart k = match Pvec.el t k with
  | None -> None
  | Some u ->
      begin match Uchar.to_int u with
      | 0x2029 (* PS *) -> Some (k, k)
      | 0x000A (* LF *) | 0x0085 (* NEL *) | 0x2028 (* LS *) -> Some (bstart, k)
      | 0x000D (* CR *) ->
          begin match Pvec.el t (k + 1) with
          | None -> Some (bstart, k)
          | Some u ->
              if Uchar.to_int u = 0x000A (* LF *) then Some (bstart, k + 1) else
              Some (bstart, k)
          end
      | _ -> None
      end
  in
  let rec find_break start = match Pvec.left_find ~start fst_half_break t with
  | None -> None
  | Some (i, u) ->
      let break = match Uchar.to_int u with
      | 0x2029 (* PS *) -> Some (i, i)
      | 0x000A (* LF *)
      | 0x0085 (* NEL *) | 0x2028 (* LS *) -> snd_half_break i (i + 1)
      | 0x000D (* CR *) ->
          begin match Pvec.el t (i + 1) with
          | None -> None
          | Some u ->
            if Uchar.to_int u = 0x000A (* LF *) then snd_half_break i (i + 2)
            else snd_half_break i (i + 1)
          end
      | _ -> assert false
      in
      match break with None -> find_break (i + 2) | Some _ as b -> b
  in
  let add_para ps first last =
    let para = Pvec.range ~first ~last t in
    if drop_empty && Pvec.is_empty para then ps else Pvec.(add_last ps para)
  in
  let rec loop ps first start = match find_break start with
  | None -> add_para ps first (Pvec.length t - 1)
  | Some (bfirst, blast) ->
      loop (add_para ps first (bfirst - 1)) (blast + 1) (blast + 1)
  in
  loop empty 0 0

(* Segmentation *)

type boundary = [ `Grapheme_cluster | `Line_break | `Sentence | `Word ]

let segments b t =
  let rec add s (segs, seg as acc) v = match Uuseg.add s v with
  | `Uchar u -> add s (segs, (Pvec.add_last seg u)) `Await
  | `Boundary ->
      let segs = if is_empty seg then segs else Pvec.add_last segs seg in
      add s (segs, empty) `Await
  | `Await | `End -> acc
  in
  let add_uchar s acc u = add s acc (`Uchar u) in
  let s = Uuseg.create b in
  fst (add s (Pvec.fold_left (add_uchar s) (empty, empty) t) `End)

let segment_count b t =
  let rec add s count v = match Uuseg.add s v with
  | `Uchar _ -> add s count `Await
  | `Boundary -> add s (count + 1) `Await
  | `Await | `End -> count
  in
  let add_uchar s count u = add s count (`Uchar u) in
  let s = Uuseg.create b in
  match add s (Pvec.fold_left (add_uchar s) 0 t) `End with
  | 0 -> 0
  | n -> n - 1

type pos = int

let boundaries b t =
  let rec add s is i v = match Uuseg.add s v with
  | `Uchar _ -> add s is i `Await
  | `Boundary -> add s (Pvec.add_last is i) i `Await
  | `Await | `End -> is
  in
  let add_uchar s is i u = add s is i (`Uchar u) in
  let s = Uuseg.create b in
  add s (Pvec.foldi_left (add_uchar s) empty t) (Pvec.length t) `End

let boundaries_mandatory b t =
  let rec add s is i v = match Uuseg.add s v with
  | `Uchar _ -> add s is i `Await
  | `Boundary -> add s (Pvec.add_last is (i, Uuseg.mandatory s)) i `Await
  | `Await | `End -> is
  in
  let add_uchar s is i u = add s is i (`Uchar u) in
  let s = Uuseg.create b in
  add s (Pvec.foldi_left (add_uchar s) empty t) (Pvec.length t) `End

(* Escaping and unescaping *)

let bslash = Uchar.of_char '\\'
let esc char = Pvec.of_list [bslash; Uchar.of_char char]
let esc_bell = esc 'b'
let esc_tab = esc 't'
let esc_lf = esc 'n'
let esc_cr = esc 'r'
let esc_quote = esc '"'
let esc_bslash = esc '\\'
let esc_uchar u = _of_ascii (_strf "\\u{%04X}" (Uchar.to_int u))
let escaped t =
  let escape_uchar (te, first as acc) i u =
    let esc u =
      Pvec.(concat_list [te; range ~first ~last:(i - 1) t; u]), i + 1
    in
    match Uchar.to_int u with
    | 0x0008 (* '\b' *) -> esc esc_bell
    | 0x0009 (* '\t' *) -> esc esc_tab
    | 0x000A (* '\n' *) -> esc esc_lf
    | 0x000D (* '\r' *) -> esc esc_cr
    | 0x0022 (* '\"' *) -> esc esc_quote
    | 0x005C (* '\\' *) -> esc esc_bslash
    | _ when Uucp.Gc.general_category u = `Cc -> esc (esc_uchar u)
    | _ -> acc
  in
  let te, first = Pvec.foldi_left escape_uchar (empty, 0) t in
  Pvec.(te ++ drop_left first t)

let is_hex_digit u = Uchar.is_char u && match Uchar.to_char u with
| '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
| _ -> false

let int_of_hex_num t =
  let add_digit n u = n * 16 + match Uchar.to_int u with
  | i when i <= 0x0039 -> i - 0x0030
  | i when i <= 0x0046 -> i - 0x0041 + 10
  | i when i <= 0x0066 -> i - 0x0061 + 10
  | _ -> assert false
  in
  Pvec.fold_left add_digit 0 t

let unescaped t =
  let not_bslash u = not @@ Uchar.equal u bslash in
  let rec loop i acc t =
    let l, r = Pvec.span_left not_bslash t in
    if Pvec.is_empty r then Ok Pvec.(acc ++ l) else
    let i = i + Pvec.length l in
    let err () = Error i in
    let unesc u i skip r =
      loop (i + skip) Pvec.(add_last (acc ++ l) u) (Pvec.drop_left skip r)
    in
    match Pvec.el r 1 with
    | None -> err ()
    | Some u ->
        match Uchar.to_int u with
        | 0x0062 (* 'b' *) -> unesc (Uchar.of_int 0x0008) i 2 r
        | 0x0074 (* 't' *) -> unesc (Uchar.of_int 0x0009) i 2 r
        | 0x006E (* 'n' *) -> unesc (Uchar.of_int 0x000A) i 2 r
        | 0x0072 (* 'r' *) -> unesc (Uchar.of_int 0x000D) i 2 r
        | 0x0022 (* '"' *) -> unesc (Uchar.of_int 0x0022) i 2 r
        | 0x005C (* '\\' *) -> unesc (Uchar.of_int 0x005C) i 2 r
        | 0x0075 (* 'u' *) ->
            begin match Pvec.el r 2 with
            | None -> err ()
            | Some u when Uchar.to_int u <> 0x007B (* '{' *) -> err ()
            | Some _ ->
                let hs, r = Pvec.(span_left is_hex_digit (drop_left 3 r)) in
                begin match Pvec.first_el r with
                | None -> err ()
                | Some u ->
                    if Uchar.to_int u <>  0x007D (* '}' *) then err () else
                    if Pvec.(length hs = 0 || length hs > 6) then err () else
                    let u = int_of_hex_num hs in
                    if not (Uchar.is_valid u) then err () else
                    unesc (Uchar.of_int u) (i + 4 + Pvec.length hs) 1 r
                end
            end
        | _ -> err ()
  in
  loop 0 empty t

(* Decoding and encoding *)

let encoding_guess = Uutf.String.encoding_guess

(* XXX implement codecs directly. *)

let str text =
  let add_uchar acc pos = function
  | `Uchar u -> Pvec.add_last acc u
  | `Malformed bytes -> invalid_arg (_strf "%d: invalid bytes %S" pos bytes)
  in
  Uutf.String.fold_utf_8 add_uchar empty text

let strf fmt = Format.kasprintf (fun s -> str s) fmt

(* Best-effort decoding *)

let of_utf_x fold ?(first = 0) ?last s =
  let add_uchar acc pos = function
  | `Uchar u -> Pvec.add_last acc u
  | `Malformed bytes -> Pvec.add_last acc Uutf.u_rep
  in
  let last = match last with None -> String.length s - 1 | Some l -> l in
  let len = last - first + 1 in
  fold ?pos:(Some first) ?len:(Some len) add_uchar empty s

let of_utf_8 = of_utf_x Uutf.String.fold_utf_8
let of_utf_16le = of_utf_x Uutf.String.fold_utf_16le
let of_utf_16be = of_utf_x Uutf.String.fold_utf_16be

(* Decoding with error handling *)

type decode = (t, t * int * int option) result

exception Dec_error of (t * int * int option)

let try_of_utf_x fold ?(first = 0) ?last s =
  let last = match last with None -> String.length s - 1 | Some l -> l in
  let add_uchar acc idx = function
  | `Uchar u -> Pvec.add_last acc u
  | `Malformed bytes ->
      let restart = idx + String.length bytes in
      let restart = if restart > last then None else Some restart in
      raise (Dec_error (acc, idx, restart))
  in
  let len = last - first + 1 in
  try Ok (fold ?pos:(Some first) ?len:(Some len) add_uchar empty s) with
  | Dec_error err -> Error err

let try_of_utf_8 = try_of_utf_x Uutf.String.fold_utf_8
let try_of_utf_16le = try_of_utf_x Uutf.String.fold_utf_16le
let try_of_utf_16le = try_of_utf_x Uutf.String.fold_utf_16be

(* Encoding *)

let buffer_add_utf_8 b t = Pvec.iter_left (Uutf.Buffer.add_utf_8 b) t
let buffer_add_utf_16le b t = Pvec.iter_left (Uutf.Buffer.add_utf_16le b) t
let buffer_add_utf_16be b t = Pvec.iter_left (Uutf.Buffer.add_utf_16be b) t

let to_utf_x buffer_add_utf_x t =
  let b = Buffer.create (Pvec.length t * 2) in
  buffer_add_utf_x b t;
  Buffer.contents b

let to_utf_8 t = to_utf_x buffer_add_utf_8 t
let to_utf_16le t = to_utf_x buffer_add_utf_16le t
let to_utf_16be t = to_utf_x buffer_add_utf_16be t

(* Pretty-printing

   XXX avoid the detour via string *)

let pp ppf t = Uuseg_string.pp_utf_8 ppf (to_utf_8 t)
let pp_text ppf t = Uuseg_string.pp_utf_8_text ppf (to_utf_8 t)
let pp_lines ppf t = Uuseg_string.pp_utf_8_lines ppf (to_utf_8 t)
let pp_uchars ppf t =
  let sep = Format.pp_print_space in
  let pp_uchar ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u) in
  Pvec.pp ~sep pp_uchar ppf t

let pp_toplevel ppf t =
  Format.fprintf ppf "(Utext.str \"%a\")" pp (escaped t)

let pp_toplevel_pvec ppf ts =
  let pp_t ppf t = Format.fprintf ppf "Utext.str \"%a\"" pp (escaped t) in
  let sep ppf () = Format.fprintf ppf ";@ " in
  Format.fprintf ppf "@[<1>Pvec.of_list @[<1>[%a]@]@]" Pvec.(pp ~sep pp_t) ts

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
