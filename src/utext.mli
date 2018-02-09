(*---------------------------------------------------------------------------
   Copyright (c) 2018 The utext programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode text for OCaml.

    [Utext] provides a type for processing Unicode text.

    See also {!Uucp} and {!Pvec} and consult a
    {{!Uucp.uminimal}minimal unicode introduction}.
    TODO make a minimal Utext specific minimal intro.

    {e %%VERSION%% — Unicode version %%UNICODE_VERSION%% —
       {{:%%PKG_HOMEPAGE%% }homepage}}

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/}The Unicode FAQ.}}
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}} *)

(** {1 Utext} *)

val unicode_version : string
(** [unicode_version] is the Unicode version supported by [Utext]. *)

type t = Uchar.t Pvec.t
(** The type for Unicode text, a persistent vector of Unicode
    characters. *)

val empty : t
(** [empty] is {!Pvec.empty}, the empty Unicode text. *)

val v : len:int -> Uchar.t -> t
(** [v ~len u] is {!Pvec.v}[ ~len u]. *)

val init : len:int -> (int -> Uchar.t) -> t
(** [init ~len f] is {!Pvec.init}[ ~len f]. *)

val of_uchar : Uchar.t -> t
(** [of_uchar u] is {!Pvec.singleton}[ u]. *)

val str : string -> t
(** [str s] is Unicode text from the {e valid} UTF-8 encoded bytes [s].

    @raise Invalid_argument if [text] is invalid UTF-8, use
    {!of_utf_8} and {!try_of_utf_8} to deal with untrusted input. *)

val strf : ('a, Format.formatter, unit, t) Pervasives.format4 -> 'a
(** [strf fmt ...] is [Format.kasprintf (fun s -> str s) fmt ...)]. *)

(** {1:preds Predicates and comparison}

    See also [Pvec]'s {{!Pvec.preds}predicates and comparisons}. *)

val is_empty : t -> bool
(** [is_empty t] is [true] if [t] is empty, this is equal to
    {!Pvec.is_empty.} *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] if the elements in each vector are
    equal. {b Warning.} The test is {e textually} meaningless unless
    [t0] and [t1] are known to be in a particular form, see e.g.
    {!canonical_caseless_key} or {{!nf}normal forms}.

    {b FIXME.} Should we provide a fool-proof equality that
    always compares in [`NFD] or [`NFC] ? Problem is that
    since we are using raw Pvec.t we cannot cache. *)

val compare : t -> t -> int
(** [compare t0 t1] is the per element lexicographical order between
    [t0] and [t1]. {b Warning.} The comparison is {e textually}
    meaningless. *)

(** {1:cmap Case mapping and folding}

    For more information about case see the
    {{:http://unicode.org/faq/casemap_charprop.html#casemap}Unicode
    case mapping FAQ} and the
    {{:http://www.unicode.org/charts/case/}case mapping charts}. Note
    that these algorithms are insensitive to language and context and
    may produce sub-par results for some users. *)

val lowercased : t -> t
(** [lowercase t] is [t] lowercased according to Unicode's default case
    conversion. *)

val uppercased : t -> t
(** [uppercase t] is [t] uppercased according to Unicode's default case
    conversion. *)

val capitalized : t -> t
(** [capitalized t] is [t] capitalized: if the first character of [t]
    is {{!Uucp.Case.is_cased}cased} it is mapped to its
    {{!Uucp.Case.Map.to_title}title case mapping}; otherwise [t] is
    left unchanged. *)

val uncapitalized : t -> t
(** [uncapitalized t] is [t] uncapitalized: if the first character of
    [t] is {{!Uucp.Case.is_cased}cased} it is mapped to its
    {{!Uucp.Case.Map.to_lower}lowercase case mapping}; otherwise [t]
    is left unchanged. *)

(** {2:caseless Case insensitive equality}

    Testing the equality of two Unicode texts in a case insensitive
    manner requires a fair amount of data massaging that includes
    {{!nf}normalization} and {{!casefold}case folding}. These results
    should be cached if many comparisons have to be made on the same
    text. The following functions return keys for a given text that
    can be used to test equality against other keys. {b Do not} test
    keys generated by different functions, the comparison would be
    meaningless. See also {!identifier_caseless_key}. *)

val casefolded : t -> t
(** [casefold t] is [t] casefolded according to Unicode's default
    casefold. This can be used to implement various forms of caseless
    equalities. [equal (casefolded t0) (casefolded t1)] determines
    default case equality
    ({{:http://www.unicode.org/versions/latest/ch03.pdf#G34145}TUS
    D144}) of [t0] and [t1]. {b Warning.} In general this notion is
    not good enough use one of the following functions. *)

val canonical_caseless_key : t -> t
(** [canonical_caseless_key t] is a key such that
    [equal (canonical_caseless_key t0) (canonical_caseless_key t1)]
    determines canonical caseless
    equality ({{:http://www.unicode.org/versions/latest/ch03.pdf#G34145}TUS
    D145}) of [t0] and [t1]. *)

val compatibility_caseless_key : t -> t
(** [compatability_caseless_key t] is a key such that
    [equal (compatibility_caseless_key t0) (compatibility_caseless_key t1)]
    determines compatibility caseless
    equality ({{:http://www.unicode.org/versions/latest/ch03.pdf#G34145}TUS
    D146}) of [t0] and [t1]. *)

(** {1:ids Unicode identifiers}

    For more information see {{:http://unicode.org/reports/tr31/}UAX 31
    Unicode Identifier and Pattern Syntax}. *)

val is_identifier : t -> bool
(** [is_identifier t] is [true] iff [t] is a
    {{:http://unicode.org/reports/tr31/#Default_Identifier_Syntax}Default
    Unicode identifier}, more precisely this is
    {{:http://unicode.org/reports/tr31/#R1}UAX31-R1}. *)

val identifier_caseless_key : t -> t
(** [identifier_caseless_key t] is a key such that
    [equal (identifier_caseless_key t0) (identifier_caseless_key t1)]
    determines identifier caseless
    equality ({{:http://www.unicode.org/versions/latest/ch03.pdf##G34145}TUS
    D147}) of [t0] and [t1]. *)

(** {1:line_cut Breaking lines and paragraphs}

    These functions break text like a simple [readline] function
    would. If you are looking for line breaks to layout text, see
    {{!seg}line break segmentation}. *)

type newline = [ `ASCII | `NLF | `Readline ]
(** The type for specifying newlines.
    {ul
    {- [`ASCII] newlines occur after a CR (U+000D), LF (U+000A) or
       CRLF ([<U+000D, U+000A>]).}
    {- [`NLF] newlines occur after the
       {{:http://www.unicode.org/versions/Unicode10.0.0/ch05.pdf#G27861}
       {e Unicode newline function}}, this
       is [`ASCII] along with NEL (Ub+0085).}
    {- [`Readline] newlines are determined as for a
       {{:http://www.unicode.org/versions/Unicode10.0.0/ch05.pdf#G21160}
       {e Unicode readline function} (R4)},
       this is [`NLF] along with FF (U+000C), LS (U+2028) or
       PS (U+2029).}} *)

val lines : ?drop_empty:bool -> ?newline:newline -> t -> t Pvec.t
(** [lines ~drop_empty ~newline t] breaks [t] into subtexts separated
    by newlines determined according to [newline] (defaults to
    [`Readline]). Separators are not part of the result and lost. If
    [drop_empty] is [true] (defaults to [false]) drops lines that are
    empty. *)

val paragraphs : ?drop_empty:bool -> t -> t Pvec.t
(** [paragraphs ~newline t] breaks [t] into subtexts separated either
    by two consecutive newlines (determined as {{!newline}[`NLF]} or
    LS (U+2028)) or a single PS (U+2029). Separators are not part of
    the result and lost. If [drop_empty] is [true] (defaults to
    [false]) drops paragraphs that are empty. *)

(** {1:nf Normalization}

    For more information on normalization consult a short
    {{!Uucp.equivalence}introduction}, the
    {{:http://www.unicode.org/reports/tr15/}UAX #15 Unicode
    Normalization Forms} and
    {{:http://www.unicode.org/charts/normalization/} normalization
    charts}. *)

type normalization = [`NFD | `NFC | `NFKD | `NFKC ]
(** The type for normalization forms.
    {ul
    {- [`NFD] {{:http://www.unicode.org/glossary/#normalization_form_d}
       normalization form D}, canonical decomposition.}
    {- [`NFC] {{:http://www.unicode.org/glossary/#normalization_form_c}
       normalization form C}, canonical decomposition followed by
       canonical composition.}
    {- [`NFKD] {{:http://www.unicode.org/glossary/#normalization_form_kd}
       normalization form KD}, compatibility decomposition.}
    {- [`NFKC] {{:http://www.unicode.org/glossary/#normalization_form_kc}
       normalization form KC}, compatibility decomposition,
       followed by canonical composition.}} *)

val normalized : normalization -> t -> t
(** [normalized nf t] is [t] normalized to [nf]. *)

val is_normalized : normalization -> t -> bool
(** [is_normalized nf t] is [true] iff [t] is in normalization form [nf]. *)

(** {1:seg Segmentation}

    For more information consult the
    {{:http://www.unicode.org/reports/tr29/}UAX #29 Unicode Text
    Segmentation}, the {{:http://www.unicode.org/reports/tr14/}UAX #14
    Unicode Line Breaking Algorithm} and the web based
    {{:http://unicode.org/cldr/utility/breaks.jsp}ICU break utility}. *)

type boundary = [ `Grapheme_cluster | `Word | `Sentence | `Line_break ]
(** The type for boundaries.
    {ul
    {- [`Grapheme_cluster] determines
    {{:http://www.unicode.org/glossary/#extended_grapheme_cluster}
    extended grapheme clusters} boundaries according to UAX 29
    (corresponds, for most scripts, to user-perceived characters).}
    {- [`Word] determines word boundaries according to UAX 29.}
    {- [`Sentence] determines sentence boundaries according to UAX 29.}
    {- [`Line_break] determines {{!mandatory}mandatory} line breaks and
       line break opportunities according to UAX 14.}} *)

val segments : boundary -> t -> t Pvec.t
(** [segments b t] is are the segments of text [t] delimited by two
    boundaries of type [b]. *)

val segment_count : boundary -> t -> int
(** [segment_count b t] is [Pvec.length (segments b t)]. *)

(** {2:boundary_pos Boundary positions} *)

type pos = int
(** The type for positions. The positions of a vector [v] of length [l]
    range over \[[0];[l]\]. They are the slits before each element and after
    the last one. They are labelled from left to right by increasing number.
    The [i]th index is between positions [i] and [i+1].
{v
positions  0   1   2   3   4    l-1    l
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | l-1 |
           +---+---+---+---+     +-----+
v}  *)

val boundaries : boundary -> t -> pos Pvec.t
(** [boundaries b t] are the positions of boundaries [b] in
    [t]. *)

val boundaries_mandatory : boundary -> t -> (pos * bool) Pvec.t
(** [boundaries_mandatory] is like {!boundaries} but returns
    the mandatory status of a boundary if the kind of boundary
    sports that notion (or always [true] if not). *)

(** {1:esc Escaping and unescaping} *)

val escaped : t -> t
(** [escaped t] is [t] except characters whose general category is
    [Control], U+0022 or U+005C which are escaped according to OCaml's
    lexical conventions for strings with:
    {ul
    {- Any U+0008 (['\b']) escaped to the sequence <U+005C, U+0062>
       (["\\b"])}
    {- Any U+0009 (['\t']) escaped to the sequence <U+005C, U+0074>
       (["\\t"])}
    {- Any U+000A (['\n']) escaped to the sequence <U+005C, U+006E>
       ["\\n"]}
    {- Any U+000D (['\r']) escaped to the sequence <U+005C, U+0072>
       (["\\r"])}
    {- Any U+0022 (['\"']) escaped to the sequence <U+005C, U+0022>
       (["\\\""])}
    {- Any U+005C (['\\']) escaped to the sequence <U+005C, U+005C>
       (["\\\\"])}
    {- Any other character is escaped by an {e hexadecimal} ["\u{H+}"] escape
       with [H] a capital hexadecimal number.}}

    {b Note.} As far as OCaml is concerned [\u{H+}] escapes are only
    supported from 4.06 on. *)

val unescaped : t -> (t, int) result
(** [unescaped s] unescapes what {!escaped} did and any other valid
    [\u{H+}] escape. The, at most six, hexadecimal digits [H] of Unicode
    hex escapes can be upper, lower, or mixed case. Any truncated or
    undefined by {!escaped} escape makes the function return
    an [Error idx] with [idx] the start index of the offending escape.

    The invariant [unescape (escape t) = Ok t] holds. *)

(** {1:codec Decoding and encoding} *)

val encoding_guess : string -> [ `UTF_8 | `UTF_16BE | `UTF_16LE ] * bool
(** [encoding_guess s] is the encoding guessed for [s] coupled with
    [true] iff there's an initial
    {{:http://unicode.org/glossary/#byte_order_mark}BOM}. *)

(** {2:dec Best-effort decoding}

    {b Warning.} The following are a best-effort decodes in which any UTF-X
    decoding error is replaced by at least one replacement character
    {!Uchar.u_rep}. *)

val of_utf_8 : ?first:int -> ?last:int -> string -> t
(** [of_utf_8 ~first ~last s] is the Unicode text that results of
    best-effort UTF-8 decoding the bytes of [s] that exist in the
    range \[[first];[last]\]. [first] defaults to [0] and [last] to
    [length s - 1]. *)

val of_utf_16le : ?first:int -> ?last:int -> string -> t
(** [of_utf_16le ~first ~last s] is like {!of_utf_8} but decodes
    UTF-16LE. *)

val of_utf_16be : ?first:int -> ?last:int -> string -> t
(** [of_utf_16be ~first ~last s] is like {!of_utf_8} but decodes
    UTF-16BE. *)

(** {2:decerr Decoding with error handling} *)

type decode = (t, t * int * int option) result
(** The type for decode result. This is:
    {ul
    {- [Ok t] if no decoding error occured.}
    {- [Error (t, err, restart)] if a decoding error occured.  [t] is
       the text decoded until the error, [err] the byte index where
       the decode error occured and [restart] a valid byte index where
       a new best-effort decode could be restarted (if any).}} *)

val try_of_utf_8 : ?first:int -> ?last:int -> string -> decode
(** [try_of_utf_8] is like {!of_utf_8} except in case of error
    [Error _] is returned as described in {!decode_result}. *)

val try_of_utf_16le : ?first:int -> ?last:int -> string -> decode
(** [try_of_utf_16be] is like {!try_of_utf_8} but decodes UTF-16BE. *)

val try_of_utf_16le : ?first:int -> ?last:int -> string -> decode
(** [try_of_utf_16be] is like {!try_of_utf_8} but decodes UTF-16BE. *)

(** {2:enc Encoding}

    {b Warning.} All these functions raise [Invalid_argument] if the
    result cannot fit in the limits of {!Sys.max_string_length}. *)

val to_utf_8 : t -> string
(** [to_utf_8 t] is the UTF-8 encoding of [t]. *)

val to_utf_16le : t -> string
(** [to_utf_16le t] is the UTF-16LE encoding of [t]. *)

val to_utf_16be : t -> string
(** [to_utf_16be t] is the UTF-16BE encoding of [t]. *)

val buffer_add_utf_8 : Buffer.t -> t -> unit
(** [buffer_add_utf_8 b t] adds the UTF-8 encoding of [t] to [b]. *)

val buffer_add_utf_16le : Buffer.t -> t -> unit
(** [buffer_add_utf_16le b t] adds the UTF-16LE encoding of [t] to [b]. *)

val buffer_add_utf_16be : Buffer.t -> t -> unit
(** [buffer_add_utf_16be b t] adds the UTF-16BE encoding of [t] to [b]. *)

(** {1:pretty Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the UTF-8 encoding of [t] instructing the [ppf]
    to use a length of [1] for each grapheme cluster of [t]. *)

val pp_text : Format.formatter -> t -> unit
(** [pp_text ppf t] is like {!pp} except each line break is hinted
    to the formatter, see {!Uuseg_string.pp_utf_8_text} for details. *)

val pp_lines : Format.formatter -> t -> unit
(** [pp_lines ppf t] is like {!pp} except only {e mandatory} line breaks
    are hinted to the formatter, see {!Uuseg_string.pp_utf_8_lines} for
    details. *)

val pp_uchars : Format.formatter -> t -> unit
(** [dump_uchars ppf t] formats [t] as a sequence of OCaml {!Uchar.t} value
    using only US-ASCII encoded characters according to the Unicode
    {{:http://www.unicode.org/versions/Unicode10.0.0/appA.pdf#G7083}
    notational convention} for code points. *)

val pp_toplevel : Format.formatter -> t -> unit
(** [pp_toplevel ppf t] formats [t] using {!escaped} and {!pp} in a manner
    suitable for the toplevel to represent a [Utext.t] value.

    {b Warning.} Before OCaml 4.06 the result might not be cut and pastable
    as [\u{H+}] escapes are not supported. *)

val pp_toplevel_pvec : Format.formatter -> t Pvec.t -> unit
(** [pp_toplevel_pvec ppf ts] formats [ts] using {!pp_toplevel}. *)

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
