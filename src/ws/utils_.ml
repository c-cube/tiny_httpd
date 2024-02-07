(* To keep the library lightweight, we vendor base64 and sha1
   from Daniel Bünzli's excellent libraries. Both of these functions
   are used only for the websocket handshake, on tiny data
   (one header's worth).

   vendored from https://github.com/dbuenzli/uuidm
   and https://github.com/dbuenzli/webs . *)

module B64 = struct
  let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let alpha_url =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  let encode ~url s =
    let rec loop alpha len e ei s i =
      if i >= len then
        Bytes.unsafe_to_string e
      else (
        let i0 = i and i1 = i + 1 and i2 = i + 2 in
        let b0 = Char.code s.[i0] in
        let b1 =
          if i1 >= len then
            0
          else
            Char.code s.[i1]
        in
        let b2 =
          if i2 >= len then
            0
          else
            Char.code s.[i2]
        in
        let u = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
        let c0 = alpha.[u lsr 18] in
        let c1 = alpha.[(u lsr 12) land 63] in
        let c2 =
          if i1 >= len then
            '='
          else
            alpha.[(u lsr 6) land 63]
        in
        let c3 =
          if i2 >= len then
            '='
          else
            alpha.[u land 63]
        in
        Bytes.set e ei c0;
        Bytes.set e (ei + 1) c1;
        Bytes.set e (ei + 2) c2;
        Bytes.set e (ei + 3) c3;
        loop alpha len e (ei + 4) s (i2 + 1)
      )
    in
    match String.length s with
    | 0 -> ""
    | len ->
      let alpha =
        if url then
          alpha_url
        else
          alpha
      in
      loop alpha len (Bytes.create ((len + 2) / 3 * 4)) 0 s 0
end

let sha_1 s =
  (* Based on pseudo-code of RFC 3174. Slow and ugly but does the job. *)
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen =
      if rem > 55 then
        len + 128 - rem
      else
        len + 64 - rem
    in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then (
      Bytes.set m (mlen - 8) (Char.unsafe_chr ((blen lsr 56) land 0xFF));
      Bytes.set m (mlen - 7) (Char.unsafe_chr ((blen lsr 48) land 0xFF));
      Bytes.set m (mlen - 6) (Char.unsafe_chr ((blen lsr 40) land 0xFF));
      Bytes.set m (mlen - 5) (Char.unsafe_chr ((blen lsr 32) land 0xFF))
    );
    Bytes.set m (mlen - 4) (Char.unsafe_chr ((blen lsr 24) land 0xFF));
    Bytes.set m (mlen - 3) (Char.unsafe_chr ((blen lsr 16) land 0xFF));
    Bytes.set m (mlen - 2) (Char.unsafe_chr ((blen lsr 8) land 0xFF));
    Bytes.set m (mlen - 1) (Char.unsafe_chr (blen land 0xFF));
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = sl x n lor Int32.shift_right_logical x (32 - n) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to (Bytes.length m / 64) - 1 do
    (* For each block *)
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do
      let k = base + (j * 4) in
      w.(j) <-
        sl (Int32.of_int (Char.code @@ Bytes.get m k)) 24
        lor sl (Int32.of_int (Char.code @@ Bytes.get m (k + 1))) 16
        lor sl (Int32.of_int (Char.code @@ Bytes.get m (k + 2))) 8
        lor Int32.of_int (Char.code @@ Bytes.get m (k + 3))
    done;
    (* Loop *)
    a := !h0;
    b := !h1;
    c := !h2;
    d := !h3;
    e := !h4;
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then
          !b land !c lor (lnot !b land !d), 0x5A827999l
        else if t <= 39 then
          !b lxor !c lxor !d, 0x6ED9EBA1l
        else if t <= 59 then
          !b land !c lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
        else
          !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if t >= 16 then
        w.(s) <-
          cls 1
            (w.(s + 13 &&& 0xF)
            lxor w.(s + 8 &&& 0xF)
            lxor w.(s + 2 &&& 0xF)
            lxor w.(s));
      let temp = cls 5 !a ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = Bytes.create 20 in
  let i2s h k i =
    Bytes.set h k (Char.unsafe_chr (Int32.to_int (sr i 24) &&& 0xFF));
    Bytes.set h (k + 1) (Char.unsafe_chr (Int32.to_int (sr i 16) &&& 0xFF));
    Bytes.set h (k + 2) (Char.unsafe_chr (Int32.to_int (sr i 8) &&& 0xFF));
    Bytes.set h (k + 3) (Char.unsafe_chr (Int32.to_int i &&& 0xFF))
  in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  Bytes.unsafe_to_string h

(*---------------------------------------------------------------------------
   Copyright (c) 2008 The uuidm programmers

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
