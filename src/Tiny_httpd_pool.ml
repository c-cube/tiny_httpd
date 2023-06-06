module A = Tiny_httpd_atomic_

type 'a list_ = Nil | Cons of int * 'a * 'a list_

type 'a t = {
  mk_item: unit -> 'a;
  max_size: int;  (** Max number of items *)
  items: 'a list_ A.t;
}

let create ~mk_item ?(max_size = 128) () : _ t =
  { mk_item; max_size; items = A.make Nil }

let rec acquire_ self =
  match A.get self.items with
  | Nil -> self.mk_item ()
  | Cons (_, x, tl) as l ->
    if A.compare_and_set self.items l tl then
      x
    else
      acquire_ self

let[@inline] size_ = function
  | Cons (sz, _, _) -> sz
  | Nil -> 0

let rec release_ self x : unit =
  match A.get self.items with
  | Cons (sz, _, _) when sz >= self.max_size ->
    (* forget the item *)
    ()
  | l ->
    if not (A.compare_and_set self.items l (Cons (size_ l + 1, x, l))) then
      release_ self x

let with_resource (self : _ t) f =
  let x = acquire_ self in
  Fun.protect ~finally:(fun () -> release_ self x) (fun () -> f x)
