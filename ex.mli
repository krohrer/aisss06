module List :
sig
  val generate : (int -> 'a) -> int -> int -> 'a list
  val from_to : int -> int -> int list
  val shuffle : 'a list -> 'a list
  val last : 'a list -> 'a
end