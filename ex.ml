module List =
struct
  let rstate = Random.State.make_self_init ()
  let rnd () = Random.State.float rstate 1.0

  let rec generate_aux fn s i l =
    if s > i then
      l
    else
      generate_aux fn s (i-1) (fn i :: l)
  
  let rec generate fn s e =
    generate_aux fn s e []
    
  let from_to s e =
    generate (fun x -> x) s e

  let shuffle l =
    let cmp a b = compare (snd a) (snd b) in
    let tag x = (x, rnd ()) in
    let untag = fst in
    let l = List.map tag l in
    let l = List.sort cmp l in
    let l = List.map untag l in
      l

  let rec last = function
  | [] -> failwith "List.last"
  | [x] -> x
  | _ :: rest -> last rest
end