[@@@ocaml.warning "-32"]

type direction =
  | Up
  | Down
  | Left
  | Right

type health =
  | Dead
  | Alive

type board_element =
  | Empty
  | Snake
  | Food

type snake =
  { pos : (int * int) list
  ; mutable d : direction
  ; mutable h : health
  }

type food =
  | Food of int * int
  | NoFood

type board =
  { data : board_element array
  ; width : int
  ; height : int
  ; mutable f : food
  ; mutable c : int
  }

let get_index p width = (snd p * width) + fst p
let get_position i width = i mod width, i / width

let has_food bo p =
  let i = get_index p bo.width in
  if bo.data.(i) = Food then true else false
;;

let is_within_boundary bo p =
  match p with
  | x, y when x > -1 && x < bo.width && y > -1 && y < bo.height -> true
  | _ ->
    print_endline "dead!!";
    bo.c <- 0;
    false
;;

let dead sn =
  sn.h <- Dead;
  sn
;;

let random_int lower_bound upper_bound =
  if lower_bound > upper_bound
  then invalid_arg "random_int: lower_bound > upper_bound"
  else lower_bound + Random.int (upper_bound - lower_bound + 1)
;;

(* make food inside boundary range. note: edges are also excluded*)
let make_food width height = Food (random_int 1 (width - 2), random_int 1 (height - 2))

(* grow snake in the current direction of it is moving *)
let snake_eat_food bo sn p =
  let i = get_index p bo.width in
  print_endline "snake eat!";
  bo.data.(i) <- Empty;
  bo.f <- make_food bo.width bo.height;
  bo.c <- bo.c + 1;
  { pos = p :: sn.pos; d = sn.d; h = sn.h }
;;

(* coordinate based on current head and direction - Not sure how to handle outside box scenario? *)
let next_coordinate (x, y) d =
  match d with
  | Right -> x + 1, y
  | Left -> x - 1, y
  | Up -> x, y - 1
  | Down -> x, y + 1
;;

(* remove tail through recursion*)
let rec remove_tail pos =
  match pos with
  | [] -> []
  | [ _ ] -> []
  | h :: t -> h :: remove_tail t
;;

(* move_direction add a new pos in head and remove the last element from the tail*)
let move_direction sn p = { pos = p :: remove_tail sn.pos; d = sn.d; h = sn.h }

(* move sn should first
   check if it has food
   no food -> move in that direction
   food -> grow size and move
*)
let rec move bo sn =
  let pos = next_coordinate (List.hd sn.pos) sn.d in
  match pos with
  | p when not (is_within_boundary bo p) -> dead sn
  | p when has_food bo p -> move bo (snake_eat_food bo sn p)
  | _ -> move_direction sn pos
;;

let clear_board bo =
  for i = 0 to Array.length bo.data - 1 do
    bo.data.(i) <- Empty
  done
;;

let rec place_snake bo sn =
  match sn.pos with
  | [] -> ()
  | h :: t ->
    let i = get_index h bo.width in
    bo.data.(i) <- Snake;
    place_snake bo { pos = t; d = sn.d; h = sn.h }
;;

let set_direction sn d = sn.d <- d

let make_board width height f =
  { data = Array.make (width * height) Empty; width; height; f; c = 0 }
;;

let place_food bo =
  match bo.f with
  | Food (x, y) ->
    let i = get_index (x, y) bo.width in
    bo.data.(i) <- Food
  | NoFood -> ()
;;

(* [ 10, 1; 9, 1; 8, 1; 7, 1; 6, 1; 5, 1; 4, 1; 3, 1; 2, 1; 1, 1; 0, 1 ] *)
let make_snake () =
  { pos = [ 5, 10; 4, 10; 3, 10; 2, 10; 1, 10; 0, 10 ]; d = Right; h = Alive }
;;
