let alpha_num = 3
let alpha_denom = 4
let alpha = float_of_int alpha_num /. float_of_int alpha_denom

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type 'a sgtree = { tree : 'a tree; size : int; max_size: int }

let alpha_height (n : int) : int = 
(* log (1/a) n = (ln n)/(ln (1/a)) = -(ln n)/(ln a) *)
  let n' = float_of_int n
  in int_of_float (floor (-.log n' /. log alpha))



let make_sorted_list (bst : 'a tree) : 'a list =
  let rec make_list_aux (bst : 'a tree) (xs : 'a list) : 'a list =
    match bst with
    | Leaf -> xs
    | Node (l, value, r) -> make_list_aux l (value :: (make_list_aux r xs))
  in make_list_aux bst []
  
let make_balanced_tree (xs : 'a list) : 'a tree =
  let arr = Array.of_list xs in
    let rec build_tree ((left, right) : int * int) : 'a tree =
      if left > right then Leaf
      else if left < right then 
        let mid = (left + right)/2
        in Node (build_tree(left, mid-1), arr.(mid), build_tree(mid+1, right))
      else  Node (Leaf, arr.(left), Leaf)
    in build_tree (0, Array.length arr - 1)

let rebuild_balanced (t : 'a tree) : 'a tree =
  make_balanced_tree (make_sorted_list t)


  
let empty : 'a sgtree = { tree = Leaf; size = 0; max_size = 0 }



let find (x : 'a) (sgt : 'a sgtree) : bool =
  let rec find_aux (t : 'a tree) : bool =
    match t with
    | Leaf -> false
    | Node (l, value, r) ->
        if value < x then find_aux r
        else if value > x then find_aux l
        else true
  in find_aux sgt.tree



let rec get_size (t : 'a tree) : int =
  match t with
  | Leaf -> 0
  | Node (l, _, r) -> get_size l + get_size r + 1

let is_alpha_balanced (subtree_size : int) (root_size : int) : bool =
  subtree_size <= int_of_float(ceil(float_of_int root_size *. alpha))

let is_depth_good (depth : int) (tree_size : int) : bool =
  depth <= (alpha_height tree_size) + 1  

let insert (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let rec insert_aux (t : 'a tree) (d : int) : 'a tree * int option =
    match t with
    | Leaf -> (Node (Leaf, x, Leaf), if is_depth_good d sgt.size then Some 1 else None)
    | Node (l, value, r) ->
        if x < value then
          let (l', optional_size) = insert_aux l (d+1)
          in let t' = Node (l', value, r)
          in match optional_size with
            | None -> (t', None)
            | Some l_child_size ->
                let t'_size = 1 + get_size r + l_child_size
                in if not (is_alpha_balanced l_child_size t'_size) then 
                  (rebuild_balanced t', None) 
                else (t', Some t'_size)
        else if x > value then
          let (r', optional_size) = insert_aux r (d+1)
          in let t' = Node (l, value, r')
          in match optional_size with
            | None -> (t', None)
            | Some r_child_size ->
                let t'_size = 1 + get_size l + r_child_size
                in if not (is_alpha_balanced r_child_size t'_size) then 
                  (rebuild_balanced t', None) 
                else (t', Some t'_size)
        else failwith "Już jest w drzewie taki element"
  in { tree = fst (insert_aux sgt.tree 0); size = sgt.size + 1; max_size = max sgt.max_size (sgt.size + 1) }



let rec find_minimum (t : 'a tree) : 'a =
  match t with
  | Leaf -> failwith "Puste drzewo"
  | Node (l, value, r) ->
      match l with
      | Leaf -> value
      | Node (l', _, _) -> find_minimum l'

let rec bst_remove (x : 'a) (t : 'a tree) : 'a tree =
  match t with
  | Leaf -> failwith "Nie ma w drzewie takiego elementu"
  | Node (l, value, r) ->
    if x > value then
      Node (l, value, bst_remove x r)
    else if x < value then
      Node (bst_remove x l, value, r)
    else
      match (l, r) with
      | Leaf, _ -> r
      | _, Leaf -> l
      | _ -> let r_min = find_minimum r in Node (l, r_min, bst_remove r_min r)
                    
let remove (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let sgt' = { tree = bst_remove x sgt.tree; size = sgt.size - 1; max_size = sgt.max_size }
  in if is_alpha_balanced sgt'.size sgt'.max_size then
    { tree = rebuild_balanced sgt'.tree; size = sgt'.size; max_size = sgt'.size }
    else sgt'


let insert_n n =
  let rec insert_aux n acc =
    if n = 0 then acc
    else insert_aux (n-1) (insert n acc)
  in insert_aux n empty

let test n =
  let extract_tree (sg : 'a sgtree) : 'a tree =
    sg.tree
  in
  let sgt = ref empty in
  let start_time = Sys.time () in
  for i = 1 to n do
    sgt := insert i !sgt
  done;
  let end_time = Sys.time () in
  Printf.printf "Czas wstawiania %d elementów: %.6f sekundy\n%!" n (end_time -. start_time);
  match (!sgt).tree with
  | Leaf -> Printf.printf "Drzewo jest puste.\n"
  | Node (_, x, _) -> Printf.printf "Korzeń drzewa przed rebuild: %d\n" x;
  sgt := { !sgt with tree = rebuild_balanced (extract_tree !sgt) };
  match (!sgt).tree with
  | Leaf -> Printf.printf "Drzewo jest puste.\n"
  | Node (_, x, _) -> Printf.printf "Korzeń drzewa po rebuild: %d\n" x;
