open Core

type vertex = int
type distance = int
type edge = vertex * vertex * distance

exception Myexception

class ['a] mutableQue init = object
  val mutable v : 'a list = init
  method push hd = 
    if List.exists v ~f:(fun x -> x = hd) 
    then ()
    else v <- hd:: v
  method pop = 
    let r = List.hd_exn (List.rev v) in
    v <- List.filter v ~f:(fun x -> x <> r);
    r
  (* match v with
     | h:: t -> 
     v <- t;
     Some h
     | [] -> None *)
  method pop_exn = 
    let r = List.hd_exn (List.rev v) in
    v <- List.filter v ~f:(fun x -> x <> r);
    r
  method is_empty = 
    v = []
  method exists x = 
    List.exists v ~f:(fun y -> x = y)
  method print string_f = 
    let s = "[" ^ (String.concat ~sep:";" (List.map v ~f:string_f)) ^ "]" in
    print_string (s ^ "\n")
end

let closed_stack = new mutableQue [1;2;3]

let dijkstra (s: vertex) (t: vertex) (edges: edge list) =
  let distance_tbl = Int.Table.create () in
  let backpointer_tbl = Int.Table.create () in
  let open_vertices = new mutableQue [s] in
  let closed_vertices = new mutableQue [] in
  (* make source distance 0 *)
  let _ = Hashtbl.add distance_tbl ~key:s ~data:0 in 
  let search_tbl (v: vertex) = 
    match Hashtbl.find distance_tbl v with
    | None -> 1000
    | Some x -> x in
  let process_vertex (process_vertex: vertex) = 
    let neighbors = List.fold ~init:[] ~f:(fun accum edge -> 
        match edge with
        | (y, x, d) when y = process_vertex -> (x, d)::accum
        | (x, y, d) when y = process_vertex -> (x, d)::accum
        | _ -> accum) edges in
    let process_vertex_distance = search_tbl process_vertex in
    List.iter neighbors ~f:(fun (v, d) -> 
        let current_distance = search_tbl v in
        let new_distance = process_vertex_distance + d in
        if new_distance < current_distance 
        then 
          let _ = Hashtbl.add distance_tbl ~key:v ~data:new_distance in
          let _ = Hashtbl.add backpointer_tbl ~key:v ~data:process_vertex in () 
        else ();
        if closed_vertices#exists v then () else open_vertices#push v);
  in
  while not open_vertices#is_empty do
    open_vertices#print string_of_int;
    let v = open_vertices#pop_exn in
    process_vertex v;
    closed_vertices#push v 
  done;
  let rec path v accum = 
    match Hashtbl.find backpointer_tbl v with
    | Some x -> path x (x :: accum)
    | None -> accum in
  (search_tbl t, path t [t])


let test () = 
  let es =  [
    (1, 2, 7);
    (1, 3, 8);
    (2, 3, 5);
    (2, 4, 5);
    (2, 5, 2);
    (3, 4, 7);
    (3, 5, 4);
    (4, 6, 7);
    (5, 6, 2);
  ] in
  let s = 1 in
  let t = 6 in
  dijkstra s t es

let () = 
  let (d, p) = test () in
  print_string (string_of_int d ^ "\n");
  print_string (String.concat ~sep:" -> " (List.map p ~f:string_of_int) ^ "\n")
