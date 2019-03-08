open Core

exception SetEmptyError

type vertex = int
type distance = int
type vertex_info = vertex * distance
type edge = vertex * vertex * distance

exception Myexception

module VertexSet : sig
  type 'a t
  val create : 'a list -> 'a t
  val pop : ('a -> 'a -> int) -> 'a t -> 'a
  val push : 'a t -> 'a -> unit
  val add : 'a t -> 'a -> ('a -> 'a -> bool) -> unit
  val is_empty : 'a t -> bool
  val exists : 'a t -> 'a -> ('a -> 'a -> bool) -> bool
  val to_string : 'a t -> ('a -> string) -> string
end = struct
  type 'a t = {mutable contents: 'a list}
  let create (l: 'a list) = 
    {contents = l}
  let pop comp self = 
    let sorted = List.sort ~compare:comp self.contents in
    match sorted with
    | h:: t -> self.contents <- t;  h
    | [] -> raise SetEmptyError
  let push self a = 
    self.contents <- a:: self.contents
  let exists self x eq = 
    List.exists self.contents ~f:(eq x)
  let add self a eq = 
    if exists self a eq then () 
    else push self a
  let is_empty self = 
    List.is_empty self.contents
  let to_string self sf = 
    "[" ^ String.concat ~sep:";" (List.map self.contents ~f:sf) ^ "]"
end

let dijkstra (s: vertex) (t: vertex) (edges: edge list) =
  let open_vertices: vertex_info VertexSet.t = VertexSet.create [(s, 0)] in
  let closed_vertices: vertex VertexSet.t = VertexSet.create [] in
  let distance_tbl = Int.Table.create () in
  let backpointer_tbl = Int.Table.create () in
  let cmp = (fun (_, d1) (_, d2) -> d1 - d2) in
  let eq = (fun x1 x2 -> cmp x1 x2 = 0) in
  let id = fun x y -> x = y in 
  let _ = Hashtbl.add distance_tbl ~key:s ~data:0 in 
  let search_tbl (v: vertex) = 
    match Hashtbl.find distance_tbl v with
    | None -> 1000
    | Some x -> x in
  let rec process () = 
    if VertexSet.is_empty open_vertices
    then ()
    else
      let (processing_vertex, _) = VertexSet.pop cmp open_vertices in
      let process_vertex_distance = search_tbl processing_vertex in
      let neighbors = List.fold ~init:[] ~f:(fun accum edge -> 
          match edge with
          | (y, x, d) when y = processing_vertex -> (x, d)::accum
          | (x, y, d) when y = processing_vertex -> (x, d)::accum
          | _ -> accum) edges in
      List.iter neighbors ~f:(fun (v, d) -> 
          let current_distance = search_tbl v in
          let new_distance = process_vertex_distance + d in
          if not (VertexSet.exists closed_vertices v id)
          then VertexSet.add open_vertices (v, min current_distance new_distance) eq
          else ();
          if  current_distance <= new_distance
          then ()
          else ( 
            ignore (Hashtbl.add distance_tbl ~key:v ~data:new_distance);
            ignore (Hashtbl.add backpointer_tbl ~key:v ~data:processing_vertex);
          ));
      VertexSet.push closed_vertices processing_vertex;
      print_string ("closed vertices: " ^ VertexSet.to_string closed_vertices string_of_int);
      process()
  in process ();
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
  print_string (String.concat ~sep:" -> " (List.map p ~f:string_of_int) ^ "\n");
  print_string "done"
