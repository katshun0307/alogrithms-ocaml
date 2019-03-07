open Core 
open Gnuplot

type data_point = float array

let plus (a: data_point) (b: data_point) : data_point = 
  Array.map2_exn a b ~f:(fun a b -> a +. b)

let dist (n: int) (a: data_point) (b: data_point): float = 
  Array.fold (
    Array.map2_exn a b ~f:(fun a b -> (Float.abs (a -. b)) ** float_of_int n))
    ~f:(fun accum a -> accum +. a) ~init:0. 
  ** (1. /. (float_of_int n))

class kmeans_graph (init: float array array) (init_k: int) (n_norm: int) v = object(self)
  val mutable data : data_point array = init
  val mutable k : int = init_k
  val mutable group : data_point list array = Array.init init_k ~f:(fun _ -> [])
  val mutable n : int = Array.length init
  val mutable dim : int = Array.length (init.(0))
  val mutable plus : data_point -> data_point -> data_point = plus
  val mutable dist : data_point -> data_point -> float = dist n_norm
  val mutable past_avg : data_point array = [||]
  val mutable update_times: int = 0
  val mutable verbose: bool = v
  method group_clear = 
    group <- Array.init init_k ~f:(fun _ -> [])
  method group_init = 
    (* while Array.for_all ~f:(fun x -> List.length x <> 0) group do *)
    Random.init 100;
    Array.iteri ~f:(fun i  _ -> group.(i) <- []) group;
    for i = 0 to (n-1) do 
      let g = Random.int k in
      group.(g) <- data.(i) :: group.(g)
    done
  (* done  *)
  method group_changed =
    self#get_average = past_avg
  method get_average: data_point array = 
    Array.map group ~f:(fun data_point_list -> 
        Array.map 
          (* array of sum of datapoints for each group : data_point array *)
          (List.fold data_point_list ~init:(Array.init dim ~f:(fun _ -> 0.))  ~f:plus)
          ~f:(fun x -> x /. float_of_int (List.length data_point_list))
      )
  method update_group = 
    if v then self#make_chart ("kmeans" ^ string_of_int update_times ^ ".png") 0 1 else ();
    update_times <- update_times + 1;
    let avg = self#get_average in
    past_avg <- avg;
    self#group_clear;
    let rec update l = 
      match l with
      | h :: t -> 
        (* distances : distance of h <-> group : float array *)
        let distances = Array.map avg ~f:(fun mean -> Float.abs (dist mean h)) in
        (* new_group : index of new(nearest) group *)
        let new_group = Array.foldi distances ~init:0 ~f:(fun i min_pos dist -> 
            if distances.(min_pos) < dist then min_pos
            else i) in
        group.(new_group) <- h:: group.(new_group);
        update t
      | [] -> ()
    in update (Array.to_list data);
    self#group_changed
  method print_status = 
    let string_of_data_point (dp: data_point) = 
      "[" ^ (String.concat ~sep:";" (List.map ~f:string_of_float (Array.to_list dp))) ^ "]"
    in
    print_string ("< trial: " ^ (string_of_int update_times) ^ " >\n");
    let g = group in
    Array.iteri g ~f:(fun i group_data_list -> 
        print_string ("=== group: " ^ (string_of_int i) ^ " ===\n");
        List.iter group_data_list ~f:(fun dp -> print_string (string_of_data_point dp ^ "\n"));
        print_string "======\n\n"
      )
  method make_chart path i j = 
    let series_list = 
      Array.foldi group ~init:[] ~f:(fun gn accum group_list -> 
          (Series.points_xy (List.map group_list ~f:(fun dp -> (dp.(i), dp.(j)))) ~title:("group" ^ (string_of_int gn)) ~color:`Blue)
          :: accum ) in
    let gp = Gp.create () in
    Gp.plot_many gp 
      (* ~range:(Range.XY (-10., 10., -1.5, 1.5))  *)
      ~output:(Gnuplot.Output.create (`Png path))
      ~title: ("kmeans" ^ (string_of_int update_times))
      series_list;
    Gp.close gp
end


let kmeans (data: data_point array) (k: int) (n_norm: int) =
  let n = Array.length data in
  if n < k then raise (Invalid_argument "sample size must be bigger than k")
  else 
    let graph = new kmeans_graph data k n_norm true in
    graph#group_init;
    let res = ref false in
    while not !res do
      res := graph#update_group
    done;
    let _ = Sys.command "convert kmeans*.png result.gif" in
    let _ = Sys.command "rm kmeans*.png" in ()

let () = 
  let data = 
    let data_a = Array.init 20 ~f:(fun _ -> let r = Random.float 2. in let s = Random.float 2. in [|2. -. r; 2. -. s|]) in
    let data_b = Array.init 20 ~f:(fun _ -> let r = Random.float 2. in let s = Random.float 2. in [|4. -. r; 4. -. s|]) in
    let data_c = Array.init 20 ~f:(fun _ -> let r = Random.float 2. in let s = Random.float 2. in [|2. -. r; 4. -. s|]) in
    let data_d = Array.init 20 ~f:(fun _ -> let r = Random.float 2. in let s = Random.float 2. in [|4. -. r; 2. -. s|]) in
    Array.concat [data_a; data_b; data_c; data_d] in
  let k = 3 in
  let n_norm = 2 in
  kmeans data k n_norm;
