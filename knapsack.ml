open Core

type item = {
  weight: float;
  value: float;
}

type subproblem = {
  items: item list;
  total_weight: float;
}

let string_of_problem p = 
  (string_of_int (List.length p.items)) ^ "__" ^ (string_of_float p.total_weight)

let rec knapsack (problem: subproblem) = 
  let tbl = String.Table.create ~size:1024 () in
  match Hashtbl.find tbl (string_of_problem problem) with
  | None -> 
    let a = (match problem.items with
        | [] -> 0.
        | item:: rest -> 
          let item_included = if item.weight <= problem.total_weight 
            then knapsack {items = rest; total_weight = (problem.total_weight -. item.weight)} +. item.value
            else 0. in
          let item_discluded = knapsack {items = rest; total_weight = problem.total_weight} in 
          max item_included item_discluded) in
    let _ = Hashtbl.add tbl (string_of_problem problem) a in a
  | Some a -> a

let test () = 
  let items = [
    {weight = 20.; value = 70.};
    {weight = 30.; value = 80.};
    {weight = 40.; value = 90.};
    {weight = 70.; value = 200.};
  ] in
  knapsack {items = items; total_weight = 60.}
