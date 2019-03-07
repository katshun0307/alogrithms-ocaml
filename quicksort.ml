let sample_array: int array =  [|1;5;1;6;7;4;9;10;14|] 

(* let rec quicksort arr s t: int =
   let len = Array.length !arr in
   let pivot = !arr.(0) in
   let new_arr = Array.create (t-s) 0 in
   for i = s to t do
    if  *)

let cut_arr arr s t = 
  let len = t - s + 1 in
  let out_arr = Array.create len 0 in
  for i = 0 to len - 1 do
    out_arr.(i) <- arr.(s + i)
  done;
  out_arr

let get_val arr i = 
  try
    arr.(i)
  with _ -> 1000000

let rec mergesort arr = 
  let len = Array.length arr in
  if len <= 1 then arr
  else
    (* split *)
    let pivot = len / 2 in
    let first_half = mergesort (cut_arr arr 0 (pivot - 1)) in
    let second_half = mergesort (cut_arr arr pivot (len-1)) in
    let out_arr = Array.create len 0 in
    let fi = ref 0 in
    let si = ref 0 in
    let i = ref 0 in
    while not(!fi = pivot - 1 && !si = len - 1 || !i > len - 1) do 
      let fv = get_val first_half (!fi) in 
      let sv = get_val second_half (!si) in
      if fv < sv then
        (out_arr.(!i) <- fv; 
         fi := !fi + 1)
      else 
        (out_arr.(!i) <- sv;
         si := !si + 1);
      i := !i + 1;
    done;
    out_arr
