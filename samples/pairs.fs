module lect1
(* pairs *)

let swap (pr : int * bool) =
    (snd pr, fst pr)

let sum_two_pairs (pr1 : int*int, pr2 : int*int) =
    (fst pr1) + (snd pr1) + (fst pr2) + (snd pr2)

let div_mod (x : int, y : int) =
    (x / y, x % y)

let sort_pair (pr : int*int) =
    if (fst pr) < (snd pr)
    then pr
    else (snd pr, fst pr)

(* nested pairs *)

let x1 = (7,(true,9)) (* int * (bool*int) *)

let x2 = fst (snd x1) (* bool *)

let x3 = (snd x1) (* bool*int *)

let x4 = ((3,5),((4,8),(0,0))) (* (int * int) * ((int * int) * (int * int)) *)
