module lect1
(* Functions taking or producing lists *)

let rec sum_list (xs : int list) =
    if xs.IsEmpty
    then 0
    else xs.Head + sum_list(xs.Tail)

let rec countdown (x : int) =
    if x=0
    then []
    else x :: countdown(x-1)

let rec append (xs : int list, ys : int list) =
    if xs.IsEmpty
    then ys
    else xs.Head :: append(xs.Tail, ys)

(* More functions over lists, here lists of pairs of ints *)

let rec sum_pair_list (xs : (int * int) list) =
    if xs.IsEmpty
    then 0
    else fst (xs.Head) + snd (xs.Head) + sum_pair_list(xs.Tail)

let rec firsts (xs : (int * int) list) =
    if xs.IsEmpty
    then []
    else (fst xs.Head)::(firsts(xs.Tail))

let rec seconds (xs : (int * int) list) =
    if xs.IsEmpty
    then []
    else (snd xs.Head)::(seconds(xs.Tail))

let rec sum_pair_list2 (xs : (int * int) list) =
    (sum_list (firsts xs)) + (sum_list (seconds xs))
