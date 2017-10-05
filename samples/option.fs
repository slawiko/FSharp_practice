module lect1
(*  Options *)

(* badly named: evaluates to 0 on empty list *)
let rec old_max (xs : int list) =
    if xs.IsEmpty
    then 0
    else if xs.Tail.IsEmpty
    then xs.Head
    else
        let tl_ans = old_max(xs.Tail)
        in
            if xs.Head > tl_ans
            then xs.Head
            else tl_ans


(* better: returns an int option *)
let rec max1 (xs : int list) =
    if xs.IsEmpty
    then None
    else
    let tl_ans = max1(xs.Tail)
    in if tl_ans.IsSome && tl_ans.Value > xs.Head
       then tl_ans
       else Some (xs.Head)


type mylist = int list
let rec max2 (xs : mylist) =
    if xs.IsEmpty
    then None
    else
        let rec max_nonempty (xs : int list) =
            if xs.Tail.IsEmpty (* xs better not be [] *)
            then xs.Head
            else let tl_ans = max_nonempty(xs.Tail)
                 in
                     if xs.Head > tl_ans
                     then xs.Head
                     else tl_ans

        in
            Some(max_nonempty xs)
