module test

open hw3

(* 
Some trivial tests are allredy done for you. You should add more tests here.
Although tests won't be graded, make sure that you solution is tested well.
*)


let tests () =
    printfn "=========================================================="
    printfn "only_capitals:"
    printfn "Test 1: %b" (only_capitals(["Aa"; "bb"]) = ["Aa"])
    printfn "Test 2: %b" (only_capitals(["aAa"; "bb"]) = [])
    printfn "Test 3: %b" (only_capitals(["aAa"; "Bb"; "C"]) = ["Bb"; "C"])

    printfn "=========================================================="
    printfn "longest_string1:"
    printfn "Test 1: %b" (longest_string1(["A"; "b"]) = "A")
    printfn "Test 2: %b" (longest_string1(["Aaa"; "b"]) = "Aaa")
    printfn "Test 3: %b" (longest_string1(["Aa"; "bbb"]) = "bbb")
    printfn "Test 4: %b" (longest_string1(["Aaaaa"; "bbbbb"]) = "Aaaaa")

    printfn "=========================================================="
    printfn "longest_string2:"
    printfn "Test 1: %b" (longest_string2(["Aaaaa"; "bbbbb"]) = "bbbbb")

    printfn "=========================================================="
    printfn "longest_string3:"
    printfn "Test 1: %b" (longest_string3(["A"; "b"]) = "A")
    printfn "Test 2: %b" (longest_string3(["Aaa"; "b"]) = "Aaa")
    printfn "Test 3: %b" (longest_string3(["Aa"; "bbb"]) = "bbb")
    printfn "Test 4: %b" (longest_string3(["Aaaaa"; "bbbbb"]) = "Aaaaa")

    printfn "=========================================================="
    printfn "longest_string4:"
    printfn "Test 1: %b" (longest_string4(["Aaaaa"; "bbbbb"]) = "bbbbb")

    printfn "=========================================================="
    printfn "longest_capitallized:"
    printfn "Test 1: %b" (longest_capitalized(["Aaa"; "Bbbb"]) = "Bbbb")
    printfn "Test 2: %b" (longest_capitalized(["Aaaa"; "Bbbb"]) = "Aaaa")
    printfn "Test 3: %b" (longest_capitalized(["aaa"; "bbbb"]) = "")
    printfn "Test 4: %b" (longest_capitalized(["Aaa"; "Bbbb"; "Cccc"]) = "Bbbb")

    printfn "=========================================================="
    printfn "rev_string:"
    printfn "Test 1: %b" (rev_string "samsung" = "gnusmas")

    printfn "=========================================================="
    printfn "first_answer:"
    printfn "Test 1: %b" (first_answer (fun x -> if x > 1 then Some x else None) [3] = 3)
    printfn "Test 2: %b" (try ignore (first_answer (fun x -> if x > 1 then Some x else None) [0] ); false
                          with NoAnswer -> true | _ -> false)

    printfn "=========================================================="
    printfn "all_answers:"
    printfn "Test 1: %b" (all_answers (fun x -> if x > 1 then Some [x] else None) [] = Some [])
    printfn "Test 2: %b" (all_answers (fun x -> if x > 1 then Some [x] else None) [3; 4] = Some [3; 4])
    printfn "Test 3: %b" (all_answers (fun x -> if x > 3 then Some [x] else None) [3; 4] = None)


    // printfn "count_wildcards:"
    // printfn "Test 1: %b" (count_wildcards Wildcard = 1)
    // printfn "Test 2: %b" (count_wildcards (TupleP [Wildcard; ConstP 42]) = 1)
    // printfn "count_wild_and_variable_lengths:"
    // printfn "Test 1: %b" (count_wild_and_variable_lengths Wildcard = 1)
    // printfn "Test 2: %b" (count_wild_and_variable_lengths (TupleP [Wildcard; Variable "xy"]) = 3)
    // printfn "count_some_var:"
    // printfn "Test 1: %b" (count_some_var ("x", Variable "x") = 1)
    // printfn "check_pat:"
    // printfn "Test 1: %b" (check_pat (TupleP [Variable "x"; Variable "x"]) = false)
    // printfn "match_pat:"
    // printfn "Test 1: %b" (match_pat (Tuple [Const 42], TupleP [Wildcard]) = Some [] )
    // printfn "Test 2: %b" (match_pat (Tuple [Const 42], Variable "x") = Some ["x", Tuple [Const 42]] )
    // printfn "first_match:"
    // printfn "Test 1: %b" (first_match (Tuple [Const 42]) [TupleP [Wildcard]] = Some [] )