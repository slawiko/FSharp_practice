module test
open hw2;;

exception NoSuchCard

let tests () =
    printfn "=========================================================="
    printfn "all_except_option:"
    printfn "existent string in empty list: %b" (all_except_option("3", []) = None)
    printfn "existent string in list: %b" (all_except_option("3", ["1"; "2"; "3"; "4"; "5"]) = Some (["1"; "2"; "4"; "5"]))
    printfn "nonexistent string in list: %b" (all_except_option("3", ["1"; "2"; "4"; "5"]) = None)

    printfn "=========================================================="
    printfn "get_substitutions1:"
    printfn "normal behavior: %b" (get_substitutions1([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick"; "Freddie"; "F"])
    printfn "only search word in lists: %b" (get_substitutions1([["Fred"]; ["Elizabeth"]; ["Fred"]], "Fred") = [])

    printfn "=========================================================="
    printfn "get_substitutions2:"
    printfn "normal behavior: %b" (get_substitutions2([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick"; "Freddie"; "F"])
    printfn "only search word in lists: %b" (get_substitutions2([["Fred"]; ["Elizabeth"]; ["Fred"]], "Fred") = [])

    printfn "=========================================================="
    printfn "similar_names:"
    printfn "normal behavior: %b" (similar_names([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie"; "Fred"; "F"]], {first="Fred"; middle="W"; last="Smith"}) = 
                            [{first="Fred"; last="Smith"; middle="W"};
                            {first="Fredrick"; last="Smith"; middle="W"};
                            {first="Freddie"; last="Smith"; middle="W"};
                            {first="F"; last="Smith"; middle="W"}])
    printfn "nonexistent name: %b" (similar_names([["Fredrick"]; ["Elizabeth";"Betty"]], {first="Fred"; middle="W"; last="Smith"}) = 
                            [{first="Fred"; last="Smith"; middle="W"}])

    printfn "=========================================================="
    printfn "card_color:"
    printfn "black: %b" (card_color(Clubs, Jack) = Black)
    printfn "red: %b" (card_color(Hearts, Jack) = Red)

    printfn "=========================================================="
    printfn "card_value:"
    printfn "num: %b" (card_value(Clubs, Num 7) = 7)
    printfn "ace: %b" (card_value(Clubs, Ace) = 11)
    printfn "queen: %b" (card_value(Clubs, Queen) = 10)

    printfn "=========================================================="
    printfn "remove_card:"
    printfn "one entry: %b" (remove_card([(Clubs, Jack)], (Clubs, Jack), NoSuchCard) = [])
    printfn "two entries: %b" (remove_card([(Clubs, Jack); (Clubs, Ace)], (Clubs, Jack), NoSuchCard) = [(Clubs, Ace)])
    printfn "two other entries: %b" (remove_card([(Clubs, Ace); (Clubs, Jack)], (Clubs, Jack), NoSuchCard) = [(Clubs, Ace)])
    printfn "no entries: %b" (try ignore (remove_card([], (Clubs, Jack), NoSuchCard)); false
                              with NoSuchCard -> true | _ -> false)

    printfn "=========================================================="
    printfn "all_same_color:"
    printfn "empty list: %b" (all_same_color([]) = true)
    printfn "same color: %b" (all_same_color([(Clubs, Jack); (Clubs, Ace)]) = true)
    printfn "different colors: %b" (all_same_color([(Clubs, Jack); (Hearts, Ace)]) = false)

    printfn "=========================================================="
    printfn "sum_cards:"
    printfn "one card: %b" (sum_cards([(Clubs, Num 7)]) = 7)
    printfn "many card: %b" (sum_cards([(Clubs, Num 7); (Hearts, Queen); (Clubs, Ace)]) = 28)

    printfn "=========================================================="
    printfn "score:"
    printfn "sum less than goal: %b" (score([(Clubs, Num 7)], 7) = 0)
    printfn "sum more than goal: %b" (score([(Clubs, Num 7); (Hearts, Num 5)], 7) = 15)
    printfn "sum more than goal and same color: %b" (score([(Clubs, Num 7); (Clubs, Num 6)], 7) = 9)

    printfn "=========================================================="
    printfn "officiate:"
    printfn "Test1: %b" (officiate([(Hearts, Num 2); (Clubs, Num 4)], [Draw], 15) = 6)
    printfn "Test2: %b" (officiate([(Clubs,Ace); (Spades,Ace); (Clubs,Ace); (Spades,Ace)], [Draw; Draw; Draw; Draw; Draw], 42) = 3)
    printfn "Test3: %b" (try ignore (officiate([(Clubs, Jack); (Spades, Num(8))], [Draw; Discard(Hearts, Jack)],42)); false
                         with IllegalMove -> true | _ -> false)
