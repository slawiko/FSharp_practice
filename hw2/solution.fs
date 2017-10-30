module hw2

type Name = {first: string; middle: string; last: string}

type suit = Clubs | Diamonds | Hearts | Spades
type rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

type color = Red | Black
type move = Discard of card | Draw

exception IllegalMove

let rec all_except_option(str: string, ls: string list): string list option = 
    match ls with
        | [] -> None
        | head::tail when head = str -> Some tail
        | head::tail -> 
            match all_except_option(str, tail) with
                | None -> None
                | Some x -> Some (head::x)

let rec get_substitutions1(substitutions: string list list, s: string): string list =
    match substitutions with
        | [] -> []
        | head::tail ->
            match all_except_option(s, head) with
                | None -> get_substitutions1(tail, s)
                | Some x -> x @ get_substitutions1(tail, s)

let rec get_substitutions2(substitutions: string list list, s: string): string list =
    let rec aux(substitutions: string list list, acc: string list) =
        match substitutions with
            | [] -> acc
            | head::tail ->
                match all_except_option(s, head) with
                    | None -> aux(tail, acc)
                    | Some x -> aux(tail, acc @ x)
    in aux(substitutions, [])

let similar_names(substitutions: string list list, name: Name): Name list =
    let rec construct_names(firsts: string list, m: string, l: string) = 
        match firsts with
            | [] -> []
            | head::tail -> {first = head; middle = m; last = l} :: construct_names(tail, m, l)
    let {first = x; middle = y; last = z} = name in
        name :: construct_names(get_substitutions2(substitutions, x), y, z)

let card_color(c: card): color =
    let (s, r) = c
    match s with
        | Spades | Clubs -> Black
        | Hearts | Diamonds -> Red

let card_value(c: card): int =
    let (s, r) = c
    match r with
        | Ace -> 11
        | King | Queen | Jack -> 10
        | Num x -> x

let rec remove_card(cs: card list, c: card, e: System.Exception): card list =
    match cs with
        | [] -> raise e
        | head::tail ->
            if (c = head)
            then tail
            else head::remove_card(tail, c, e)

let rec all_same_color(cs: card list): bool =
    match cs with
        | [] | [_] -> true
        | first::second::tail -> 
            (card_color(first) = card_color(second)) && all_same_color(second::tail)

let sum_cards(cs: card list): int =
    let rec aux(cs1: card list, acc: int) =
        match cs1 with
            | [] -> acc
            | head::tail -> aux(tail, acc + card_value(head))
    in aux(cs, 0)

let score(held_cards: card list, goal: int): int =
    let tmp = 
        let sum = sum_cards(held_cards) in 
            if (sum > goal)
            then 3 * (sum - goal)
            else goal - sum
    let div = all_same_color(held_cards)
    if (div)
    then tmp / 2
    else tmp

let officiate(cards: card list, moves: move list, goal: int): int =
    let rec officiate_rec(cards: card list, moves: move list, held_cards: card list): int =
        match moves with
            | [] -> score(held_cards, goal)
            | head_m::tail_m ->
                match head_m with
                    | Discard c -> officiate_rec(cards, tail_m, remove_card(held_cards, c, IllegalMove))
                    | Draw ->
                        match cards with
                            | [] -> score(held_cards, goal)
                            | head_c::tail_c ->
                                if (sum_cards(head_c::held_cards) > goal)
                                then score(head_c::held_cards, goal)
                                else officiate_rec(tail_c, tail_m, head_c::held_cards)
    in officiate_rec(cards, moves, [])