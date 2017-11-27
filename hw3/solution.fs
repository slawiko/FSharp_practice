module hw3
open System

exception NoAnswer

type pattern = Wildcard | Variable of string | UnitP | ConstP of int | TupleP of pattern list | ConstructorP of string * pattern

type value = Const of int | Unit | Tuple of value list | Constructor of string * value

let rec g f1 f2 p =
    let r = g f1 f2
    in match p with 
        | Wildcard          -> f1 ()
        | Variable x        -> f2 x
        | TupleP ps         -> List.foldBack (fun p i -> (r p) + i) ps 0
        | ConstructorP(_,p) -> r p
        | _                 -> 0

(**** for the challenge problem only ****)

type typ = | Anything | UnitT | IntT | TupleT of typ list | Type of string

let only_capitals = List.filter (fun str -> Char.IsUpper(str, 0))

let longest_string1 ls =
    List.fold (fun acc str -> if (String.length(acc) < String.length(str)) then str else acc) "" ls

let longest_string2 ls =
    longest_string1(List.fold (fun acc str -> str::acc) [] ls)

let longest_string_helper cmp = List.fold (fun acc str -> if (cmp str acc) then str else acc)  ""

let longest_string3 ls = longest_string_helper (fun acc x -> String.length(x) < String.length(acc)) ls

let longest_string4 ls = longest_string_helper (fun acc x -> String.length(x) <= String.length(acc)) ls

let longest_capitalized ls = (only_capitals >> longest_string1) ls

let rev_string str = Array.ofSeq str |> Array.rev |> String

let rec first_answer cb ls =
    match ls with
        | [] -> raise NoAnswer
        | head::tail -> 
            match cb(head) with
                | None -> first_answer cb tail
                | Some x -> x

let rec all_answers cb ls =
    match ls with
        | [] -> Some []
        | head::tail ->
            match cb(head) with
                | None -> None
                | Some x ->
                    match all_answers cb tail with
                        | None -> None
                        | Some y -> Some(x@y)

