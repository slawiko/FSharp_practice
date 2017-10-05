module lect1
(*Let expressions*)

let silly1 (z : int) =
    let x = if z > 0 then z else 34
    let y = x+z+9
        in if x > y then x*2 else y*y
let x = 9
let silly2 () =
    let x = 1

        in (let x = 2 in x+1) + (let y = x+2 in y+1)
