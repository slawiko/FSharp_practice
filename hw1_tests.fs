module test
open hw1;;

(* 
Some trivial tests are allredy done for you. You should add more tests here.
Although tests won't be graded, make sure that you solution is tested well.
*)

let tests () =
    printfn "=========================================================="
    printfn "is_older:"
    printfn "Test (2017, 6, 13) older than (2017, 6, 14): %b" (is_older((2017, 6, 13), (2017, 6, 14)) = true)
    printfn "Test (2017, 6, 13) older than (2017, 7, 13): %b" (is_older((2017, 6, 13), (2017, 7, 13)) = true)
    printfn "Test (1996, 6, 13) older than (2017, 6, 13): %b" (is_older((1996, 6, 13), (2017, 6, 13)) = true)
    printfn "Test (2017, 6, 13) not older than (2017, 6, 13): %b" (is_older((2017, 6, 13), (2017, 6, 13)) = false)
    printfn "Test (2017, 6, 13) not older than (2017, 6, 12): %b" (is_older((2017, 6, 13), (2017, 6, 12)) = false)
    printfn "Test (1996, 6, 13) not older than (1995, 6, 13): %b" (is_older((1996, 6, 13), (1995, 6, 13)) = false)

    printfn "=========================================================="
    printfn "number_in_month:"
    printfn "Test empty list: %b" (number_in_month([], 9) = 0)
    printfn "Test 1 of 1 overlap: %b" (number_in_month([(2017, 9, 13)], 9) = 1)
    printfn "Test 4 of 4 overlap: %b" (number_in_month([(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)], 9) = 4)
    printfn "Test 0 of 4 overlap: %b" (number_in_month([(2017, 9, 7);(2017, 9, 8);(2017, 9, 9);(2017, 9, 10)], 8) = 0)
    printfn "Test 2 of 4 overlap: %b" (number_in_month([(2017, 6, 13);(2017, 6, 13);(2017, 7, 13);(2017, 8, 13)], 6) = 2)

    printfn "=========================================================="
    printfn "number_in_months:"
    printfn "Test 1 of 1 overlap: %b" (number_in_months([(2017, 9, 13)], [9]) = 1)
    printfn "Test 5 of 5 overlap: %b" (number_in_months([(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)], [9]) = 5)
    printfn "Test 3 of 6 overlap: %b" (number_in_months([(2017, 7, 13);(2017, 8, 13);(2017, 9, 13);(2017, 10, 13);(2017, 11, 13);(2017, 12, 13)], [7;8;9]) = 3)
    printfn "Test 0 of 6 overlap: %b" (number_in_months([(2017, 7, 13);(2017, 8, 13);(2017, 9, 13);(2017, 10, 13);(2017, 11, 13);(2017, 12, 13)], [1;2;3]) = 0)
    printfn "Test empty dates: %b" (number_in_months([], [9]) = 0)
    printfn "Test empty months: %b" (number_in_months([(2017, 6, 13)], []) = 0)
    printfn "Test empty both: %b" (number_in_months([], []) = 0)

    printfn "=========================================================="
    printfn "dates_in_month:"
    printfn "Test 1 of 1 overlap: %b" (dates_in_month([(2017, 9, 13)], 9) = [(2017, 9, 13)])
    printfn "Test 5 of 5 overlap: %b" (dates_in_month([(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)], 9) = [(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)])
    printfn "Test 2 of 5 overlap: %b" (dates_in_month([(2017, 7, 13);(2017, 7, 14);(2017, 9, 13);(2017, 10, 13);(2017, 11, 13)], 7) = [(2017, 7, 13);(2017, 7, 14)])
    printfn "Test 0 of 5 overlap: %b" (dates_in_month([(2017, 9, 7);(2017, 9, 8);(2017, 9, 9);(2017, 9, 10);(2017, 9, 11)], 8) = [])
    printfn "Test empty list: %b" (dates_in_month([], 9) = [])

    printfn "=========================================================="
    printfn "dates_in_months:"
    printfn "Test 1 of 1 overlap: %b" (dates_in_months([(2017, 9, 13)], [9]) = [(2017, 9, 13)])
    printfn "Test 5 of 5 overlap: %b" (dates_in_months([(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)], [9]) = [(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13);(2017, 9, 13)])
    printfn "Test 0 of 5 overlap: %b" (dates_in_months([(2017, 9, 7);(2017, 9, 8);(2017, 9, 9);(2017, 9, 10);(2017, 9, 11)], [8]) = [])
    printfn "Test 2 of 5 overlap: %b" (dates_in_months([(2017, 5, 13);(2017, 7, 13);(2017, 8, 13);(2017, 10, 13);(2017, 11, 13)], [7;8]) = [(2017, 7, 13);(2017, 8, 13)])
    printfn "Test empty dates: %b" (dates_in_months([], [9]) = [])
    printfn "Test empty months: %b" (dates_in_months([(2017, 6, 13)], []) = [])
    printfn "Test empty both: %b" (dates_in_months([], []) = [])

    printfn "=========================================================="
    printfn "get_nth:"
    printfn "Test get first: %b" (get_nth(["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"], 1) = "1")
    printfn "Test get last: %b" (get_nth(["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"], 10) = "10")
    printfn "Test get middle: %b" (get_nth(["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"], 5) = "5")

    printfn "=========================================================="
    printfn "date_to_string:"
    printfn "Test 1: %b" (date_to_string(2017, 9, 13) = "September 13, 2017")
    printfn "Test 2: %b" (date_to_string(2018, 1, 24) = "January 24, 2018")
    printfn "Test 3: %b" (date_to_string(2004, 11, 15) = "November 15, 2004")
    printfn "Test 4: %b" (date_to_string(2005, 12, 14) = "December 14, 2005")

    printfn "=========================================================="
    printfn "number_before_reaching_sum"
    printfn "Test equal to first: %b" (number_before_reaching_sum(2, [2; 3]) = 0)
    printfn "Test less than first: %b" (number_before_reaching_sum(1, [2; 3]) = 0)
    printfn "Test more than first but less than second: %b" (number_before_reaching_sum(3, [2; 3]) = 1)
    printfn "Test more than all: %b" (number_before_reaching_sum(13, [3; 8 ; 1]) = 3)

    printfn "=========================================================="
    printfn "what_month:"
    printfn "Test first day in year: %b" (what_month(1) = 1)
    printfn "Test last day in year: %b" (what_month(365) = 12)
    printfn "Test first day after treshold: %b" (what_month(32) = 2)
    printfn "Test last day before treshold: %b" (what_month(31) = 1)

    printfn "=========================================================="
    printfn "month_range:"
    printfn "Test days on treshold: %b" (month_range(59, 61) = [2; 3; 3])
    printfn "Test full month: %b" (month_range(335, 365) = [12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12;12])
    printfn "Test days on treshold over month: %b" (month_range(31, 60) = [1;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;2;3])

    printfn "=========================================================="
    printfn "oldest:"
    printfn "Test 1: %b" (oldest([]) = None)
    printfn "Test 2: %b" (oldest([(2017, 9, 13)]) = Some (2017, 9, 13))
    printfn "Test 3: %b" (oldest([(2017, 9, 13); (2017, 8, 12)]) = Some (2017, 8, 12))
    printfn "Test 4: %b" (oldest([(2015, 9, 14); (2017, 8, 12); (2017, 8, 15)]) = Some (2015, 9, 14))
    printfn "Test 5: %b" (oldest([(2017, 8, 12);(2017, 8, 11);(2017, 8, 10)]) = Some (2017, 8, 10))

    printfn "=========================================================="
    printfn "remove_duplicates:"
    printfn "Test all duplicates: %b" (remove_duplicates([9; 9], [9; 9], 1) = [9])
    printfn "Test no duplicates: %b" (remove_duplicates([8; 9], [8; 9], 1) = [8; 9])
    printfn "Test many duplicates: %b" (remove_duplicates([8; 8; 9; 9], [8; 8; 9; 9], 1) = [8; 9])
    printfn "Test empty list: %b" (remove_duplicates([], [], 1) = [])

    printfn "=========================================================="
    printfn "reasonable_date:"
    printfn "Test leap year: %b" (reasonable_date(1900, 2, 28) = true)
    printfn "Test leap year: %b" (reasonable_date(1900, 2, 29) = false)
    printfn "Test everything is fine: %b" (reasonable_date(2017, 12, 31) = true)
    printfn "Test too many days: %b" (reasonable_date(2017, 12, 32) = false)
    printfn "Test too many months: %b" (reasonable_date(2017, 13, 31) = false)
    printfn "Test zero day: %b" (reasonable_date(2017, 12, 0) = false)
    printfn "Test zero month: %b" (reasonable_date(2017, 0, 2) = false)
    printfn "Test zero year: %b" (reasonable_date(0, 12, 2) = false)
