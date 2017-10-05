module hw1

let year (a, _, _) = a
let month (_, a, _) = a
let day (_, _, a) = a

let is_older(date1: (int*int*int), date2: (int*int*int)) =
    if year(date1) = year(date2)
    then
        if (month(date1) = month(date2))
        then day(date1) < day(date2)
        else month(date1) < month(date2)
    else year(date1) < year(date2)

let rec number_in_month(dates: (int*int*int) list, m: int) =
    if dates.IsEmpty
    then 0
    else
        let count = 0
        if month(dates.Head) = m
        then number_in_month(dates.Tail, m) + 1
        else number_in_month(dates.Tail, m)

let rec number_in_months(dates: (int*int*int) list, months: int list) =
    if months.IsEmpty
    then 0
    else number_in_months(dates, months.Tail) + number_in_month(dates, months.Head)

let rec dates_in_month(dates: (int*int*int) list, m: int) =
    if dates.IsEmpty
    then []
    else
        if month(dates.Head) = m
        then dates.Head :: dates_in_month(dates.Tail, m)
        else dates_in_month(dates.Tail, m)

let rec dates_in_months(dates: (int*int*int) list, months: int list) = 
    if months.IsEmpty
    then []
    else dates_in_month(dates, months.Head) @ dates_in_months(dates, months.Tail)

let rec get_nth(ls: string list, n: int) =
    if n = 1
    then ls.Head
    else get_nth(ls.Tail, n - 1)

let date_to_string(date: (int*int*int)) =
    let number_month: string list = [
        "January";
        "February";
        "March";
        "April";
        "May";
        "June";
        "July";
        "August";
        "September";
        "October";
        "November";
        "December"
    ]
    sprintf "%s %d, %d" (get_nth(number_month, month(date))) (day(date)) (year(date))

let rec number_before_reaching_sum(sum: int, ls: int list) =
    if ls.IsEmpty
    then 0
    else
        if sum - ls.Head > 0
        then number_before_reaching_sum(sum - ls.Head, ls.Tail) + 1
        else 0

// Helper
let rec calculate_month(num: int, ls: int list) =
    if ls.IsEmpty
    then 0
    else 
        if num - ls.Head > 0
        then calculate_month(num - ls.Head, ls.Tail) + 1
        else 1

let number_month_size: int list = [
    31;
    28;
    31;
    30;
    31;
    30;
    31;
    31;
    30;
    31;
    30;
    31
]
// end:Helper

let what_month(number: int) =
    calculate_month(number, number_month_size)

let rec month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else calculate_month(day1, number_month_size) :: month_range(day1 + 1, day2)

let rec oldest(dates: (int*int*int) list) =
    let rec find_oldest(date: (int*int*int), dates: (int*int*int) list) =
        if dates.IsEmpty
        then date
        else
            if is_older(date, dates.Head)
            then find_oldest(date, dates.Tail)
            else find_oldest(dates.Head, dates.Tail)

    if dates.IsEmpty
    then None
    else Some (find_oldest(dates.Head, dates.Tail))

// Helper
let rec get_nth_int(ls: int list, n: int) =
    if n = 1
    then ls.Head
    else get_nth_int(ls.Tail, n - 1)

let rec remove_duplicates(temp_months: int list, months: int list, index: int) =
    let rec first_index(m: int, months: int list) =
        if months.IsEmpty
        then 0
        else
            if m = months.Head
            then 1
            else first_index(m, months.Tail) + 1
    if temp_months.IsEmpty
    then []
    else
        let fi = first_index(get_nth_int(months, index), months)
        if index = fi
        then
            get_nth_int(months, index) :: remove_duplicates(temp_months.Tail, months, index + 1)
        else 
            remove_duplicates(temp_months.Tail, months, index + 1)
// end:Helper

let number_in_months_2(dates: (int*int*int) list, months: int list) =
    let clean = remove_duplicates(months, months, 1)
    number_in_months(dates, clean)

let dates_in_months_2(dates: (int*int*int) list, months: int list) =
    let clean = remove_duplicates(months, months, 1)
    dates_in_months(dates, clean)

let reasonable_date(date: (int*int*int)) =
    let is_leap_year(y: int) =
        y % 4 = 0 && y % 100 <> 0 || y % 400 = 0
    if year(date) > 0
    then
        if 12 >= month(date) && month(date) >= 1
        then
            if (is_leap_year(year(date)) && month(date) = 2) && (29 >= day(date) && day(date) >= 1)
            then true
            else
                if get_nth_int(number_month_size, month(date)) >= day(date) && day(date) >= 1
                then true
                else false
        else false
    else false
