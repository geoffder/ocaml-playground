open Base

let ruleset =
  let create_rule (label, num) = fun i ->
    if i % num = 0 then Some label else None in
  List.map ~f:create_rule

let fizzer rules i =
  List.map ~f:(fun f -> f i) rules
  |> List.fold ~init:None ~f:(Option.merge ~f:( ^ ))
  |> function | Some label -> label
              | None -> Int.to_string i

let fizzbuzz rule_list a b =
  let rules = ruleset rule_list in
  List.range a b |> List.map ~f:(fizzer rules)

let result = fizzbuzz [ ("Fizz", 3); ("Buzz", 5) ] 1 31
