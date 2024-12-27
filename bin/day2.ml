open Printf

let _r = Str.regexp "x"

let rec adjacent_pairs list =
  match list with
  | x :: y :: rest -> (x, y) :: adjacent_pairs (y :: rest)
  | _ -> []



let read_and_solve filename safety_check =
  let file = open_in filename in
  let rec solve_lines answer =
    try
      let line = input_line file in
      let report =
        line
        |> String.split_on_char ' ' (* Split the line by spaces *)
        |> List.filter (fun s -> s <> "")
           (* Remove empty strings (optional, to handle extra spaces) *)
        |> List.map int_of_string
      in
      (* Convert tokens to integers *)
      let report_safe = if safety_check report then 1 else 0 in
      solve_lines (answer + report_safe)
    with
    | End_of_file ->
        close_in file;
        answer
    | _ -> 0
  in
  solve_lines 0

  let is_report_safe_part1 report =
    let diffs = adjacent_pairs report |> List.map (fun (x, y) -> x - y) in
    diffs |> List.map abs |> List.for_all (fun x -> x >= 1 && x <= 3)
    && (diffs |> List.for_all (( > ) 0) || diffs |> List.for_all (( < ) 0))
  
  let is_report_safe_part2 report =
    let rec traverse left right =
      match right with
      | x :: rest ->
          let report_to_try = List.rev left @ rest in
          if is_report_safe_part1 report_to_try then true
          else traverse (x :: left) rest
      | _ -> is_report_safe_part1 left
    in
    traverse [] report

let answer = read_and_solve "input.txt" is_report_safe_part1
let () = printf "Part1 %d\n" answer
let answer = read_and_solve "input.txt" is_report_safe_part2
let () = printf "Part2 %d\n" answer
