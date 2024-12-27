open Printf

let read_two_lists filename = 
  let file = open_in filename in
  let rec read_lines out1 out2 = 
    try 
      let line = input_line file in
      let parsed = line
      |> String.split_on_char ' '       (* Split the line by spaces *)
      |> List.filter (fun s -> s <> "") (* Remove empty strings (optional, to handle extra spaces) *)
      |> List.map int_of_string      in   (* Convert tokens to integers *)
      match parsed with
      | a::b::_ -> read_lines (a::out1) (b::out2)
      | _ -> failwith "must be two elements" 
    with
      | End_of_file -> close_in file; (out1, out2)
      | _ -> ([], [])
  in read_lines [] []

let (left, right) = read_two_lists "input.txt"

let left = List.sort compare left
let right = List.sort compare right

let diff (x, y) = abs (x - y)
let answer = List.combine left right 
  |> List.map diff
  |> List.fold_left (+) 0


let () = printf "part1: %d\n" answer

let answer = let counter x = (right 
                              |> List.filter ( (==) x ) 
                              |> List.length) * x in 
      left |> List.map counter
           |> List.fold_left (+) 0

let () = printf "part2: %d\n" answer
