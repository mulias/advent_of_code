open! Base

type crane_spec =
  | CrateMover9000
  | CrateMover9001

type stack = string list [@@deriving yojson]

type stacks = (int * stack) list [@@deriving yojson]

type step =
  { count : int
  ; source : int [@key "from"]
  ; dest : int [@key "to"]
  }
[@@deriving yojson]

type input =
  { stacks : stacks
  ; steps : step list
  }
[@@deriving yojson]

let find_stack id stacks : stack =
  List.Assoc.find_exn stacks id ~equal:Int.equal

let update_stack stack_id stack stacks : stacks =
  List.map stacks ~f:(fun (id, value) ->
      if id = stack_id then (id, stack) else (id, value))

let perform_step stacks step ~crane_spec : stacks =
  let source_stack = find_stack step.source stacks in
  let dest_stack = find_stack step.dest stacks in
  let crates, new_source_stack = List.split_n source_stack step.count in
  let new_dest_stack =
    match crane_spec with
    | CrateMover9000 -> List.append (List.rev crates) dest_stack
    | CrateMover9001 -> List.append crates dest_stack
  in
  stacks
  |> update_stack step.source new_source_stack
  |> update_stack step.dest new_dest_stack

let top_crates stacks : string =
  stacks |> List.map ~f:(fun (_, stack) -> List.hd_exn stack) |> String.concat

let final_state { steps; stacks } ~crane_spec : stacks =
  List.fold steps ~init:stacks ~f:(perform_step ~crane_spec)

let () =
  let input = Yojson.Safe.from_file "input.json" |> input_of_yojson in
  let part_1 = input |> final_state ~crane_spec:CrateMover9000 |> top_crates in
  let part_2 = input |> final_state ~crane_spec:CrateMover9001 |> top_crates in
  assert (String.equal part_1 "GFTNRBZPF") ;
  Stdio.printf "Part 1: %s\n" part_1 ;
  assert (String.equal part_2 "VRQWPDSGP") ;
  Stdio.printf "Part 2: %s\n" part_2
