module D = Domain
module F = Format

type phase = Widen | Narrow

module Make (Memory : D.MEMORY_DOMAIN) = struct
  module Semantics = Semantics.Make (Memory)
  module Table = Domain.Table (Semantics.Memory)
  module Memory = Semantics.Memory

  let initialize llm =
    let init_mem =
      Llvm.fold_left_functions
        (fun mem func ->
          Memory.add
            (D.Location.of_variable func)
            (Memory.Value.FunSet.singleton func |> Memory.Value.of_funset)
            mem)
        Memory.bottom llm
    in
    let entry_point = Utils.find_main llm |> Utils.entry_point in
    Table.add entry_point init_mem Table.empty

  module LabelSet = Set.Make (Domain.Label)
  module LabelMap = Map.Make (Domain.Label)

  let back_edges: LabelSet.t LabelMap.t ref = ref LabelMap.empty
  let fore_edges: LabelSet.t LabelMap.t ref = ref LabelMap.empty

  let prev_labels label =
    try LabelMap.find label !back_edges with Not_found -> LabelSet.empty
  let next_labels label =
    try LabelMap.find label !fore_edges with Not_found -> LabelSet.empty

  let add_edge l1 l2 =
    let prevs = prev_labels l2 |> LabelSet.add l1 in
    let _ = back_edges := LabelMap.add l2 prevs !back_edges in
    let nexts = next_labels l1 |> LabelSet.add l2 in
    fore_edges := LabelMap.add l1 nexts !fore_edges

  let rec run llctx phase worklist (callgraph, table) =
    if Domain.Worklist.is_empty worklist
    then (callgraph, table)
    else
      let label = Domain.Worklist.choose worklist in
      let removed_worklist = Domain.Worklist.remove label worklist in
      let (new_worklist, new_callgraph, new_table) = match phase with
        | Widen ->
            let (new_callgraph, nexts) =
              Semantics.transfer_label llctx
                label callgraph (Table.find label table) in
            let new_table =
              nexts
              |> List.fold_left (fun t (next, new_m) ->
                  add_edge label next;
                  let old_m = Table.find next t in
                  let m = Memory.join new_m old_m in
                  Table.add next m t
              ) Table.empty
              |> Table.merge (fun _ m1 m2 ->
                  let default = Memory.bottom in
                  let v1 = Option.value m1 ~default in
                  let v2 = Option.value m2 ~default in
                  Some (Memory.widen v1 v2)
              ) table in
            let new_worklist = Table.fold (fun l m wl ->
              if Memory.order m (Table.find l table) then wl
              else Domain.Worklist.add l wl
            ) new_table removed_worklist in
            (new_worklist, new_callgraph, new_table)
        | Narrow ->
            let transfer_memory =
              label
              |> prev_labels
              |> LabelSet.elements
              |> List.map (fun l ->
                  Semantics.transfer_label llctx l callgraph (Table.find l table)
              )
              |> List.split
              |> snd
              |> List.flatten
              |> List.filter_map (fun (l, m) ->
                  if l = label then Some m else None
              )
              |> List.fold_left (fun m1 m2 -> Memory.join m1 m2) Memory.bottom in
            let old_memory = Table.find label table in
            let new_memory = Memory.narrow old_memory transfer_memory in
            let new_table = Table.add label new_memory table in
            let new_worklist = Domain.Worklist.union removed_worklist (
              if Memory.order old_memory new_memory
              then LabelSet.empty
              else next_labels label
            ) in
            (new_worklist, callgraph, new_table)
        in
      run llctx phase new_worklist (new_callgraph, new_table)

  let check_instr llctx instr memory =
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_print_num instr || Utils.is_print_ptr instr
      ->
        let arg = Llvm.operand instr 0 in
        let v = Semantics.eval arg memory in
        F.printf "%s @@ %s : %a\n" (Utils.string_of_exp arg)
          (Utils.string_of_location llctx instr)
          Memory.Value.pp v
    | Llvm.Opcode.Call when Utils.is_print_mem instr ->
        F.printf "=== Abstract Memory @@ %s ===\n%a\n"
          (Utils.string_of_location llctx instr)
          Memory.pp memory
    | (Llvm.Opcode.Load as op) | (Llvm.Opcode.Store as op) ->
        let e =
          if op = Llvm.Opcode.Load then Llvm.operand instr 0
          else Llvm.operand instr 1
        in
        let v = Semantics.eval e memory in
        let null =
          D.Location.null |> Memory.Value.LocSet.singleton
          |> Memory.Value.of_locset
        in
        if Memory.Value.order null v then
          let location = Utils.string_of_location llctx instr in
          let exp = Utils.string_of_exp e in
          F.printf "Potential Null Dereference @@ %s, %s = %a\n" location exp
            Memory.Value.pp v
        else ()
    | Llvm.Opcode.SDiv | Llvm.Opcode.UDiv ->
        let e = Llvm.operand instr 1 in
        let v = Semantics.eval e memory in
        let zero =
          Memory.Value.Numerical.of_int 0 |> Memory.Value.of_numerical
        in
        if Memory.Value.order zero v then
          let location = Utils.string_of_location llctx instr in
          let exp = Utils.string_of_exp e in
          F.printf "Potential Division-by-zero @@ %s, %s = %a\n" location exp
            Memory.Value.pp v
        else ()
    | _ -> ()

  let check llctx table =
    Table.iter
      (fun label memory ->
        match label with
        | Llvm.Before instr -> check_instr llctx instr memory
        | _ -> ())
      table
end
