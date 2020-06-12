module D = Domain
module F = Format

module Semantics = Semantics.Make
module Memory = D.Memory
module Value = D.Value
module Location = D.Location
module Summary = D.Summary
module FEnv = D.FunctionEnv

module type A = sig
  val analyze_function : Llvm.llvalue -> FEnv.t -> Summary.t
end

module Make : A = struct

  let rec fixpoint f x =
    let fx = f x in
    if Memory.order fx x then
      x
    else
      fixpoint f (Memory.join x fx)

  let from_summary v mem fenv instr =
    let callee = Utils.call_callee instr in
    let Summary.Summary (ps, _, _) =
      FEnv.find (Llvm.value_name callee) fenv in
    List.combine (Utils.call_args instr) ps
    |> List.filter (
        fun (a, _) -> v = Semantics.eval a mem
    )
    |> List.split
    |> snd
    |> List.split

  let is_used_in v mem fenv instr =
    let arg ind = Semantics.eval (Llvm.operand instr ind) mem in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Load -> v = arg 0
    | Llvm.Opcode.Store -> v = arg 1
    | Llvm.Opcode.Call
        when not (Utils.is_free instr || Utils.is_malloc instr) ->
          from_summary v mem fenv instr
          |> fst
          |> List.exists (fun x -> x)
    | _ -> false

  let is_freed_in v mem fenv instr =
    let arg ind = Semantics.eval (Llvm.operand instr ind) mem in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_free instr -> v = arg 0
    | Llvm.Opcode.Call when not (Utils.is_malloc instr) ->
        from_summary v mem fenv instr
        |> snd
        |> List.exists (fun x -> x)
    | _ -> false

  let analyze_function func fenv =
    let instrs =
      List.filter (
        fun i -> not (Utils.is_call i && Utils.is_llvm_intrinsic i)
      ) (Utils.instrs_of_function func) in
    let param =
      func
      |> Llvm.params
      |> Array.to_list
      |> List.map (
        fun p -> (p, p |> Location.of_symbol |> Value.of_location)
      ) in
    let init_memory = List.fold_left (
      fun m (p, v) ->
        Memory.add (Location.of_variable p) v m
    ) Memory.bottom param in
    let memory = fixpoint (
      fun m ->
        List.fold_left Memory.join m (Semantics.transfer_instrs instrs m fenv)
    ) init_memory in
    let ps = List.map (
      fun (_, v) ->
        (
          List.exists (is_used_in v memory fenv) instrs,
          List.exists (is_freed_in v memory fenv) instrs
        )
    ) param in
    let rv =
      func
      |> Utils.return_values
      |> List.fold_left (
          fun v r -> Value.join v (Semantics.eval r memory)
      ) Value.bottom in
    Summary.make ps rv memory

end
