module D = Domain
module F = Format

module Semantics = Semantics.Make
module Memory = D.Memory
module Value = D.Value

module type A = sig
  val analyze_function : Llvm.llvalue -> D.Summary.t
end

module Make : A = struct

  let rec fixpoint f x =
    let fx = f x in
    if Memory.order fx x then
      x
    else
      fixpoint f (Memory.join x fx)

  let is_used_in v mem instr =
    let arg ind = Semantics.eval (Llvm.operand instr ind) mem in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Load -> v = arg 0
    | Llvm.Opcode.Store -> v = arg 1
    | _ -> false

  let is_freed_in v mem instr =
    let arg ind = Semantics.eval (Llvm.operand instr ind) mem in
    match Llvm.instr_opcode instr with
    | Llvm.Opcode.Call when Utils.is_free instr -> v = arg 0
    | _ -> false

  let analyze_function func =
    let instrs =
      List.filter (
        fun i -> not (Utils.is_call i && Utils.is_llvm_intrinsic i)
      ) (Utils.instrs_of_function func) in
    let params = func |> Llvm.params |> Array.to_list in
    let param_values =
      List.map (
        fun p -> (p, p |> D.Location.of_symbol |> Value.of_location)
      ) params in
    let init_memory = List.fold_left (
      fun m (p, v) ->
        Memory.add (D.Location.of_variable p) v m
    ) Memory.bottom param_values in
    let memory = fixpoint (
      fun m ->
        List.fold_left Memory.join m (Semantics.transfer_instrs instrs m)
    ) init_memory in
    let param_summaries = List.map (
      fun (_, v) ->
        (
          List.exists (is_used_in v memory) instrs,
          List.exists (is_freed_in v memory) instrs
        )
    ) param_values in
    let rv = D.Summary.make_rv (
      func
      |> Utils.return_values
      |> List.fold_left (
          fun v r -> Value.join v (Semantics.eval r memory)
      ) Value.bottom
    ) params in
    D.Summary.make param_summaries rv

end
