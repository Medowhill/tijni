module D = Domain
module Analysis = Analysis.Make

let main argv =
  if Array.length argv <> 3 then (
    prerr_endline "Usage: analyzer [input file] [output file]";
    exit 1
  );
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file argv.(1) in
  let llm = Llvm_irreader.parse_ir llctx llmem in
  let fenv = Llvm.fold_left_functions (
    fun fenv f ->
      let name = Llvm.value_name f in
      if
        not (
          Utils.is_llvm_function f ||
          name = "malloc" ||
          name = "free"
        )
      then
        let summary = Analysis.analyze_function f fenv in
        D.FunctionEnv.add name summary fenv
      else
        fenv
  ) D.FunctionEnv.empty llm in
  let oc = open_out argv.(2) in
  Format.set_formatter_out_channel oc;
  Format.printf "%a" D.FunctionEnv.pp fenv;
  close_out oc

let _ = main Sys.argv
