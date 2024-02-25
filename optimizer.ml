(* let optimize : T.program -> T.program =
  fun prog -> raise (Failure "Not implemented") *)




(*control flow graph contruction*)

type label = int
and def = int
and stmt = def * T.linstr
and stmts = stmt list
and block = (label * stmts)
and blocks = block list
and flows = (label * label)  list
and control_flow_graph = {
  blocks : blocks;
  flows : flows
}

let def_count = ref 0
let incr_def_count () =
  let temp = !def_count in
  def_count := !def_count+1;
  temp

let label_count = ref 0
let incr_label_count () =
  let temp = !label_count in
  label_count := !label_count+1;
  temp


let rec build_blocks : T.program -> bool -> (stmts * blocks)
= fun prog isLeader ->
  let is_branch h =
    match snd h with
    | T.UJUMP _ -> true
    | CJUMP (_,_)| CJUMPF (_,_) -> true
    | _ -> false
  in
  match prog with
  | h::t -> (
    let current_def = incr_def_count() in
    let flag = fst h in
    match (not isLeader && flag=0 && flag!=3) with
    | true -> (
      (*if statements, then set next stmt leader*)      
      let n_isLeader = is_branch h in
      (*recursively find blocks*)
      let (prev_stmts, prev_blocks) = build_blocks t n_isLeader in
      let new_stmts = (current_def, h)::prev_stmts in
      (new_stmts, prev_blocks)
    )
    | false -> (
      let current_label = incr_label_count() in
      (*if if statements, then set next stmt leader*)      
      let n_isLeader = is_branch h in
      (*recursively find blocks*)
      let (prev_stmts, prev_blocks) = build_blocks t n_isLeader in
      let new_stmts = (current_def, h)::prev_stmts in
      let new_block = (current_label,new_stmts) in
      ([], new_block::prev_blocks)
    ) 
  )
  | _ -> ([], [])

let rec build_flows : blocks -> blocks -> flows
= fun blocks' blocks ->
  let check_stmt flag stmt = (fst (snd stmt)) = flag in
  let check_block flag block = List.exists (check_stmt flag) (snd block) in
  
  match blocks' with
  | block::t -> (
    (*sequential flow*)
    let flow = 
      match t with
      | h::x -> [(fst block, fst h)]
      | _ -> []
    in
    (*conditional branch flow*)
    let last_instr = snd (List.hd (List.rev (snd block ))) in
    let flow' = 
      match snd last_instr with
      | T.UJUMP l -> (
        let t_block = List.find (check_block l) blocks in
        [(fst block, fst t_block)]
      )
      | CJUMP (_,l) | CJUMPF (_,l) -> (
        let t_block = List.find (check_block l) blocks in
        [(fst block, fst t_block)]
      )
      | _ -> []
    in
    let flow = flow @ flow' in
    (flow @ build_flows t blocks)
  )
  | _ -> []

let build_cfg : T.program -> control_flow_graph
= fun prog ->
  let (_, blocks) = build_blocks prog true in
  let flows = build_flows blocks blocks in
  let cfg = {blocks = blocks; flows = flows} in
  cfg



(* reaching definition analysis part *)

type io = {
  block_label : label;
  bin : stmts;
  bout : stmts
}
and rda = io list



let rec union l1 l2 = 
  match l1 with
  | h::t -> if List.mem h l2 then union t l2 else h::(union t l2)
  | _ -> l2

(*minus l1 l2 : l1 - l2 in logical sense*)
let rec minus l1 l2 =
  match l1 with
  | h::t -> if List.mem h l2 then minus t l2 else h::(minus t l2)
  | _ -> []

let rec update_rda : control_flow_graph -> rda -> rda
= fun cfg rda ->
  (* find defined value*)
  let defined_variable stmt = 
    match snd (snd stmt) with
    | T.ASSIGNV (v,_,_,_) | T.ASSIGNC (v,_,_,_) -> v
    | T.ASSIGNU (v,_,_) -> v
    | COPY (v,_) -> v
    | _ -> "None"
  in

  let rec kill_d stmt =
    let f x stmt =
      let v = defined_variable stmt in
      v = x
    in
    let full_stmts = List.flatten (List.map snd cfg.blocks) in
    let v = defined_variable stmt in
    List.filter (f v) full_stmts
  in

  let rec kill_b stmts =
    match stmts with
    | stmt::t -> union (kill_d stmt) (kill_b t)
    | _ -> []
  in

  let rec gen stmts =
    match stmts with
    | stmt::t -> (
      let v = defined_variable stmt in
      if not (String.equal v "None") then (
        let kills = kill_b t in
        let stmt = minus [stmt] kills in
        stmt @ (gen t)
      ) else gen t
    )
    | _ -> []
  in

  (*update_io = fun io : update in and out set of one label*)
  let update_io io =
    let rec sum_out_set ios = 
      match ios with
      | io'::t -> union io'.bout (sum_out_set t)
      | _ -> []
    in
    (*in*)
    let f1 io' = List.mem (io'.block_label, io.block_label) cfg.flows in
    let t = List.filter f1 rda in
    let in_b = sum_out_set t in

    let _ = print_string "size of in set is " in
    let _ = print_endline (string_of_int (List.length in_b)) in
    
    (*out*)
    let f2 l b = l = (fst b) in
    let stmts = snd (List.find (f2 io.block_label) cfg.blocks) in
    let out_b = union (gen stmts) (minus in_b (kill_b stmts)) in

    let _ = print_string "size of out set is " in
    let _ = print_endline (string_of_int (List.length out_b)) in
    
    {block_label=io.block_label; bin=in_b; bout=out_b}
  in

  let rec is_changed rda' = 
    match rda' with
    | io::t -> (
      let io' = List.find (fun io' -> io'.block_label = io.block_label) rda in
      if ((List.length io.bin)=(List.length io'.bin)) && ((List.length io.bout)=(List.length io'.bout)) then is_changed t else true
    )
    | _ -> false
  in

  (*debug*)
  let _ = print_endline "=-=-=-=-=-" in
  
  let rda' = List.map update_io rda in
  if is_changed rda' then update_rda cfg rda' else rda
  

let build_rda : control_flow_graph -> rda
= fun cfg ->
  (*return initial input and output set*)
  let rec init_rda : blocks -> rda
  = fun blocks ->
    match blocks with
    | b::t -> (
      let bb = {
        block_label = fst b;
        bin = [];
        bout = []
      } in
      bb :: init_rda t
    )
    | _ -> []
  in
  let init_rda = init_rda cfg.blocks in
  let rda = update_rda cfg init_rda in
  rda




  
let optimization_rda : control_flow_graph -> control_flow_graph
= fun cfg ->
  (*reaching definitions analysis : 안해도 될 것 같음*)
  (* let rda = build_rda cfg in *)
  (* copy propagation *)
  (* let cfg' = copy_propagation cfg rda in *)
  let cfg' = cfg in
  cfg'



let rec constant_folding : stmts -> stmts
= fun stmts ->
  let is_copyc instr = 
    match instr with 
    | T.COPYC (x,c) -> if x.[0]='t' then (x,c) else ("None", 0)
    | _ -> ("None", 0)
  in

  let rec change_u_to_c stmts u c = 
    match stmts with
    | stmt::t -> (
      let (def,(label, instr)) = stmt in
      let new_instr = (
        match instr with
        | T.COPY (u,v) -> if v=u then T.COPYC (u,c) else instr
        | T.ASSIGNV (x,bop,y,z) -> if z=u then T.ASSIGNC (x,bop,y,c) else instr
        | _ -> instr
      ) in
      (def, (label, new_instr)) :: (change_u_to_c t u c)
    )
    | _ -> []
  in

  (*check whether u is used in the instruction that cannot be changed*)
  let untouchable stmt u = 
    let instr = snd (snd stmt) in
    match instr with
    | T.ASSIGNV (_,_,y,z) -> if y=u || z=u then true else false
    | T.ASSIGNC (_,_,y,_) -> if y=u then true else false
    | _ -> false
  in

  let rec can_change stmts x = 
    match stmts with
    | stmt::t -> if untouchable stmt x then false else can_change t x
    | _ -> true
  in

  match stmts with 
  | stmt :: t -> (
    (*check whether instruction is COPYC (x, c)*)
    let (x,c) = is_copyc (snd (snd stmt)) in
    if (not (String.equal x "None")) && (not (can_change t x)) then constant_folding (change_u_to_c t x c) else stmt::(constant_folding t)
  )
  | _ -> []
  
let rec opt_cf : blocks -> blocks
= fun blocks ->
  match blocks with
  | block::t -> (
    let optimized_stmts = constant_folding (snd block) in
    let optimized_block = (fst block, optimized_stmts) in
    optimized_block :: opt_cf t
  )
  | _ -> []



let optimize : T.program -> T.program =
  fun prog ->
    let cfg = build_cfg prog in
    let new_blocks = opt_cf cfg.blocks in
    (* let cfg' = {blocks=new_blocks; flows:cfg.flows} in *)
    let prog' = List.map snd (List.flatten (List.map snd new_blocks)) in
    prog'