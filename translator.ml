let label = ref 0

let label_count () : string =
  let temp = "t" ^ string_of_int !label in
  label := !label + 1;
  temp

let flag = ref 3

let flag_count () : T.label =
  let temp = !flag in
  flag := !flag + 1;
  temp

let rec translate_exp : S.exp -> T.var * T.program
= fun exp ->
  let t = label_count () in
  match exp with
  | NUM i -> (t, [(0, COPYC (t, i))])
  | ADD (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, ADD, t1, t2))])
  | SUB (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, SUB, t1, t2))])
  | MUL (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, MUL, t1, t2))])
  | DIV (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, DIV, t1, t2))])
  | MINUS exp ->
    let (t1, code1) = translate_exp exp in
    (t, code1 @ [(0, ASSIGNU (t, MINUS, t1))])
  | NOT exp ->
    let (t1, code1) = translate_exp exp in
    (t, code1 @ [(0, ASSIGNU (t, NOT, t1))])
  | LT (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, LT, t1, t2))])
  | LE (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, LE, t1, t2))])
  | GT (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, GT, t1, t2))])
  | GE (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, GE, t1, t2))])
  | EQ (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, EQ, t1, t2))])
  | AND (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, AND, t1, t2))])
  | OR (exp1, exp2) ->
    let (t1, code1) = translate_exp exp1 in
    let (t2, code2) = translate_exp exp2 in
    (t, code1 @ code2 @ [(0, ASSIGNV (t, OR, t1, t2))])
  | LV (lv) ->
    match lv with
    | ID id -> (t, [(0, COPY (t, id))])
    | ARR (id, exp) ->
      let (t1, code) = translate_exp exp in
      (t, code @ [(0, LOAD (t, (id, t1)))])

and translate_while : S.exp * S.stmt -> T.program
= fun (exp, stmt) ->
  let (t1, code1) = translate_exp exp in
  let code2 = translate_stmt stmt in
  let flag1 = flag_count () in
  let flag2 = flag_count () in
  [(flag1, T.SKIP)] @ code1 @ [(0, T.CJUMPF (t1, flag2))] @ code2 @ [(0, T.UJUMP flag1); (flag2, T.SKIP)]

and translate_if : S.exp * S.stmt * S.stmt -> T.program
= fun (exp, stmt1, stmt2) ->
  let (t, code) = translate_exp exp in
  let code1 = translate_stmt stmt1 in
  let code2 = translate_stmt stmt2 in
  let flag1 = flag_count () in
  let flag2 = flag_count () in
  let flag3 = flag_count () in
  code @ [(0, T.CJUMP (t, flag1)); (0, T.UJUMP flag2); (flag1, T.SKIP)] @ code1 @ [(0, T.UJUMP flag3); (flag2, T.SKIP)] @ code2 @ [(0, T.UJUMP flag3); (flag3, T.SKIP)]
  
and translate_assign : S.lv * S.exp -> T.program
= fun (lv, exp) ->
  match lv with
  | ID (id) ->
    let (t, code) = translate_exp exp in
    code @ [ (0, COPY (id, t)) ]
  | ARR (id, exp') ->
    let (t', code') = translate_exp exp' in
    let (t, code) = translate_exp exp in
    code' @ code @ [(0, STORE ((id, t'), t))]

and translate_stmt : S.stmt -> T.program
= fun stmt ->
  match stmt with
  | ASSIGN (lv, exp) -> translate_assign (lv, exp)
  | IF (exp, stmt1, stmt2) -> translate_if (exp, stmt1, stmt2)
  | WHILE (exp, stmt) -> translate_while (exp, stmt)
  | DOWHILE (stmt', exp) -> (translate_stmt stmt') @ (translate_stmt stmt)
  | READ (id) -> [(0, READ id)]
  | PRINT (exp) -> 
    let (t1, code1) = translate_exp exp in
    code1 @ [(0, WRITE t1)]
  | BLOCK (block) -> translate_block block

and translate_stmts : S.stmts -> T.program
= fun stmts ->
  match stmts with
  | h::t -> (translate_stmt h) @ (translate_stmts t)
  | _ -> []

and translate_decl : S.decl -> T.program
= fun (typ, id) ->
  match typ with
  | TINT -> [ (0, COPYC(id, 0)) ]
  | TARR i -> [ (0, ALLOC (id, i)) ]
  
and translate_decls : S.decls -> T.program
= fun decls ->
  match decls with
  | h::t -> (translate_decl h) @ (translate_decls t)
  | _ -> []

and translate_block : S.block -> T.program
= fun block ->
  match block with
  | (decls,stmts) -> (translate_decls decls) @ (translate_stmts stmts)

and translate : S.program -> T.program
= fun s ->
  let end_flag = flag_count() in
  translate_block s @ [(end_flag, HALT)]