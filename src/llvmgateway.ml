open Ast

type env = { c : Llvm.llcontext;
             m : Llvm.llmodule;
             b : Llvm.llbuilder;

             (* llvalue/llbasicblock binded to Ast.ident*)
             mem    : (Ast.ident * Llvm.llvalue) list;
             labels : (string * Llvm.llbasicblock) list }

let create_env ctx ll_mod bd = { c = ctx; m = ll_mod; b = bd; mem = []
                               ; labels = [] }

let (>>*) m f = f m; m

let env_of_mod ll_mod = 
  let ctx = Llvm.global_context () in 
  create_env ctx ll_mod (Llvm.builder ctx)

let string_of_ident : Ast.ident -> string = function
  | ID_Local i
  | ID_Global i -> i

let string_of_ident_raw : Ast.ident -> string = function
  | ID_Local i  -> i ^ "_l"
  | ID_Global i -> i ^ "_g"

let (%>) f g = fun x -> g (f x)

let print_mem env =
  List.iter (fst %> string_of_ident_raw %> (Core.printf "got: %s\n")) env.mem

let lookup env id = 
  try List.assoc id env.mem
  with e ->
    Core.printf "not found: %s\n" (string_of_ident id);
    print_mem env;
    raise e

let lookup_fn env (id : Ast.ident) : Llvm.llvalue = 
  match id with
  | ID_Local _ -> assert false
  | ID_Global i -> match Llvm.lookup_function i env.m with
                   | Some fn -> fn
                   | _ -> assert false

let label : env -> Ast.ident -> Llvm.llbasicblock =
  fun env id -> List.assoc (string_of_ident id) env.labels

let linkage : Ast.linkage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | LINKAGE_Private               -> Private
  | LINKAGE_Internal              -> Internal
  | LINKAGE_Available_externally  -> Available_externally
  | LINKAGE_Linkonce              -> Link_once
  | LINKAGE_Weak                  -> Weak
  | LINKAGE_Common                -> Common
  | LINKAGE_Appending             -> Appending
  | LINKAGE_Extern_weak           -> External_weak
  | LINKAGE_Linkonce_odr          -> Link_once_odr
  | LINKAGE_Weak_odr              -> Weak_odr
  | LINKAGE_External              -> External

let dll_storage : Ast.dll_storage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | DLLSTORAGE_Dllimport -> Dllimport
  | DLLSTORAGE_Dllexport -> Dllexport

let visibility : Ast.visibility -> Llvm.Visibility.t =
  let open Llvm.Visibility
  in function
  | VISIBILITY_Default   -> Default
  | VISIBILITY_Hidden    -> Hidden
  | VISIBILITY_Protected -> Protected

let cconv : Ast.cconv -> int =
  let open Llvm.CallConv
  in function
  | CC_Ccc    -> c
  | CC_Fastcc -> fast
  | CC_Coldcc -> cold
  | CC_Cc i   -> assert false

let rec ll_type : env -> Ast.raw_type -> Llvm.lltype =
  fun env ->
  let ctx = env.c in
  let open Llvm
  in function
  | TYPE_I i -> begin match i with
                      | 1  -> i1_type ctx
                      | 8  -> i8_type ctx
                      | 16 -> i16_type ctx
                      | 32 -> i32_type ctx
                      | 64 -> i64_type ctx
                      | _  -> integer_type ctx i end
  | TYPE_Pointer t         -> pointer_type (ll_type env t)
  | TYPE_Void              -> void_type ctx
  | TYPE_Half              -> assert false
  | TYPE_Float             -> float_type ctx
  | TYPE_Double            -> double_type ctx
  | TYPE_X86_fp80          -> x86fp80_type ctx
  | TYPE_Fp128             -> fp128_type ctx
  | TYPE_Ppc_fp128         -> ppc_fp128_type ctx
  | TYPE_Label             -> label_type ctx
  | TYPE_Metadata          -> assert false
  | TYPE_X86_mmx           -> x86_mmx_type ctx
  | TYPE_Array (i, t)      -> array_type (ll_type env t) i
  | TYPE_Function (r, a)   ->
     function_type (ll_type env r) (Array.of_list a |> Array.map (ll_type env))
  | TYPE_Struct s          ->
     struct_type ctx (Array.of_list s |> Array.map (ll_type env))
  | TYPE_Packed_struct s   ->
     packed_struct_type ctx (Array.of_list s |> Array.map (ll_type env))
  | TYPE_Opaque            -> assert false
  | TYPE_Vector (i, t)     -> vector_type (ll_type env t) i

let icmp : Ast.icmp -> Llvm.Icmp.t =
  let open Llvm.Icmp
  in function
  | Eq  -> Eq
  | Ne  -> Ne
  | Ugt -> Ugt
  | Uge -> Uge
  | Ult -> Ult
  | Ule -> Ule
  | Sgt -> Sgt
  | Sge -> Sge
  | Slt -> Slt
  | Sle -> Sle

let fcmp : Ast.fcmp -> Llvm.Fcmp.t =
  let open Llvm.Fcmp
  in function
  | False -> False
  | Oeq   -> Oeq
  | Ogt   -> Ogt
  | Oge   -> Oge
  | Olt   -> Olt
  | Ole   -> Ole
  | One   -> One
  | Ord   -> Ord
  | Uno   -> Uno
  | Ueq   -> Ueq
  | Ugt   -> Ugt
  | Uge   -> Uge
  | Ult   -> Ult
  | Ule   -> Ule
  | Une   -> Une
  | True  -> True

let ibinop : Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | Add (_, _) -> build_add
  | Sub (_, _) -> build_sub
  | Mul (_, _) -> build_mul
  | UDiv _     -> build_udiv
  | SDiv _     -> build_sdiv
  | URem       -> build_urem
  | SRem       -> build_srem
  | Shl (_, _) -> build_shl
  | LShr _     -> build_lshr
  | AShr _     -> build_ashr
  | And        -> build_and
  | Or         -> build_or
  | Xor        -> build_xor

let fbinop : Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | FAdd -> build_fadd
  | FSub -> build_fsub
  | FMul -> build_fmul
  | FDiv -> build_fdiv
  | FRem -> build_frem

let conversion_type : Ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue) =
  let open Llvm
  in function
  | Trunc    -> const_trunc
  | Zext     -> const_zext
  | Sext     -> const_sext
  | Fptrunc  -> const_fptrunc
  | Fpext    -> const_fpext
  | Uitofp   -> const_uitofp
  | Sitofp   -> const_sitofp
  | Fptoui   -> const_fptoui
  | Fptosi   -> const_fptosi
  | Inttoptr -> const_inttoptr
  | Ptrtoint -> const_ptrtoint
  | Bitcast  -> const_bitcast

(** FIXME: should be splitted into const/value? *)
let rec value : env -> Ast.raw_type -> Ast.value -> Llvm.llvalue =
  fun env ty ->
  let open Llvm
  in function
  | VALUE_Ident i          -> lookup env i
  | VALUE_Integer i        -> const_int (ll_type env ty) i
  | VALUE_Float f          -> const_float (ll_type env ty) f
  | VALUE_Bool b           -> const_int (Llvm.i1_type env.c) (if b then 1 else 0)
  | VALUE_Null             -> const_null (ll_type env ty)
  | VALUE_Undef            -> undef (ll_type env ty)
  | VALUE_Struct s         ->
     const_struct env.c (Array.of_list s |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Packed_struct s  ->
     const_packed_struct env.c (Array.of_list s
                                |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Array a          ->
     const_array  (ll_type env ty) (Array.of_list a
                                |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Vector v         ->
     const_vector (Array.of_list v |> Array.map (fun (ty, v) -> value env ty v))
  | VALUE_Zero_initializer -> assert false

let rec instr : env -> Ast.instr -> (env * Llvm.llvalue) =
  fun env ->
  let open Llvm in
  function

  | INSTR_IBinop (op, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = ibinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_ICmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = icmp cmp in
     (env, build_icmp cmp v1 v2 "" env.b)

  | INSTR_FBinop (op, _, ty, v1, v2)       ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let op = fbinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_FCmp (cmp, ty, v1, v2)        ->
     let v1 = value env ty v1 in
     let v2 = value env ty v2 in
     let cmp = fcmp cmp in
     (env, build_fcmp cmp v1 v2 "" env.b)

  | INSTR_Conversion (conv, ty, v, ty') ->
     let v = value env ty v in
     let conv = conversion_type conv in
     (env, conv v (ll_type env ty'))

  | INSTR_GetElementPtr ((t, v), tvl)       ->
     let indices = List.map (fun (t,v) -> value env t v) tvl
                   |> Array.of_list in
     (env, build_gep (value env t v) indices "" env.b)

  | INSTR_ExtractElement ((ty, vec), (ty', idx))      ->
     let vec = value env ty vec in
     let idx = value env ty' idx in
     (env, build_extractelement vec idx "" env.b)

  | INSTR_InsertElement ((ty, vec), (ty', el), (ty'', idx))  ->
     let vec = value env ty vec in
     let el = value env ty' el in
     let idx = value env ty'' idx in
     (env, build_insertelement vec el idx "" env.b)

  | INSTR_ShuffleVector ((t, v), (t', v'), (t'', v'')) ->
     let v = value env t v in
     let v' = value env t' v' in
     let v'' = value env t'' v'' in
     (env, build_shufflevector v v' v'' "" env.b)

  | INSTR_ExtractValue ((t, v), idx)         ->
     (* FIXME: llvm api take an int and not a list... *)
     begin match idx with
     | [ idx ] ->
        let v = value env t v in
        (env, build_extractvalue v idx "" env.b)
     | _ -> assert false end

  | INSTR_InsertValue ((t, vec), (t', el), idx)    ->
     (* FIXME: llvm api take an int and not a list... *)
     begin match idx with
     | [ idx ] ->
        let vec = value env t vec in
        let el = value env t' el in
        (env, build_insertvalue vec el idx "" env.b)
     | _ -> assert false end

  | INSTR_Call (tail, (t, i), args)             ->
     let fn = lookup_fn env i in
     let args = Array.of_list args
                |> Array.map (fun (t, v) -> value env t v) in
     (env, build_call fn args "" env.b >>* set_tail_call tail)

  | INSTR_Alloca (ty, nb, _)          ->
     (env,
       match nb with
       | None -> build_alloca (ll_type env ty) "" env.b
       | Some (t, nb) ->
          build_array_alloca (ll_type env ty) (value env t nb) "" env.b )

  | INSTR_Load (_, (t, v), _)                 ->
     (env, build_load (value env t v) "" env.b)

  | INSTR_Phi (t, incoming)                 ->
     let incoming =
       List.map (fun (v, i) -> (value env t v, label env i)) incoming in
     (env, build_phi incoming "" env.b)

  | INSTR_Select ((t, cond), (t', thenv), (t'', elsev))        ->
     let cond = value env t cond in
     let thenv = value env t' thenv in
     let elsev = value env t'' elsev in
     (env, build_select cond thenv elsev "" env.b)

  | INSTR_VAArg                         -> assert false
  | INSTR_LandingPad                    -> assert false

  | INSTR_Store (_, (t, v), (_, p), _) ->
     let v = value env t v in
     let p = lookup env p in
     (env, build_store v p env.b)

  | INSTR_Fence                         -> assert false
  | INSTR_AtomicCmpXchg                 -> assert false
  | INSTR_AtomicRMW                     -> assert false

  | INSTR_Invoke ((t, i1), tvl, (_, i2), (_, i3))   ->
     let args = List.map (fun (t, v) -> value env t v) tvl
                |> Array.of_list in
     let fn = lookup_fn env i1 in
     (env, build_invoke fn args (label env i2) (label env i3) "" env.b)

  | INSTR_Ret (t, v)                    ->
     (env, build_ret (value env t v) env.b)

  | INSTR_Ret_void                      ->
     (env, build_ret_void env.b)

  | INSTR_Br ((t, v), (_, tbb), (_, fbb))   ->
     let cond = value env t v in
     let tbb = label env tbb in
     let fbb = label env fbb in
     (env, build_cond_br cond tbb fbb env.b)

  | INSTR_Br_1 (_, i)                   ->
     (env, build_br (label env i) env.b)

  | INSTR_Switch ((t, v), (t', i), tvtil)        ->
     let case = value env t v in
     let elsebb = label env i in
     let count = List.length tvtil in
     let switch = Llvm.build_switch case elsebb count env.b in
     List.iter (fun ((t, v), (t', i)) ->
                Llvm.add_case switch (value env t v) (label env i))
               tvtil ;
     (env, switch)


  | INSTR_IndirectBr ((t, v), til) ->
    let addr = value env t v in
    let count = List.length til in
    let indirectbr = Llvm.build_indirect_br addr count env.b in
    List.iter
      (fun (_, i) -> Llvm.add_destination indirectbr (label env i)) til;
    (env, indirectbr )

  | INSTR_Resume (t, v)                 ->
     let llv = value env t v in
     (env, build_resume llv env.b)

  | INSTR_Unreachable                   -> (env, build_unreachable env.b)

  | INSTR_Bitcast ((t, v), ty)          ->
    let llv = value env t v in 
    env, build_bitcast llv (ll_type env ty) "" env.b

  | INSTR_Assign (id, inst)             ->
     let (env, llv) = instr env inst in
     let env = { env with mem = (id, llv)::env.mem } in
     env, llv

let global : env -> Ast.global -> env =
  fun env g ->
  let llv = value env g.g_typ (match g.g_value with Some x -> x
                                                  | None -> assert false) in
  let Ast.ID_Global name = g.g_ident in
  let llv = Llvm.define_global name llv env.m in
  {env with mem = (g.g_ident, llv) :: env.mem }

let declaration : env -> Ast.declaration -> env * Llvm.llvalue =
  fun env dc ->
  let name = (string_of_ident dc.dc_name) in
  let fn =  match Llvm.lookup_function name env.m with
    | None -> Llvm.declare_function name (ll_type env dc.dc_type) env.m ;
    | Some fn -> fn in
  (env, fn)

let create_block : env -> Ast.block -> Llvm.llvalue -> env =
  fun env b fn ->
  if List.mem_assoc (fst b) env.labels then assert false ;
  let llb = Llvm.append_block env.c (fst b) fn in
  { env with labels = (fst b, llb) :: env.labels }

let block : env -> Ast.block -> env =
  fun env block ->
  let bb = List.assoc (fst block) env.labels in
  Llvm.position_at_end bb env.b;
  (* process instructions *)
  let env = List.fold_left (fun env i -> instr env i |> fst) env (snd block) in
  env

let definition : env -> Ast.definition -> env =
  fun env df ->
  let (env, fn) = declaration env df.df_prototype in
  (* Do not allow function redefinition. May change? *)
  if Array.length (Llvm.basic_blocks fn) <> 0
  then (
    Core.printf "possibly function redefinition: %s\n" (string_of_ident_raw df.df_prototype.dc_name);
    flush_all ();
    assert false (* env *)
  )
  else begin

  let env =
    lookup_fn env df.df_prototype.dc_name
    |> Llvm.params
    |> Array.mapi (fun i a -> (List.nth df.df_args i, a ))
    |> Array.fold_left (fun env (i, a) ->
                        Llvm.set_value_name (string_of_ident i) a;
                        { env with mem = (i, a) :: env.mem }) env in
  (* First create needed blocks.
   * [block] function will use them when building instructions. *)
  let env =
    List.fold_left (fun env b -> create_block env b fn) env df.df_instrs in
  List.fold_left (fun env bl -> block env bl) env (df.df_instrs)
  end
let ll_module : Ast.modul -> env =
  fun modul ->
  let c = Llvm.global_context () in
  let m = Llvm.create_module c modul.m_name in
  let b = Llvm.builder c in 
  let Ast.TLE_Target target = modul.m_target in
  let Ast.TLE_Datalayout datalayout = modul.m_datalayout in
  Llvm.set_target_triple target m;
  Llvm.set_data_layout datalayout m;
  let env = { c = c; m = m; b = b; mem = []; labels = [] } in
  let env = List.fold_left (fun env g -> global {env with mem=[]} g)
                           env (List.map snd modul.m_globals) in
  let env = List.fold_left (fun env dc -> fst (declaration {env with mem=[]} dc))
                           env (List.map snd modul.m_declarations) in
  let env = List.fold_left (fun env df -> definition {env with mem=[];
                                                               labels=[]} df)
                           env (List.map snd modul.m_definitions) in
  { env with mem = [] ; labels = [] }

let ll_module_in ll_mod md = 
  let c = Llvm.global_context () in
  let m = ll_mod in
  let b = Llvm.builder c in 
  let Ast.TLE_Target target = md.m_target in
  let Ast.TLE_Datalayout datalayout = md.m_datalayout in
  Llvm.set_target_triple target m;
  Llvm.set_data_layout datalayout m;
  let env = { c = c; m = m; b = b; mem = []; labels = [] } in
  let env = List.fold_left (fun env g -> global {env with mem=[]} g)
                           env (List.map snd md.m_globals) in
  let env = List.fold_left (fun env dc -> fst (declaration {env with mem=[]} dc))
                           env (List.map snd md.m_declarations) in
  let env = List.fold_left (fun env df -> definition {env with mem=[];
                                                               labels=[]} df)
                           env (List.map snd md.m_definitions) in
  { env with mem = [] ; labels = [] }