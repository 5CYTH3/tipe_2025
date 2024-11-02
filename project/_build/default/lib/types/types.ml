type t =
    (* Primitives *)
    | Bool
    | Str
    | Int
    | TVar of string
    (* Function *)
    | Abs of t * t
    (* Scheme *)
    | Forall of string * t 
    [@@deriving show];;

module TypeMap = Map.Make(String)

let show_typemap tm =
    "{\n" ^ (TypeMap.fold (fun k d acc -> (Printf.sprintf "  %s: %s\n" k (show d)) ^ acc ) tm "") ^ "}"

type subst = t TypeMap.t (* // *)

let rec apply_subst (s: subst) (t: t) =
    match t with
    | TVar v -> begin
        match TypeMap.find_opt v s with (* If type is not found, works like Id *)
        | Some x -> x
        | None -> t
    end
    | Abs (arg, ret) -> Abs (apply_subst s arg, apply_subst s ret)
    | Forall (v, t) -> Forall (v, apply_subst (TypeMap.remove v s) t)
    | t -> t
;;

let apply_subst_to_env subst env = TypeMap.map (fun t -> apply_subst subst t) env;;

let ( *&* ) s1 s2 =
    TypeMap.union (fun _ t _ -> Some t) (TypeMap.map (apply_subst s1) s2) s1
;;

type env = t TypeMap.t;; (* String : Type *)

let extend_env env k t = TypeMap.add k t env;; 

let apply_env (env: env) k: t =
    try TypeMap.find k env
    with Not_found -> failwith ("Var not in scope : " ^ k)
;;

module Ftv = Set.Make (String);;

type ftvs = Ftv.t


let rec free_tvs t: ftvs =
    match t with
    | Int | Bool | Str -> Ftv.empty
    | TVar v -> Ftv.singleton v
    | Abs (t1, t2) -> Ftv.union (free_tvs t1) (free_tvs t2)
    | Forall (v, t) -> Ftv.remove v (free_tvs t)
;;

(* For now, returns a list but I don't like it. *)
let free_tvs_env env =
    TypeMap.fold (fun _ t acc -> Ftv.union (free_tvs t) acc) env Ftv.empty
;;

exception TypeError of string

let rec unify t1 t2: subst =
    match t1, t2 with
    | Bool, Bool | Int, Int | Str, Str -> TypeMap.empty
    | TVar v1, TVar v2 when v1 = v2 -> TypeMap.empty
    | TVar v, t | t, TVar v ->
        if Ftv.mem v (free_tvs t) then
            raise (TypeError ("Occurs check failed for variable " ^ v))
        else
            TypeMap.singleton v t
    | Abs (t1a, t1b), Abs (t2a, t2b) ->
        let s1 = unify t1a t2a in
        let s2 = unify (apply_subst s1 t1b) (apply_subst s1 t2b) in
        s2 *&* s1
    | Forall (v1, t1), Forall (v2, t2) ->
        let t2' = apply_subst (TypeMap.singleton v2 (TVar v1)) t2 in
        unify t1 t2'
    | _ -> raise (TypeError "Cannot unify types")
;;

let generalize env t =
    let env_fv = free_tvs_env env in
    let ftvs_of_t = free_tvs t in
    let vars_to_generalize = Ftv.filter (fun v -> not (Ftv.mem v env_fv)) ftvs_of_t in
    match Ftv.elements vars_to_generalize with
    | [] -> t
    | vars -> List.fold_right (fun v acc -> Forall (v, acc)) vars t
;;

let fresh_tv =
  let counter = ref 0 in
  fun () ->
    let v = "t" ^ string_of_int !counter in
    incr counter;
    TVar v
;;

let rec instantiate t =
    match t with
    | Forall (v, t) -> let fresh_var = fresh_tv () in instantiate (apply_subst (TypeMap.singleton v fresh_var) t)
    | _ -> t
;;


