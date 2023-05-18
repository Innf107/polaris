open Syntax
open Syntax.Typed

module VarMap : module type of Map.Make (Name)
module EnvMap : module type of Map.Make (String)
module RecordVImpl : module type of Multimap.Make (String)

type eval_capabilities = {
  switch : Eio.Switch.t;
  fs : Eio.Fs.dir Eio.Path.t;
  mgr : Eio.Process.mgr;
}

type eval_env = { 
  vars : value VarMap.t
; env_vars : string EnvMap.t
; argv : string list 
; call_trace : loc list
; last_status : int ref
; module_vars : runtime_module VarMap.t
; exceptions : (name list * eval_env * expr) VarMap.t
}

and runtime_module = {
  mod_vars : value VarMap.t
; modules : runtime_module VarMap.t
}

and value =
  | StringV of string
  | NumV of float
  (* The closure environment has to be lazy to
     support recursive lets, since the definition of a recursive function has
     to be stored in its own closure, which also stores its on environment, etc.*)
  | ClosureV of eval_env lazy_t * Typed.pattern list * Typed.expr
  (* PrimOps should be mostly indistinguishable from regular closures.
     The only exception is pretty printing, where primops are printed as
     "<primop: name>" instead of <closure(args)>
     Also, primop names are represented as strings, not names, since
     they are definitely unique and there is no point in keeping
     an additional index *)
  | PrimOpV of string
  | BoolV of bool
  (* Lists are *immutable*. 
     Otherwise we would have to deal with some kind of a 'place' system, which is
     anything but ideal. One should be able to approximate mutable lists
     by mutable references to immutable lists 99% of the time, so this is
     hopefully not going to be an issue. *)
  | ListV of value list
  | TupleV of value array
  | RecordV of (value RecordVImpl.t)
  (* Represents a concurrent thread of execution *)
  | PromiseV of value Eio.Promise.t
  (* Until there are type classes, we need to keep the constructor name
     for pretty printing purposes *)
  | DataConV of name * value
  (* Data constructors can be used like functions *)
  | PartialDataConV of name
  | PartialExceptionV of name * name list * eval_env * expr
  | ExceptionV of name * (name * value) list * exception_trace * string Lazy.t
  | VariantConstructorV of string * value list
  | RefV of value ref

and exception_trace = 
  | NotYetRaised
  | RaisedPreviously of {
    original_trace : loc list;
    reraised : loc list;
  }


type eval_error =
  | PolarisException of name * (name * value) list * exception_trace * string Lazy.t
  | IndexOutOfRange of value list * int * loc list

  | RuntimeError of string * loc list
  | PrimOpArgumentError of string * value list * string * loc list

  | ArgParseError of string

  | NonExhaustiveMatch of value * loc list

  | EnsureFailed of string * loc list

exception EvalError of eval_error

module Value : sig
    type t = value
    val pretty : t -> string
end

val make_eval_env : string list -> eval_env

val isUnitV : value -> bool

val eval_header : eval_env -> header -> eval_env 

val eval_seq_state : cap:eval_capabilities -> [`Expr | `Statement] -> eval_env -> expr list -> value * eval_env


