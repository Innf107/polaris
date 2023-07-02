(** Record definitions and common implementations for emulating type classes
    using first class modules *)

(* laws:
    associativity:  append a (append b c) = append (append a b) c
    left identity:  append empty a        = a
    right identity: append a empty        = a
*)
module type Monoid = sig
  type t

  val append : t -> t -> t
  val empty : t
end

let mconcat : type a. (module Monoid with type t = a) -> a list -> a =
 fun monoid ->
  let (module M) = monoid in
  List.fold_left M.append M.empty

let fold : type m. (module Monoid with type t = m) -> ('b -> m) -> 'b list -> m
    =
 fun monoid f list -> mconcat monoid (List.map f list)

let monoid_list : type a. (module Monoid with type t = a list) =
  let module ListM = struct
    include List

    type t = a list

    let empty = []
  end in
  (module ListM)

let monoid_difflist : type a. (module Monoid with type t = a Difflist.t) =
  let module DifflistM = struct
    include Difflist

    type t = a Difflist.t
  end in
  (module DifflistM)

let monoid_set :
    type a. (module Set.S with type t = a) -> (module Monoid with type t = a) =
 fun set_impl ->
  let (module SetImpl) = set_impl in
  let module SetM = struct
    include SetImpl

    let append = SetImpl.union
  end in
  (module SetM)

let monoid_or : (module Monoid with type t = bool) =
  let module BoolM = struct
    type t = bool

    let append = ( || )
    let empty = false
  end in
  (module BoolM)

module type Monad = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
end

module MonadUtil (M : Monad) = struct
  let rec traverse : ('a -> 'b M.t) -> 'a list -> 'b list M.t =
    let ( let* ) = M.bind in
    fun f list ->
      match list with
      | [] -> M.pure []
      | a :: rest ->
          let* b = f a in
          let* rest = traverse f rest in
          M.pure (b :: rest)
end

module MonadOption : Monad = struct
  type 'a t = 'a option

  let bind = Option.bind
  let pure = Option.some
end

module MonadList = MonadUtil (struct
  type 'a t = 'a list

  let bind x cont = List.concat_map cont x
  let pure x = [ x ]
end)

module Option = MonadUtil (MonadOption)
