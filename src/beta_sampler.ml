open Core

module Distribution = struct
  type t =
    { a : int
    ; b : int
    ; rand : Random.State.t
    }
  [@@deriving fields]

  let get_rand () = Util.split_random ()
  let create ~a ~b = { a; b; rand = get_rand () }
  let with_a_b t ~a ~b = { a; b; rand = rand t }
  let empty () = create ~a:1 ~b:1
  let with_a t ~a = with_a_b t ~a ~b:(b t)
  let with_b t ~b = with_a_b t ~a:(a t) ~b
  let incr_a t = with_a t ~a:(a t + 1)
  let incr_b t = with_b t ~b:(b t + 1)

  let incr_a_capped t ~cap =
    match cap with
    | None -> incr_a t
    | Some cap ->
      let { a; b; rand } = t in
      if a + b < cap
      then incr_a t
      else if b = 1
      then t
      else if Float.( <= )
                (Random.State.float rand 1.0)
                (Int.to_float b /. Int.to_float (a + b))
      then with_a_b t ~a:(a + 1) ~b:(b - 1)
      else t
  ;;

  let incr_b_capped t ~cap =
    match cap with
    | None -> incr_b t
    | Some cap ->
      let { a; b; rand } = t in
      if a + b < cap
      then incr_b t
      else if a = 1
      then t
      else if Float.( <= )
                (Random.State.float rand 1.0)
                (Int.to_float a /. Int.to_float (a + b))
      then with_a_b t ~a:(a - 1) ~b:(b + 1)
      else t
  ;;

  (* Sampling algorithm based on code from:
   https://jamesmccaffrey.wordpress.com/2019/06/27/implementing-beta-distribution-sampling-using-c/*)
  let sample_float a b rand =
    let open Float in
    let alpha = a +. b in
    let beta =
      if min a b <=. 1.0
      then max (1.0 /. a) (1.0 /. b)
      else sqrt ((alpha -. 2.0) /. ((2.0 *. a *. b) -. alpha))
    in
    let gamma = a +. (1.0 /. beta) in
    let rec loop tries =
      if Int.( <= ) tries 0
      then (* Beta sampler bug *) 0.0
      else (
        let u1 = Random.State.float rand 1.0 in
        let u2 = Random.State.float rand 1.0 in
        let v = beta *. log (u1 /. (1.0 -. u1)) in
        let w = a *. exp v in
        let tmp = log (alpha /. (b + w)) in
        if (alpha *. tmp) +. (gamma *. v) -. 1.3862944 >=. log (u1 *. u1 *. u2)
        then w /. (b +. w)
        else loop (Int.( - ) tries 1))
    in
    loop 1000
  ;;

  let sample { a; b; rand } = sample_float (Int.to_float a) (Int.to_float b) rand
end

type t = Distribution.t String.Table.t

let create () : t = String.Table.create ()
let add t ~key = String.Table.add t ~key ~data:(Distribution.empty ()) |> ignore

let get t ~key =
  let%map.Option { Distribution.a; b; rand = _ } = String.Table.find t key in
  a, b
;;

let keys t = String.Table.keys t

let get_or_create t ~key =
  String.Table.find_or_add t key ~default:(fun () -> Distribution.empty ())
;;

let update ?cap t ~key ~outcome =
  let existing = get_or_create t ~key in
  let updated =
    match outcome with
    | `Success -> Distribution.incr_a_capped existing ~cap
    | `Failure -> Distribution.incr_b_capped existing ~cap
  in
  String.Table.set t ~key ~data:updated
;;

let sample t ~key =
  let%map.Option distribution = String.Table.find t key in
  Distribution.sample distribution
;;

let choose ?(excluding = []) ?(penalty = 0.0) t =
  String.Table.keys t
  |> List.map ~f:(fun key ->
         let v = Option.value_exn (sample t ~key) in
         let multiplier =
           if List.mem excluding key ~equal:String.equal then penalty else 1.0
         in
         key, v *. multiplier)
  |> List.max_elt ~compare:(fun (_k1, v1) (_k2, v2) -> Float.compare v1 v2)
;;
