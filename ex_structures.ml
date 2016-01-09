module Int = struct
    type t = int
    let compare = compare
end

module FloatField = struct
    type t = float
    let e_add = 0.
    let e_mul = 1.
    let max_field = max_float /. 2.
    let compare = compare
    let ( =. ) a b = (abs_float (a -. b)) < 0.000001
    let ( <>. ) = ( <> )
    let ( >=. ) = ( >= )
    let ( +. ) = ( +. )
    let ( -. ) = ( -. )
    let ( *. ) = ( *. )
    let ( /. ) = ( /. )
    let sqrt = sqrt
    let print oc = Printf.fprintf oc "%f"
    let scan ic = Scanf.fscanf ic "%f" (fun i -> i)
end

module RationalField = struct
    type t = int * int
    let e_add = (0,1)
    let e_mul = (1,1)
    let max_field = (max_int / 2,1)
    let rec gcd (a,b)=if a=0 then b else if b=0 then a else gcd (b,a mod b)
    let simpl (a,b) = let c = gcd (a,b) in (a / c, b / c)
    let ( =. ) a b = (simpl a) = (simpl b)
    let ( <>. ) a b = not (a =. b)
    let ( +. ) (a,b) (c,d) = simpl (a*d + b*c, b * d)
    let ( -. ) a (c,d) = a +. (-c,d)
    let ( *. ) (a,b) (c,d) = simpl (a * c, b * d)
    let ( /. ) a (c,d) = a *. (d,c)
    let ( >=. ) a b = let (c,d) = a -. b in
        (c >= 0 && d >= 0) || (c <= 0 && d <= 0)
    let sqrt (a,b) = (30*a,int_of_float (sqrt (float @@ 900*a*b)))
    let compare a b = if a =. b then 0 else if a >=. b then 1 else -1
    let print oc (a,b) = Printf.fprintf oc "%d/%d" a b
    let scan ic = let a = Scanf.fscanf ic "%f" (fun i -> i) in
         try Scanf.fscanf ic "/%d" (fun b -> simpl (int_of_float a,b))
          with _ -> simpl (int_of_float (Pervasives.( *.) a 1000.),1000)
end

module MakePair (A : Set.OrderedType) (B : Set.OrderedType) =
struct
    type t = (A.t * B.t)
    let compare (a,b) (c,d) = match A.compare a c with
        | 0 -> B.compare b d
        | x -> x
end

module IntPair = MakePair(Int)(Int)
