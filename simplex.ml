open Matrix

module Int =
       struct
         type t = int
         let compare = compare
       end

module IntSet = Set.Make(Int)

type basis = IntSet.t * IntSet.t

type problem = matrix * matrix

let from_can_to_stand : problem -> problem =
    fun (c,a) ->
        (concat_down [c;(-1.) |*. c;zeros (nb_l a) 1],
         concat_down [a;(-1.) |*. a;id (nb_l a)])

let extracted_c : matrix -> IntSet.t -> matrix =
    fun mat set ->
        IntSet.fold (fun i -> to_right (mat |.| i)) set (ones (nb_l mat) 0)

let extracted_l : matrix -> IntSet.t -> matrix =
    fun mat set ->
        IntSet.fold (fun i -> to_down (mat |.- i)) set (ones 0 (nb_c mat))

let reduced_cost : problem -> basis -> matrix =
    fun (c,a) (b,n) ->
        let a_b = extracted_c a b and c_b = extracted_l c b
        and a_n = extracted_c a n and c_n = extracted_l c n in
            c_n |- (trans a_n) |* (inv (trans a_b)) |* c_b

let desc_dir : problem -> basis -> int -> matrix =
    fun (c,a) (b,n) j ->
        let a_b = extracted_c a b and a_n = extracted_c a n in
        let d_b = (-1.) |*. (inv a_b) |* a_n |* (basis_vect    
        (IntSet.cardinal n) j) and d = basis_vect (nb_c a) j
        and k = ref 0 i
            IntSet.iter (fun i -> (d |= (!k,0)) (d_b |. (i,0));incr k) b;
            k := 0;
            IntSet.iter (fun i -> (d |= (!k,0))
            (if i=j then 1. else 0.);incr k) n;
            d

let rec one_step : problem -> basis -> matrix -> matrix =
    fun (c,a) (b,n) x ->
        let r = reduced_cost (c,a) (b,n) in
