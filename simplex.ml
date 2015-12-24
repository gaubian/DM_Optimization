open Matrix

module Int =
       struct
         type t = int
         let compare = compare
       end

module IntSet = Set.Make(Int)

exception Unbounded

type basis = IntSet.t * IntSet.t
type problem = matrix * matrix
type choic = ((int * float) list -> int)

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
    fun (_,a) (b,n) j ->
        let a_b = extracted_c a b and a_n = extracted_c a n in
        let d_b = (-1.) |*. (inv a_b) |* a_n |* (basis_vect    
        (IntSet.cardinal n) j) and d = basis_vect (nb_c a) j
        and k = ref 0 in
            IntSet.iter (fun i -> (d |= (!k,0)) (d_b |. (i,0));incr k) b;
            k := 0;
            IntSet.iter (fun i -> (d |= (!k,0))
            (if i=j then 1. else 0.);incr k) n;
            d

let change_basis : basis -> int -> int -> basis =
    fun (b,n) i_in i_out ->
        (IntSet.add i_in (IntSet.remove i_out b),
         IntSet.add i_out (IntSet.remove i_in b))

let rec one_step : problem -> basis -> matrix -> choic -> choic -> matrix =
    fun prob bas x choic_in choic_out ->
        let r = reduced_cost prob bas in
        match fold_left r
        (fun i _ l x -> if x < 0. then (i,x)::l else l)
        []
        with [] -> x
            | l -> let j = choic_in l in
                   let d = desc_dir prob bas j in
                   match fold_left d (fun i _ (y,l) d_i ->
                       match compare  (-. (x |. (i,0)) /. d_i) y
                       with  0 -> (y,(i,d_i)::l)
                           | 1 -> (y,l)
                           | _ -> (-. (x |. (i,0)) /. d_i,[i,d_i])
                   ) (0.,[])
                   with (0.,_) -> raise Unbounded
                       |(y,l) -> let k = choic_out l in
                                 one_step prob (change_basis bas j k)
                                 (x |+ (y |*. d)) choic_in choic_out
