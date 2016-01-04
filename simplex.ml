let (/.) : float -> float -> float =
    fun a b ->
        if a /. b = nan then failwith "Pas Content!" else a /. b

let ( *. ) : float -> float -> float =
    fun a b ->
        if b *. a = nan then failwith "Pas Content" else a *. b


module FloatField = struct
    type t = float
    let e_add = 0.
    let e_mul = 1.
    let ( +. ) = ( +. )
    let ( -. ) = ( -. )
    let ( *. ) = ( *. )
    let ( /. ) = ( /. )
    let print = print_float
end

module Int = struct
    type t = int
    let compare = compare
end

module FloatMatrix = Matrix.Make(FloatField)
module IntSet = Set.Make(Int)

open FloatMatrix

exception Unbounded
exception No_solution

type basis = IntSet.t * IntSet.t
type problem = matrix * matrix * matrix
type choice = {c_in:(int*float) list -> int;c_out:(int*float) list -> int}
type rest = matrix * matrix
type final_problem = {minim:bool; obj:matrix; eq:rest; les:rest; mor:rest}

let extracted_c : matrix -> IntSet.t -> matrix =
    fun mat set ->
        IntSet.fold (fun i -> to_left (mat |.| i)) set (ones (nb_l mat) 0)

let extracted_l : matrix -> IntSet.t -> matrix =
    fun mat set ->
        IntSet.fold (fun i -> to_up (mat |.- i)) set (ones 0 (nb_c mat))

let reduced_cost : problem -> basis -> matrix =
    fun (c,a,_) (b,n) ->
        let a_b = extracted_c a b and c_b = extracted_l c b
        and a_n = extracted_c a n and c_n = extracted_l c n in
        let y = linear_solver (trans a_b) c_b in
            c_n |- ((trans a_n) |* y)

let desc_dir : problem -> basis ->  int -> matrix =
    fun (_,a,_) (b,n) j ->
        IntSet.(
        let a_b = extracted_c a b
        and a_n = extracted_c a n in
        let d_b = (-1.) |*.(inv a_b) |* a_n |* (basis_vect (cardinal n) j) 
        and d = zeros (nb_c a) 1
        and k = ref 0 in
            iter (fun i -> (d |= (i,0)) (d_b |. (!k,0));incr k) b;
            k := 0;
            iter (fun i -> (d |= (i,0))(if !k=j then 1. else 0.);incr k) n;
            d)

let change_basis : basis -> int -> int -> basis =
    fun (b,n) i_in i_out ->
        IntSet.(add i_in (remove i_out b),add i_out (remove i_in n))

let rec j_th_littlest : IntSet.t -> int -> int =
    fun b i ->
        let a = IntSet.min_elt b in
            match i with
                |0 -> a
                |_ -> j_th_littlest (IntSet.remove a b) (i-1)

let make_surj : problem -> problem =
    fun (c,a,b) ->
        independant_lines a
        |> List.filter (fun i -> exists (a |.- i) (fun _ _ -> (<) 0.))
        |> fun l -> (List.map ((|.-) a) l,List.map ((|.-) b) l)
        |> fun (l',l'') -> (c,concat_down l',concat_down l'')

let rec solve : problem -> basis -> matrix -> choice -> matrix =
    fun prob bas x ({c_in = choic_in;c_out = choic_out} as choice_f) ->
        let r = reduced_cost prob bas in
        match fold_left r (fun i _ l x ->if x < 0. then (i,x)::l else l) []
        with [] -> x
            | l -> let j = j_th_littlest (snd bas) (choic_in l) in
                   let d = desc_dir prob bas (choic_in l) in
                   match IntSet.fold (fun i (y,l) ->
                       let d_i = d |. (i,0) in
                           if d_i >= 0.
                               then (y,l)
                               else match compare  (-. (x |. (i,0)) /. d_i) y
                       with  0 -> (y,(i,d_i)::l)
                           | 1 -> (y,l)
                           | _ -> (-. (x |. (i,0)) /. d_i,[i,d_i])
                   ) (fst bas) (infinity,[])
                   with (_,[]) -> raise Unbounded
                       |(y,l) -> let k = choic_out l in
                                 solve prob (change_basis bas j k)
                                 (x |+ (y |*. d)) choice_f

let basis_from_point : matrix -> matrix -> basis =
    fun mat' x ->
        let open IntSet in
        let rec construct (b,n) i = function
            | t::q when t=i -> construct (add i b, n) (i+1) q
            | _ when i = nb_c mat' -> (b,n)
            | l                    -> construct (b,add i n) (i+1) l
        in
        let mat = trans mat' in
        fold_left x (fun i _ l y -> if y <> 0. then i::l else l) []
        |> fun l -> List.map ((|.-) mat) l
        |> concat_down
        |> to_up mat
        |> independant_lines
        |> List.filter ((<) (List.length l - 1))
        |> List.map (fun i -> i - List.length l)
        |> (@) l
        |> List.fold_left (fun (l',rem) i ->
           (if rem <= 0 then (l',rem) else (i::l', rem - 1))) ([],nb_c mat)
        |> fst
        |> List.sort Pervasives.compare
        |> construct (empty,empty) 0

(*        let open IntSet in
        let rec incl : int -> basis -> basis =
            fun i (b,n) ->
                if i = nb_l x
                    then (b,n)
                    else incl (i+1)
                  (if (x |. (i,0)) > 0. then (add i b ,n) else (b,add i n))
        and compl : int -> basis -> basis =
            fun i (b,n) ->
                if i = m
                    then (b,n)
                    else compl (i+1)
                       (let a = choose n in (add a b, remove a n))
        in
        let (b,n) = incl 0 (empty,empty) in
            compl (cardinal b) (b,n)*)

let start_point : problem -> choice -> (matrix*basis) =
    fun (c,a,b) choice_f ->
        let c' = init (nb_l a + nb_c a) 1
            (fun i _ -> if i < nb_c a then 0. else 1.)
        and d = init (nb_l a) (nb_l a) (fun i j ->
            if i <> j then 0. else if (b |. (i,0) >= 0.) then 1. else -1.)
        in
        let a'= to_right a d
        and st= to_down (zeros (nb_c a) 1) (d |* b) in
        let sol=solve (c',a',b) (basis_from_point a' st) st choice_f
        in
        let x = extract_block sol (nb_c a) 1 0 0 in
            if a |* x = b
                then (x,(basis_from_point a x))
                else raise No_solution

let trivial_choic : choice =
    let my_f = fun l -> fst (List.fold_left min (max_int,max_float) l) in
        {c_in = my_f; c_out = my_f}

let simplex_w_eq : problem -> choice -> matrix =
    fun prob choice_f ->
        let prob' = make_surj prob in
        let st,bas = start_point prob' choice_f in
             solve prob' bas st choice_f

let simplex : final_problem -> choice -> matrix =
    fun ({eq=a_eq,b_eq;les=a_les,b_les;mor=a_mor,b_mor;_} as pb) choice_f->
        let a_ineq = to_down a_les ((-1.) |*. a_mor)
        and b_ineq = to_down b_les ((-1.) |*. b_mor) in
        let a_ineq_pos = to_right a_ineq ((-1.) |*. a_ineq)
        and a_eq_pos = to_right a_eq ((-1.) |*. a_eq)
        and b_final = to_down b_eq b_ineq in
        let a_ineq_f = to_right a_ineq_pos (id (nb_l a_ineq))
        and a_eq_f = to_right a_eq_pos (zeros (nb_l a_eq_pos) (nb_l a_ineq)) in
        let a_final = to_down a_eq_f a_ineq_f
        and c_int = (if pb.minim then 1. else -1.) |*. (pb.obj) in
        let c_final = concat_down [c_int;(-1.) |*. c_int; zeros (nb_l a_ineq) 1] in
        let x_int = simplex_w_eq (c_final,a_final,b_final) choice_f in
            (extract_block x_int (nb_c a_les) 1 0 0) |-
            (extract_block x_int (nb_c a_les) 1 (nb_c a_les) 0)



(*
First Example:
*)
let a_eq = ones 1 3;;
let b_eq = ones 1 1;;
let a_les = id 3;;
let b_les = ones 3 1;;
let a_mor = id 3;;
let b_mor = zeros 3 1;;

let c = eye 3 1 1 0;;

let prob = {minim=true; obj=c; eq=(a_eq,b_eq); les=(a_les,b_les); mor=(a_mor,b_mor)};;

print (simplex prob trivial_choic);;


(*
Second Example:


let a_eq = ones 1 2;;
let b_eq = ones 1 1;;
let a_les = zeros 0 2;;
let b_les = zeros 0 1;;
let a_mor = id 2;;
let b_mor = zeros 2 1;;

let c = eye 2 1 1 0;;

let prob = {minim=false; obj=c; eq=(a_eq,b_eq); les=(a_les,b_les); mor=(a_mor,b_mor)};;

let yolo = simplex prob trivial_choic;;
Printf.printf "Résultat:\n";;
print yolo;;
*)
