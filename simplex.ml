open Matrix

let (/.) a b = if b = 0. then infinity else a /. b

let debug = Printf.printf "COUCOU%d%!\n"

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
            IntSet.iter (fun i -> (d |= (i,0)) (d_b |. (!k,0));incr k) b;
            k := 0;
            IntSet.iter (fun i -> (d |= (i,0))
            (if !k = j then 1. else 0.);incr k) n;
            d


let print_bas : basis -> unit =
    fun (b,n) ->
        let open IntSet in
            print_endline "B";
            iter print_int b;
            print_endline "N";
            iter print_int n;
            print_newline ()

let change_basis : basis -> int -> int -> basis =
    fun (b,n) i_in i_out ->
        (IntSet.add i_in (IntSet.remove i_out b),
         IntSet.add i_out (IntSet.remove i_in n))

let rec j_th_littlest : IntSet.t -> int -> int =
    fun b i ->
        let a = IntSet.min_elt b in
            match i with
                |0 -> a
                |_ -> j_th_littlest (IntSet.remove a b) (i-1)

let rec solve : problem -> basis -> matrix -> choic -> choic -> matrix =
    fun prob bas x choic_in choic_out ->
        print_endline "a";
        print (snd prob);
        Printf.printf "\nc\n";
        print (fst prob);
        Printf.printf "\nx\n";
        print x;
        let r = reduced_cost prob bas in
        Printf.printf "\nr\n";
        print r;
        match fold_left r
        (fun i _ l x -> if x < 0. then (i,x)::l else l) []
        with [] -> x
            | l -> let j = j_th_littlest (snd bas) (choic_in l) in
                   Printf.printf "\nj:%d\n" j;
                   let d = desc_dir prob bas j in
                   Printf.printf "\nd\n";
                   print d;
                   match IntSet.fold (fun i (y,l,flg) ->
                       let d_i = d |. (i,0) in
                       let flg' = flg || d_i < 0. in
                       match compare  (-. (x |. (i,0)) /. d_i) y
                       with  0 -> (y,(i,d_i)::l,flg')
                           | 1 -> (y,l,flg')
                           | _ -> (-. (x |. (i,0)) /. d_i,[i,d_i],flg')
                   ) (fst bas) (infinity,[],false)
                   with (_,_,false) -> raise Unbounded
                       |(y,l,_) -> let k = choic_out l in
                                 solve prob (change_basis bas j k)
                                 (x |+ (y |*. d)) choic_in choic_out

let basis_from_point : matrix -> int -> basis =
    fun x m ->
        let open IntSet in
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
            compl (cardinal b) (b,n)

let start_point : problem -> matrix -> choic -> choic -> (matrix*basis) =
    fun (c,a) b choic_in choic_out ->
        let c' = init (nb_l a + nb_c a) 1
            (fun i _ -> if i < nb_c a then 0. else 1.)
        and d = init (nb_l a) (nb_l a) (fun i j ->
            if i <> j then 0. else if (b |. (i,0) >= 0.) then 1. else -1.)
        in
        let a' = to_right a d and st = to_down (zeros (nb_c a) 1) (d |* b)
        in
        let sol = solve (c',a') (basis_from_point st (nb_l a)) st
        choic_in choic_out in
        let x = extract_block sol (nb_l a) 1 0 0 in
            (x,(basis_from_point x (nb_l a)))

let simplex : problem -> matrix -> choic -> choic -> matrix =
    fun prob b choic_in choic_out ->
        let (st,bas) = start_point prob b choic_in choic_out in
             solve prob bas st choic_in choic_out

let trivial_choic : choic =
    fun l ->
        fst (List.hd l)

let a = ones 1 2;;
let c = eye 2 1 0 0;;
let b = ones 1 1;;
let agjzri = (simplex (c,a) b trivial_choic trivial_choic) in
    Printf.printf "coucouaefa";
    print agjzri;;
