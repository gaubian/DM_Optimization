module Make (F : Structures.Field) = struct
   module FMatrix = Matrix.Make(F)
   module IntSet = Set.Make(Ex_structures.Int)

   open FMatrix
   open F

   exception Unbounded
   exception No_solution

   type basis = IntSet.t * IntSet.t
   type problem = matrix * matrix * matrix
   type choice = {c_in:(int*F.t) list -> int;c_out:(int*F.t) list -> int}
   type rest = matrix * matrix
   type t = {minim:bool; obj:matrix; eq:rest; les:rest; mor:rest}

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
         let d_b = (e_add -. e_mul) |*.(inv a_b) |* a_n |* (basis_vect (cardinal n) j) 
         and d = zeros (nb_c a) 1
         and k = ref 0 in
            iter (fun i -> (d |<- (i,0)) (d_b |. (!k,0));incr k) b;
            k := 0;
            iter (fun i -> (d |<- (i,0))(if !k=j then e_mul else e_add);incr k) n;
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
         |> List.filter (fun i -> exists (a |.- i) (fun _ _ -> (<) e_add))
         |> fun l -> (List.map ((|.-) a) l,List.map ((|.-) b) l)
         |> fun (l',l'') -> (c,concat_down l',concat_down l'')

   let rec solve : problem -> basis -> matrix -> choice -> matrix =
      fun prob bas x ({c_in = choic_in;c_out = choic_out} as choice_f) ->
         let r = reduced_cost prob bas in
         match fold_left r (fun i _ l x->if x < e_add then (i,x)::l else l) []
         with [] -> x
            | l -> let j = j_th_littlest (snd bas) (choic_in l) in
                   let d = desc_dir prob bas (choic_in l) in
                   match IntSet.fold (fun i (y,l) ->
                      let d_i = d |. (i,0) in
                         if d_i >=. e_add
                             then (y,l)
                             else match compare  (e_add -. (x |. (i,0)) /. d_i) y
                   with 0 -> (y,(i,d_i)::l)
                      | 1 -> (y,l)
                      | _ -> (e_add -. (x |. (i,0)) /. d_i,[i,d_i])
                   ) (fst bas) (max_field,[])
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
        fold_left x (fun i _ l y -> if y <>. e_add then i::l else l) []
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

   let start_point : problem -> choice -> (matrix*basis) =
      fun (c,a,b) choice_f ->
         let c' = init (nb_l a + nb_c a) 1
            (fun i _ -> if i < nb_c a then e_add else e_mul)
         and d = init (nb_l a) (nb_l a) (fun i j ->
            if i <> j then e_add else if (b |. (i,0) >=. e_add) then e_mul else e_add -. e_mul)
         in
         let a'= to_right a d
         and st= to_down (zeros (nb_c a) 1) (d |* b) in
         let sol=solve (c',a',b) (basis_from_point a' st) st choice_f
         in
         let x = extract_block sol (nb_c a) 1 0 0 in
            if a |* x |= b
               then (x,(basis_from_point a x))
               else raise No_solution

   let trivial_choic : choice =
      let my_f = fun l -> fst (List.fold_left min (max_int,max_field) l) in
         {c_in = my_f; c_out = my_f}

   let simplex_w_eq : problem -> choice -> matrix =
      fun prob choice_f ->
         let prob' = make_surj prob in
         let st,bas = start_point prob' choice_f in
            solve prob' bas st choice_f

   let simplex : t -> choice -> matrix =
      fun ({eq=a_eq,b_eq;les=a_les,b_les;mor=a_mor,b_mor;_} as pb) choice_f->
         let a_ineq = to_down a_les ((e_add -. e_mul) |*. a_mor)
         and b_ineq = to_down b_les ((e_add -. e_mul) |*. b_mor) in
         let a_ineq_pos = to_right a_ineq ((e_add -. e_mul) |*. a_ineq)
         and a_eq_pos = to_right a_eq ((e_add -. e_mul) |*. a_eq)
         and b_final = to_down b_eq b_ineq in
         let a_ineq_f = to_right a_ineq_pos (id (nb_l a_ineq))
         and a_eq_f = to_right a_eq_pos (zeros (nb_l a_eq_pos) (nb_l a_ineq)) in
         let a_final = to_down a_eq_f a_ineq_f
         and c_int = (if pb.minim then e_mul else e_add -. e_mul) |*. (pb.obj) in
         let c_final = concat_down [c_int;(e_add -. e_mul) |*. c_int; zeros (nb_l a_ineq) 1] in
         let x_int = simplex_w_eq (c_final,a_final,b_final) choice_f in
            (extract_block x_int (nb_c a_les) 1 0 0) |-
            (extract_block x_int (nb_c a_les) 1 (nb_c a_les) 0)

   let init : bool -> rest -> rest -> rest -> matrix -> choice -> t =
      fun minim eq les mor obj choice_f ->
         {minim=minim ;obj=obj; eq=eq; les=les; mor=mor}
end
