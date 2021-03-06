module Make(F : Structures.Field) = struct
    module IntSet = Set.Make(Ex_structures.Int)
    module IntMap = Map.Make (Ex_structures.Int)
    module IntPairMap = Map.Make(Ex_structures.IntPair)
    module IntSetSet = Set.Make(IntSet)
    module FGraph = Weighted_graph.Make(F)
    module FMatrix = Matrix.Make(F)
    module FSimplex = Simplex.Make(F)

    type corr_edg_int = (int IntPairMap.t * (int*int) IntMap.t)

    (* Subsets of a set *)
    let rec subsets : IntSet.t -> IntSetSet.t =
        fun set ->
            let open IntSet in
            if is_empty set
                then IntSetSet.singleton empty
                else let elt = choose set in
                     let subs = subsets (remove elt set) in
                     let subs' = IntSetSet.fold (fun i -> IntSetSet.add 
                     (add elt i)) subs IntSetSet.empty in
                         IntSetSet.union subs subs'

    (* I must know which matrix's column correspond to which edge *)
    let edges_to_int : FGraph.t -> corr_edg_int =
        fun grph ->
            let open IntMap in
            let compt = ref (-1) in
                fold (fun i -> fold 
                     (fun j _ (e_to_i,i_to_e) ->
                          if i > j then (
                          incr compt;
                          (IntPairMap.add (i,j) !compt
                           (IntPairMap.add (j,i) !compt e_to_i),
                          add !compt (i,j) i_to_e))
                                    else (e_to_i,i_to_e)
                     ))
                grph (IntPairMap.empty,IntMap.empty)

    (* Take a set of vertices, and returns corresponding edge_cut *)
    let cut_to_edges : FGraph.t -> IntSet.t -> (int * int) list =
        fun grph cut ->
            let open IntSet in
            fold (fun i -> (@) (List.map (fun j -> (i,j))
            @@ elements (filter (fun j -> not @@ IntSet.mem j cut) (FGraph.adjac_set grph i)
            ))) cut []

    (* Return the constraints' matrix in the LP *)
    let constr_to_mat:FGraph.t-> corr_edg_int -> IntSetSet.t -> FMatrix.t =
        fun grph (e_to_i,i_to_e) constrs ->
            let open FMatrix in
            let mat = make (IntSetSet.cardinal constrs) (FGraph.nb_e grph) 
                      F.e_add
            and compt = ref 0 in
                IntSetSet.iter (fun set -> List.iter (fun edg ->
                    let i = IntPairMap.find edg e_to_i in
                        (mat |<- (!compt,i)) F.e_mul)
                (cut_to_edges grph set); incr compt) constrs;
                mat

    let degrees_to_mat : FGraph.t-> corr_edg_int -> FMatrix.t =
        fun grph (e_to_i,i_to_e) ->
            let open FMatrix in
            let mat = make (FGraph.nb_v grph) (FGraph.nb_e grph) F.e_add
            and compt = ref (-1) in
                FGraph.iter grph (fun u -> incr compt;IntMap.iter
                                 (fun v _ ->
                                  let i = IntPairMap.find (u,v) e_to_i in
                                      (mat |<- (!compt,i)) F.e_mul));
                mat

   let solve_w_constr:FGraph.t -> IntSetSet.t -> (F.t IntPairMap.t * F.t) =
         fun grph constr ->
             let open FMatrix in
             let (e_to_i,i_to_e) as corr = edges_to_int grph in
             let constr_mat = constr_to_mat grph corr constr in
             let constr_b = make (nb_l constr_mat) 1 F.(e_mul +. e_mul) in
             let degree_mat = degrees_to_mat grph corr in
             let degree_b = make (nb_l degree_mat) 1 F.(e_mul +. e_mul) in
             let obj = init (FGraph.nb_e grph) 1
                 (fun i _ -> let (u,v) = IntMap.find i i_to_e in 
                      FGraph.find_e grph u v)
             in
             let constr_eq = (degree_mat,degree_b)
             and constr_les = (ones 0 (FGraph.nb_e grph), ones 0 1)
             and constr_mor = (constr_mat,constr_b) in
             let problem = FSimplex.init true constr_eq constr_les 
                 constr_mor obj in
             let x = FSimplex.simplex problem FSimplex.lexi_choic in
                 (IntPairMap.map (fun i -> x |. (i,0)) e_to_i, ((trans obj) |* x) |. (0,0)) 

    (* Using solve with all possible subsets *)
    let naive_solve : FGraph.t -> (F.t IntPairMap.t * F.t) =
        fun grph ->
            let vertices = FGraph.vertices grph in
            let u = IntSet.choose vertices in
            let constr_wo_u = subsets (IntSet.remove u vertices) in
            let constr = IntSetSet.remove (IntSet.empty) constr_wo_u in
                solve_w_constr grph constr

    (* One step of our final algorithm *)
    let rec step : FGraph.t -> IntSetSet.t -> (F.t IntPairMap.t * F.t) =
        fun grph constraints ->
            let (w,cost) = solve_w_constr grph constraints in
            let grph' = IntMap.mapi (fun i -> IntMap.mapi (fun j _ -> 
                IntPairMap.find (i,j) w)) grph in
            let (cut,size) = FGraph.min_cut grph' in
                if F.(size >=. e_mul +. e_mul)
                    then (w,cost)
                    else step grph (IntSetSet.add cut constraints)

    (* Final function *)
    let solve : FGraph.t -> (F.t IntPairMap.t * F.t) =
        fun grph ->
            step grph (IntSetSet.empty)
end
