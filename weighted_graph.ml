module Make(F : Structures.Field) = struct
    module IntMap = Map.Make(Ex_structures.Int)
    module IntSet = Set.Make(Ex_structures.Int)

    type t = (F.t IntMap.t) IntMap.t

    let print : t -> unit =
            IntMap.iter (fun v nei -> Printf.printf "%d\n" v; IntMap.iter (fun u x -> Printf.printf "\t%d\t" u; F.print x; print_newline ()) nei)


    let empty : t =
        IntMap.empty

    let nb_v : t -> int =
        IntMap.cardinal

    let vertices : t -> IntSet.t =
        fun grph ->
            IntMap.bindings grph
            |> List.map fst
            |> IntSet.of_list

    let mem_v : t -> int -> bool =
        fun grph i ->
            IntMap.mem i grph

    let add_v : t -> int -> t =
        fun grph i ->
            IntMap.add i (IntMap.empty) grph

    let adjac : t -> int -> F.t IntMap.t =
        fun grph i ->
            IntMap.find i grph

    let mem_e : t -> int -> int -> bool =
        fun grph i j ->
            IntMap.mem j (adjac grph i)

    let find_e : t -> int -> int -> F.t =
        fun grph i j ->
            IntMap.find j (adjac grph i)

    let add_e : t -> int -> int -> F.t -> t =
       fun grph i j x ->
          let ad_i = adjac grph i and ad_j = adjac grph j in
              IntMap.add i (IntMap.add j x ad_i) grph
              |> IntMap.add j (IntMap.add i x ad_j)

    let del_e : t -> int -> int -> t =
       fun grph i j ->
          let ad_i = adjac grph i and ad_j = adjac grph j in
              IntMap.add i (IntMap.remove j ad_i) grph
              |> IntMap.add j (IntMap.remove i ad_j)

    let del_v : t -> int -> t =
        fun grph i ->
            IntMap.fold (fun j _ gr -> del_e gr i j) (adjac grph i) grph
            |> IntMap.remove i

    let merge : t -> int -> int -> int -> t =
        fun grph i j k ->
            let merge_adj : F.t IntMap.t -> F.t IntMap.t -> F.t IntMap.t =
                fun adj_i adj_j -> 
                    IntMap.merge (fun l a b -> if i=l || j=l then None else match (a,b) with
                       | Some(c),Some(d) -> Some(F.(c +. d))
                       | _,None    -> a
                       | _         -> b) adj_i adj_j
            in
            let adj_i = adjac grph i and adj_j = adjac grph j in
            let adj = merge_adj adj_i adj_j in
                IntMap.fold (fun l x gr -> add_e gr k l x) adj (add_v (del_v (del_v grph j) i) k)

    let choose_v : t -> int =
        fun grph ->
            fst (IntMap.choose grph)

    let rec return_cut : t -> int -> (int * int * IntSet.t * F.t) =
        fun grph u ->
            if nb_v grph = 2
               then (u, choose_v (del_v grph u),
                   IntSet.singleton (choose_v (del_v grph u)),
                   IntMap.fold (fun _ -> F.( +. )) (adjac grph u) F.e_add)
               else let (x,v) = IntMap.fold (fun v x (x',v') ->
            if F.(x >=. x' || x' =. max_field) then (x,v) else (x',v'))
                       (adjac grph u) (F.max_field,u) in
                return_cut (merge grph u v v) v

    let rec min_cut : t -> (IntSet.t * F.t) =
        fun grph ->
            let rec aux : t -> int -> (IntSet.t * F.t) =
                fun grph i ->
                    if nb_v grph < 2
                        then (IntSet.empty, F.max_field)
                        else let (s,t,cut,c) = return_cut grph i in
                             let (cut',c') = aux (merge grph s t s) i in
                                 if F.(c' >=. c)
                                     then (cut, c)
                                     else ((if IntSet.mem s cut'
                                              then IntSet.add t cut'
                                              else cut'),c')
            in aux grph (choose_v grph)

   let min_dists : t -> int -> F.t IntMap.t =
       fun grph i ->
           let rec aux : F.t IntMap.t -> F.t IntMap.t -> F.t IntMap.t =
               fun a nei->
                   try
                       let open IntMap in
                       let (nrst,x) = min_binding nei in
                           if mem nrst grph
                           then aux a (remove nrst nei)
                           else aux (add nrst x a)
                               (fold
                               (fun v y -> add v (F.(x +. y)))
                               (adjac grph nrst) (remove nrst nei))
                   with Not_found -> a
            in aux (IntMap.singleton i F.e_add) (adjac grph i)
           

   let minimum_tour : t -> (int list*F.t) =
       fun grph ->
           let hasht = Hashtbl.create 0 and u = choose_v grph in
               Hashtbl.add hasht ((IntSet.singleton u),u) ([u],F.e_add);
               let rec aux : (IntSet.t * int) -> (int list * F.t) =
                   fun (visit_set,last_visit) ->
                       try Hashtbl.find hasht (visit_set,last_visit)
                       with Not_found ->
                           let rem = IntSet.remove last_visit visit_set in
                           let (a,b) = IntMap.fold (fun v x (l,c) -> 
                               if IntSet.mem v rem && last_visit <> u
                                   then let (l',c') = aux (rem,v) in
                                       F.(if c' +. x >=. c
                                           then (l,c)
                                           else ((last_visit::l'),c' +. x))
                                   else (l,c))
                           (adjac grph last_visit) ([],F.max_field) in
                            Hashtbl.add hasht (visit_set,last_visit) (a,b);
                            (a,b)
               and initi = vertices grph in
                   IntMap.fold (fun v x (l,c) -> let (l',c') = aux (initi,v) in
                       F.(if c' +. x >=. c
                              then (l,c)
                              else (u::l',c' +. x)))
                   (adjac grph u) ([],F.max_field)
end
