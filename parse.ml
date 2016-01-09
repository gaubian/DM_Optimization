module Make (F : Structures.Field) = struct
(*
OSEF = Type Name Comment Capacity Node_Coord
int = Dimension
Explicit -> EDGEWEIGHTFORMAT
*_*D
*)
    module FGraph = Weighted_graph.Make(F)

    let parse_adjacency : in_channel -> FGraph.t =
        fun ic ->
            let open FGraph in
            let rec n = Scanf.fscanf ic "%d" (fun i -> i)
            and add_v_up_to : t -> int -> t =
                fun grph ->
                    function
                        | -1 -> grph
                        | i -> add_v_up_to (add_v grph i) (i-1)
            and parse_edge : t -> t =
                fun grph ->
                    try
                        Scanf.fscanf ic " %d %d " (fun i j -> parse_edge @@
                            add_e grph (i-1) (j-1) (F.scan ic))
                    with _ -> grph
            in
                parse_edge (add_v_up_to empty (n-1))

    let erase_file : string -> unit =
        fun filename ->
           flush @@ open_out filename

    let euclid : (F.t*F.t) -> (F.t*F.t) -> F.t =
        fun (a,b) (c,d) ->
            let open F in
            let x = a -. c and y = b -. d in
                sqrt ((x *. x) +. (y *. y))

    let parse_2D : string -> ((F.t*F.t) -> (F.t*F.t) -> F.t) -> FGraph.t =
        fun w dist ->
            let open F in
            let l= ref [] and ic = open_in w and oc = open_out "last.in" in
            let dim = Scanf.fscanf ic 
                "%s@DIMENSION : %d%s@NODE_COORD_SECTION\n" (fun _ i _-> i) in
                Printf.fprintf oc "%d" dim;
                for i=0 to dim-1 do
                    Scanf.fscanf ic " %d " (fun i ->
                        let a = scan ic in
                            Scanf.fscanf ic " " ();
                        let b = scan ic in
                            List.iter (fun (j,x) -> Printf.fprintf oc
                                "\n %d %d " i j; print oc @@ dist x (a,b)) !l;
                        l := (i,(a,b))::!l);
                done;
            flush oc;
            let ic = open_in "last.in" in
            let res = parse_adjacency ic in
                flush oc;
                res
end
