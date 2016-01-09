module Make (F : Structures.Field) = struct
    module FGraph = Weighted_graph.Make(F)
    module IntPairMap = Map.Make(Ex_structures.IntPair)

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

    let parse_until : in_channel -> string -> unit =
        fun ic w ->
            let flag = ref false in
            while not !flag do
                 Scanf.fscanf ic "%s@\n" (fun i -> flag := i = w);
            done

    let parse_2D : in_channel -> int -> FGraph.t =
        fun ic dim ->
            let open F in
            let l= ref [] and oc = open_out "last.in" in
                parse_until "NODE_COORD_SECTION";
                Printf.fprintf oc "%d" dim;
                for i=0 to dim-1 do
                    Scanf.fscanf ic " %d " (fun i ->
                        let a = scan ic in
                            Scanf.fscanf ic " " ();
                        let b = scan ic in
                            List.iter (fun (j,x) -> Printf.fprintf oc
                                "\n %d %d " i j; print oc @@ euclid x (a,b)) !l;
                        l := (i,(a,b))::!l);
                done;
            flush oc;
            let ic = open_in "last.in" in
            let res = parse_adjacency ic in
                flush oc;
                res

    let parse_lower : in_channel -> int -> FGraph.t =
        fun ic dim ->
            let oc = open_out "last.in" in
                parse_until ic "EDGE_WEIGHT_SECTION";
                Printf.fprintf oc "%d" dim;
                for i=1 to dim do
                for j=1 to i-1 do
                    Scanf.fscanf ic " " ();
                    let x = F.scan ic in
                        Printf.fprintf oc "\n%d %d " i j;
                        F.print oc x;
                done;
                Scanf.fscanf ic " " ();
                let _ = F.scan ic in ()
                done;
            flush oc;
            let ic = open_in "last.in" in
            let res = parse_adjacency ic in
                flush oc;
                res

    let parse_upper : in_channel -> int -> FGraph.t =
        fun ic dim ->
            let oc = open_out "last.in" in
                parse_until ic "EDGE_WEIGHT_SECTION";
                Printf.fprintf oc "%d" dim;
                for i=1 to dim do
                for j=i+1 to dim do
                    Scanf.fscanf ic " " ();
                    let x = F.scan ic in
                        Printf.fprintf oc "\n%d %d " i j;
                        F.print oc x;
                done;
                done;
            flush oc;
            let ic = open_in "last.in" in
            let res = parse_adjacency ic in
                flush oc;
                res

    let parse : string -> FGraph.t =
        fun w ->
            let ic=open_in w in
                let dim = Scanf.fscanf ic 
                    "%s@DIMENSION : %d" (fun _ i-> i) in
                try
                  let _ = Scanf.fscanf ic
                    " EDGE_WEIGHT_TYPE: EXPLICIT" () in
                    try
                      let _ = Scanf.fscanf ic
                        " EDGE_WEIGHT_FORMAT: LOWER_DIAG" () in
                          parse_lower ic dim
                    with Scanf.Scan_failure(_) -> parse_upper ic dim
                with Scanf.Scan_failure(_) -> parse_2D ic dim


    let print_tour : (int list * F.t) -> string -> unit =
        fun (l,x) w ->
            let oc = open_out w in
            Printf.fprintf oc "Length: ";
            F.print oc x;
            Printf.fprintf oc "\nOrdered list of vertices:";
            List.iter (Printf.fprintf oc "\n%d") l;
            flush oc

    let print_simplex : (F.t IntPairMap.t * F.t) -> string -> unit =
        fun (map,x) w ->
            let oc = open_out w in
            Printf.fprintf oc "Length: ";
            F.print oc x;
            Printf.fprintf oc "\nList of edges with their weight:";
            IntPairMap.iter (fun (i,j) x -> Printf.fprintf oc
                "\n(%d,%d) = " i j; F.print oc x) map;
            flush oc

end
