module F = Which_structure.F
module FParse = Parse.Make(F)
module FGraph = Weighted_graph.Make(F)
module Flower_bound = Lower_bound.Make(F)

let main () =
    let grph = FParse.parse (Sys.argv.(2))
    and out_file = Sys.argv.(3) in
       match Sys.argv.(1) with
           | "naive_tour"       -> let result = FGraph.min_tour grph in
                                       FParse.print_tour result out_file
           | "approximate_tour" -> let result = FGraph.approx_min_tour grph in
                                       FParse.print_tour result out_file
           | "naive_LP"         -> let result = Flower_bound.naive_solve grph in
                                       FParse.print_simplex result out_file
           | "cut_LP"           -> let result = Flower_bound.solve grph in
                                       FParse.print_simplex result out_file
           | _                  -> failwith
                           (Sys.argv.(1) ^ "is not a receivable argument");;

main ()
