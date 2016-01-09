(*
module FSimplex = Simplex.Make(Ex_structures.RationalField)
module FMatrix = Matrix.Make(Ex_structures.RationalField)

open FSimplex
open FMatrix
*)

(*
let a_eq = ones 1 3;;
let b_eq = ones 1 1;;
let a_les = id 3;;
let b_les = ones 3 1;;
let a_mor = id 3;;
let b_mor = zeros 3 1;;

let c = eye 3 1 1 0;;

let prob = {minim=true; obj=c; eq=(a_eq,b_eq); les=(a_les,b_les); mor=(a_mor,b_mor)};;

print (simplex prob trivial_choic);;
*) 

(*
let a_eq = ones 1 2;;
let b_eq = ones 1 1;;
let a_les = zeros 0 2;;
let b_les = zeros 0 1;;
let a_mor = id 2;;
let b_mor = zeros 2 1;;

let c = eye 2 1 1 0;;

let prob = {minim=true; obj=c; eq=(a_eq,b_eq); les=(a_les,b_les); mor=(a_mor,b_mor)};;

let yolo = simplex prob trivial_choic;;
Printf.printf "Résultat:\n";;
print yolo;;
*)


(*
let grph =
List.fold_left
add_v empty
[0;1;4;5]
;;
let grph' = List.fold_left (fun grp (i,j,x) -> add_e grp i j x)
grph [0,1,2.5;0,4,3.;1,4,2.;1,5,2.;5,4,3.];;
*)
(* List.iter (Printf.printf "%d\n") (fst (minimum_tour grph)) *)
(*
let (l,c) = approx_minimum_tour grph';;
List.iter (Printf.printf "%d\n") l;
Printf.printf "%f\n" c
*)
(*
let grph =
List.fold_left
add_v empty
[0;1;2;3]
;;
let grph' = List.fold_left (fun grp (i,j,x) -> add_e grp i j x)
grph [0,1,1.;1,2,1.;2,3,1.;3,0,1.;0,2,1.];;

let (a,b) = FLower_bound.solve grph';;
Printf.printf "Resultat:\n";;
IntPairMap.iter (fun (a,b) -> Printf.printf "(%d,%d)\t%f\n" a b) a
*)

module FloatGraph = Weighted_graph.Make(Ex_structures.FloatField)
module FLower_bound = Lower_bound.Make(Ex_structures.FloatField)
module FMatrix = Matrix.Make(Ex_structures.FloatField)
module IntPairMap = Map.Make(Ex_structures.IntPair)
module FParse = Parse.Make(Ex_structures.FloatField)

open FloatGraph

let grph = FParse.parse_2D "test" FParse.euclid;;

FloatGraph.iter grph (fun i -> Printf.printf "%d\n" i; IntMap.iter (fun j x -> Printf.printf "\t%d\t" j; Ex_structures.FloatField.print stdout x; print_newline ()))
