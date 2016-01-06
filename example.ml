module FSimplex = Simplex.Make(Ex_structures.RationalField)
module FMatrix = Matrix.Make(Ex_structures.RationalField)

open FSimplex
open FMatrix

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
