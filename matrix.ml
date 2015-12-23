type matrix = int * int * float array array

exception Finished of int

let init : int -> int -> (int -> int -> float) -> matrix =
    fun m n f ->
        (m,n,Array.init m @@ fun i -> Array.init n (f i))

let make : int -> int -> float -> matrix =
    fun m n el ->
        init m n @@ fun i j -> el

let nb_l : matrix -> int =
    fun (m,_,_) ->
        m

let nb_c : matrix -> int =
    fun (_,n,_) ->
        n

let (|.) : matrix -> (int * int) -> float =
    fun (_,_,tab) (i,j) ->
        tab.(i).(j)

let (|.-) : matrix -> int -> matrix =
    fun mat i ->
        init 1 (nb_c mat) @@ fun _ j -> mat |. (i,j)

let (|.|) : matrix -> int -> matrix =
    fun mat j ->
        init (nb_l mat) 1 @@ fun i _ -> mat |. (i,j)

let (|=) : matrix -> (int * int) -> float -> unit =
    fun (_,_,tab) (i,j) el ->
        tab.(i).(j) <- el

let map : matrix -> (int -> int -> float -> float) -> matrix =
    fun ((m,n,_) as mat) f ->
        init m n (fun i j -> f i j (mat |. (i,j)))

let iter : matrix -> (int -> int -> float -> unit) -> unit =
    fun ((m,n,_) as mat) f ->
        ignore @@ map mat (fun i j x -> f i j x; 0.)

let print : matrix -> unit =
    fun mat ->
        iter mat (fun _ j x -> print_string @@
            ((if j = 0 then "\n" else "\t")^(string_of_float x)))

let trans : matrix -> matrix =
    fun mat ->
        init (nb_c mat) (nb_l mat) @@ fun i j -> mat |. (j,i)

let copy : matrix -> matrix =
    fun mat ->
        init (nb_l mat) (nb_c mat) @@ fun i j -> mat |. (i,j)

let extract_block : matrix -> int -> int -> int -> int -> matrix =
    fun mat m n i j ->
        init m n (fun k l -> mat |. (i+k,j+l))

let of_float : float -> matrix =
    fun x ->
        (1,1,[|[|x|]|])

let (|+) : matrix -> matrix -> matrix =
    fun mat mat' ->
        map mat (fun i j -> (+.) (mat' |. (i,j)))

let (|-) : matrix -> matrix -> matrix =
    fun mat mat' ->
        map mat' (fun i j -> (-.) (mat |. (i,j)))

let (|* ) : matrix -> matrix -> matrix =
    fun ((m,n,tab) as mat) ((o,p,_) as mat') ->
        init m p (fun i j -> Array.fold_left (+.) 0.
            (Array.init n (fun k -> (mat |. (i,k)) *. (mat' |. (k,j)))))

let (|*.) : float -> matrix -> matrix =
    fun x mat ->
        init (nb_l mat) (nb_c mat) (fun i j -> x *. (mat |. (i,j)))

let id : int -> matrix =
    fun n ->
        init n n (fun i j -> if i=j then 1. else 0.)

let zeros : int -> int -> matrix =
    fun m n ->
        init m n (fun _ _ -> 0.)

let ones : int -> int -> matrix =
    fun m n ->
        init m n (fun _ _ -> 1.)

let eye : int -> int -> int -> int -> matrix =
    fun m n i j ->
        init m n (fun k l -> if (i=k) && (j=l) then 1. else 0.)

let basis_vect : int -> int -> matrix =
    fun n i ->
        eye n 1 i 0

let switch_l : matrix -> int -> int -> unit =
    fun mat i j ->
        for k = 0 to nb_c mat - 1 do
            let z = mat |. (i,k) in
                (mat |= (i,k)) (mat |. (j,k));
                (mat |= (j,k)) z;
        done

let switched_l : matrix -> int -> int -> matrix =
    fun mat i j ->
        let mat' = copy mat in
            switch_l mat' i j; mat'

let switch_c : matrix -> int -> int -> unit =
    fun mat i j ->
        for k = 0 to nb_l mat - 1 do
            let z = mat |. (k,i) in
                (mat |= (k,i)) (mat |. (k,j));
                (mat |= (k,j)) z;
        done


let switched_c : matrix -> int -> int -> matrix =
    fun mat i j ->
        let mat' = copy mat in
            switch_c mat' i j; mat'

let inject : matrix -> matrix -> int -> int -> unit =
    fun mat mat' i' j' ->
        for i = i' to -1 + min (nb_l mat) (i' + nb_l mat') do
        for j = j' to -1 + min (nb_c mat) (j' + nb_c mat') do
            (mat |= (i,j)) (mat' |. (i-i',j-j'));
        done;
        done

let injected : matrix -> matrix -> int -> int -> matrix =
    fun mat mat' i j ->
        let mat'' = copy mat in
            inject mat'' mat' i j; mat''

let to_right : matrix -> matrix -> matrix =
    fun mat mat' ->
        init (nb_l mat) (nb_c mat + nb_c mat') (fun i j ->
            if j < nb_c mat then mat |. (i,j) else mat' |. (i,j- nb_c mat))

let to_down : matrix -> matrix -> matrix =
    fun mat mat' ->
        init (nb_l mat + nb_l mat') (nb_c mat) (fun i j ->
            if i < nb_l mat then mat |. (i,j) else mat' |. (i- nb_l mat,j))

let to_left : matrix -> matrix -> matrix =
    fun mat mat' ->
        to_right mat' mat

let to_up : matrix -> matrix -> matrix =
    fun mat mat' ->
        to_down mat' mat

let concat_right : matrix list -> matrix =
    fun l ->
        let rec tab = Array.of_list l
        and aux : int -> int -> matrix =
            fun i j ->
                if j-i < 2
                    then tab.(i)
                    else to_right (aux i ((i+j)/2)) (aux ((i+j)/2) j)
        in aux 0 (Array.length tab)

let concat_down : matrix list -> matrix =
    fun l ->
        let rec tab = Array.of_list l
        and aux : int -> int -> matrix =
            fun i j ->
                if j-i < 2
                    then tab.(i)
                    else to_down (aux i ((i+j)/2)) (aux ((i+j)/2) j)
        in aux 0 (Array.length tab)

let filter_l : matrix -> (int -> bool) -> matrix =
    fun mat f ->
        concat_down (Array.to_list @@ Array.init (nb_l mat)
            (fun i -> if f i then mat |.- i else zeros 0 (nb_c mat)))

let filter_c : matrix -> (int -> bool) -> matrix =
    fun mat f ->
        concat_right (Array.to_list @@ Array.init (nb_c mat)
            (fun i -> if f i then mat |.| i else zeros (nb_l mat) 0))

let erase_l : matrix -> int -> matrix =
    fun mat i ->
        filter_l mat ((<>) i)

let erase_c : matrix -> int -> matrix =
    fun mat i ->
        filter_c mat ((<>) i)

let minor : matrix -> (int*int) -> matrix =
    fun mat (i,j) ->
        erase_l (erase_c mat j) i

let to_diag : matrix -> matrix =
    fun mat ->
        init 1 (min (nb_l mat) (nb_c mat)) (fun _ j -> mat |. (j,j))

let from_diag : matrix -> matrix =
    fun (m,n,tab) ->
        if m = 1
            then init n n (fun i j -> if i<>j then 0. else tab.(0).(j))
            else init m m (fun i j -> if i<>j then 0. else tab.(i).(0))

let pivot : matrix -> float =
    fun mat ->
        let eps = ref 1. in
        for i=0 to nb_l mat - 1 do
            try
            for j = i to nb_l mat - 1 do
                if mat |. (j,i) > 0.
                    then raise (Finished(j));
            done;
            with Finished(j) -> if j<>i
                                    then (switch_l mat i j;eps := -. !eps);
            let lin = (1. /. (mat |. (i,i))) |*. (mat |.- i) in
            for j = i+1 to nb_l mat - 1 do
                inject mat ((mat |.- j) |- ((mat |. (j,i)) |*. lin)) j 0;
            done;
        done; !eps

let det : matrix -> float =
    fun mat ->
        let mat' = copy mat in
            let res = ref (pivot mat') in
                iter (to_diag mat') (fun _ _ x -> res := x *. !res);
                !res

let trace : matrix -> float =
    fun mat ->
        let res = ref 0. in
                iter (to_diag mat) (fun _ _ x -> res := x +. !res);
                !res

let inv : matrix -> matrix =
    fun mat_init ->
        let mat = copy mat_init in
        let mat' = id (nb_l mat) in
        for i=0 to nb_l mat - 1 do
            try
            for j = i to nb_l mat - 1 do
                if mat |. (j,i) > 0.
                    then raise (Finished(j));
            done;
            with Finished(j) -> if j<>i then
                 (switch_l mat' i j;switch_l mat i j);
            let lin = (1. /. (mat |. (i,i))) |*. (mat |.- i)
            and li' = (1. /. (mat |. (i,i))) |*. (mat' |.- i) in
            for j = i+1 to nb_l mat - 1 do
                inject mat' ((mat' |.- j) |- ((mat |. (j,i)) |*. li')) j 0;
                inject mat ((mat |.- j) |- ((mat |. (j,i)) |*. lin)) j 0;
            done;
        done;
        for i=nb_l mat - 1 downto 0 do
            print mat;
            print mat';
            let lin = (1. /. (mat |. (i,i))) |*. (mat |.- i)
            and li' = (1. /. (mat |. (i,i))) |*. (mat' |.- i) in
            for j = i-1 downto 0 do
                inject mat' ((mat' |.- j) |- ((mat |. (j,i)) |*. li')) j 0;
                inject mat ((mat |.- j) |- ((mat |. (j,i)) |*. lin)) j 0;
            done;
        done;
            iter (to_diag mat) (fun _ j x ->
                inject mat' ((1. /. x) |*. (mat' |.- j)) j 0);
            mat'

let rec (|^) : matrix -> int -> matrix =
    fun mat -> function
        0 -> id (nb_l mat)
       |i when i < 0 -> (inv mat) |^ (-i)
       |i -> (fun sq -> if i mod 2 = 0 then sq |* sq else mat |* sq |* sq) 
             (mat |^ (i/2))
