module Lists

let rec last list =
    match list with
    | [] -> None
    | [x] -> Some x
    | _::t -> last t;;

let rec last_two list =
    match list with
    | [] | [_] -> None
    | [x;y] -> Some (x,y)
    | h::t -> last_two t;;

let at pos list =
    if pos < 1 
        then None
        else
        let rec aux p l =
            match l with
            | [] -> None
            | h::t -> 
                if p = 1 
                    then Some h 
                    else aux (p-1) t
        in aux pos list;;

let length list =
    let rec aux acc list =
        match list with
        | [] -> acc
        | _::t -> aux (acc + 1) t
    in aux 0 list;;

let reverse list = 
    let rec aux acc l =
        match l with
        | [] -> acc
        | h::t -> aux (h::acc) t
    in aux [] list;;

let is_palindrome list = 
    list = reverse list;;

type 'a node = 
    | One of 'a
    | Many of 'a node list;;

let flatten node_list =
    let rec aux acc n_list =
        match n_list with
        | [] -> acc
        | One x :: t -> aux (x :: acc)  t
        | Many xs :: t -> aux (aux acc xs) t
    in reverse (aux [] node_list);;

let pack list = 
    let rec aux current acc l =
        match l with
        | [] -> acc
        //todo
        | [x] -> (x :: current) :: acc
        | a :: (b :: _ as t) -> 
            if a = b
            then aux (a :: current) acc t
            else aux [] ((a :: current) :: acc) t
    in reverse (aux [] [] list);;

let encode list =
    let rec aux acc count list =
        match list with
        | [] -> acc
        | [a] -> (count, a) :: acc
        | a :: (b :: _ as t) ->
            if a = b
            then aux acc (count+1) t
            else aux ((count, a) :: acc) 1 t
    in reverse (aux [] 1 list);;