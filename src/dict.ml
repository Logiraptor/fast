
type ('k, 'v) dict =
      Value of ('k * 'v * ('k, 'v) dict)
    | Empty

let rec dump keyFunc valueFunc d : string =
        Printf.sprintf "{%s}" @@ dump_elem d keyFunc valueFunc
    and dump_elem d keyFunc valueFunc =
        match d with
          Empty -> ""
        | Value (k, v, rest) -> Printf.sprintf "%s -> %s; %s" (keyFunc k) (valueFunc v) (dump_elem rest keyFunc valueFunc) 

let append dict key value =
    Value (key, value, dict)


let rec lookup dict key =
    match dict with
        Empty -> None
      | Value (k, v, rest) -> 
        if k = key then
            Some v
        else
            lookup rest key


let rec rev_lookup dict key =
    match dict with
        Empty -> None
      | Value (k, v, rest) -> 
        if v = key then
            Some k
        else
            rev_lookup rest key

let rec length dict =
    match dict with
        Empty -> 0
      | Value (_, _, rest) -> 1 + (length rest)