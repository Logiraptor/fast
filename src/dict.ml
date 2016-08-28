
type ('k, 'v) dict =
      Value of ('k * 'v * ('k, 'v) dict)
    | Empty

let rec dump_dict d keyFunc (valueFunc) : string =
        Printf.sprintf "{%s}" @@ dump_dict_elem d keyFunc valueFunc
    and dump_dict_elem d keyFunc valueFunc =
        match d with
          Empty -> ""
        | Value (k, v, rest) -> Printf.sprintf "%s -> %s; %s" (keyFunc k) (valueFunc v) (dump_dict_elem rest keyFunc valueFunc) 


let dict_append dict key value =
    Value (key, value, dict)

let rec dict_lookup dict key =
    match dict with
        Empty -> None
      | Value (k, v, rest) -> 
        if k = key then
            Some v
        else
            dict_lookup rest key

