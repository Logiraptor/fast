


(*arithmetic*)
let zero f x = x

let succ n f x = f (n f x)

let pred n f x = n (fun g -> fun h -> h (g f)) (fun u -> x) (fun u -> u)

let plus m n f x = m f (n f x)

let sub m n = n pred m

let mult m n f = m (n f)

let pow b e = e b


(*numbers*)
let two = succ (succ zero)
let four = plus two two
let ten = plus (plus four four) two

(*logic*)

let true_ x y = x
let false_ x y = y

let and_ p q = p q p
let or_ p q = p p q
let not_ p = p false_ true_
let ifthenelse p a b = p a b

let iszero n = n (fun x->false_) true_
let leq x y = iszero (sub x y)
let eq x y = and_ (leq x y) (leq y x)


(*combinators*)
let u f = f f
let y = u (fun f -> fun f_ -> f_ (fun x -> f f f_ x))

(*factorial*)
let fact r x =
    (ifthenelse (iszero x)
        (fun q -> (succ zero)),
        (fun q -> (mult x r (pred x)))
    ) 0

(*resolution functions*)
let inc x = x + 1
let resolve n = n inc 0

let () =
    print_int (resolve (y fact four))
