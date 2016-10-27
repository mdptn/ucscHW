(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(*----------------------------------------------------- 
- bigint.ml
- Ocaml Desk Calculator
- Assignment 2
- CMPS 112 Fall 2016
- Megan Nguyen
- mednguye@ucsc.edu
- Code Template Provided by Professor Mackey
-----------------------------------------------------*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))


    (* cmp function
       returns a comparison value in the same way as strcmp in C
       returns < 0 if length list1 < list2
       returns = 0 if length and values list1 = list2
       returns > 0 if length list1 > list2 *)
    let cmp list1 list2 = 
        if (List.length list1) < (List.length list2)
        then -1
        else if (List.length list1) > (List.length list2)
        then 1
        (* if not < or >, then length is equal. compare each value *)
        else (let reverse1 = reverse list1 in
              let reverse2 = reverse list2 in
              if reverse1 > reverse2
              then 1
              else if reverse2 > reverse1
              then -1
              else 0)


    (* trimzero function
       deletes leading 0 digits , helps with absolute subtraction
       referenced from trimzeros.ml example *)
    let trimzero list =
        let rec trimzero' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                let cdr' = trimzero' cdr
            in match car, cdr' with
                | 0, []     -> []
                | car, cdr' -> car::cdr'
        in trimzero' list


    (* add' function
       helper function, carries out addition *)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)


    (* sub' function
       helper function, carries out subtraction *)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> trimzero (sub' list1 [carry] 0)
        | [], list2, carry   -> trimzero (sub' [carry] list2 0)
        | car1::cdr1, car2::cdr2, carry ->
          (* if list1 digit is < list2 digit, carry over*)
          if (car1 - carry) < car2
          then let diff = (10 + car1) - (car2 + carry)
               in diff mod radix :: trimzero (sub' cdr1 cdr2 1)
          (* else just subtract as normal *)
          else let diff = car1 - car2 - carry
               in diff mod radix :: trimzero (sub' cdr1 cdr2 0)


    (* add function
       compares signs of both values and then uses add' or sub' helper function *)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* if both signs are equal, use the add' helper function *)
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        (* if they aren't equal, then one is neg and one is pos.
           find out if the first number is negative,
           then compare its absolute value with the second number.
           subtract whichever number is smaller using sub'. *)
        else if neg1 = Neg
        then (if (cmp value1 value2) > 0
              then Bigint (Neg, trimzero (sub' value1 value2 0))
              else Bigint (Pos, trimzero (sub' value2 value1 0)))
        (* find out if the first number is positive*)
        else if neg1 = Pos
        then (if (cmp value1 value2) >= 0
              then Bigint (Pos, trimzero (sub' value1 value2 0))
              else Bigint (Neg, trimzero (sub' value2 value1 0)))
        else zero


    (* sub function
       compares signs of both values and then uses add' or sub' helper function
       almost identical to add function, but handles signs differently *)
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* if both signs are equal, compare values and use the sub' helper function *)
        if neg1 = neg2
        then (if (cmp value1 value2) > 0
              then Bigint (neg1, trimzero (sub' value1 value2 0))
              else if (cmp value1 value2) < 0
              then Bigint (neg1, trimzero (sub' value2 value1 0))
              else zero)
        (* find out if the first number is negative
           if so, add the values and yield a negative number *)
        else if neg1 = Neg
        then Bigint (Neg, add' value1 value2 0)
        (* find out if the first number is positive
           if so, add the values and yield a positive number *)
        else if neg1 = Pos
        then Bigint (Pos, add' value1 value2 0)
        else zero
      

    (* doublex2 function
       doubles a number x2
       used muldivrem-trace.ml as reference *)
    let doublex2 num = num + num


    (* mul' function
       multiplication helper
       used muldivrem-trace.ml as reference *)
    let rec mul' (multiplier, power2, multiplicand') =
        if (cmp power2 multipler) > 0
        then multiplier, [0]
        else let remainder, product =
                mul' (multiplier, doublex2 power2, doublex2 multiplicand')
            in if remainder < power2
                then remainder, product
              else remainder - power2, product + multiplicand'


    (* mul2 function
       calls the mul' helper function
       used muldivrem-trace.ml as reference *)
    let mul2 (multiplier, multiplicand) =
        let _, product = mul' (multiplier, [1], multiplicand)
        in product


    (* mul function
       compares signs of the two numbers and then calls mul2 to help
       if both numbers are the same sign, the answer will be pos. else neg *)
    let mul = (Bigint (neg1, multiplier)) (Bigint (neg2, multiplicand)) =
        (* check if both signs are equal *)
        if neg1 = neg2
        then Bigint (Pos, mul2 (multiplier, multiplicand))
        else Bigint (Neg, mul2 (multiplier, multiplicand))


    let div = add

    let rem = add

    let pow = add

end

