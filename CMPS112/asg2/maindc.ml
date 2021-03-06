(* $Id: maindc.ml,v 1.4 2012-02-16 17:47:43-08 - - $ *)
(*----------------------------------------------------- 
- maindc.ml
- Ocaml Desk Calculator
- Assignment 2
- CMPS 112 Fall 2016
- Megan Nguyen
- mednguye@ucsc.edu
- Code Template Provided by Professor Mackey
-----------------------------------------------------*)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint


(* used registers.ml as reference *)
let register_array = Array.make 256(false, Bigint.zero)


(* prints the number list.
   since dc prints the '\' character if a number exceeds 70 characters,
   this recursive function prints accordingly. *)
let rec print_number number = 
    let n = string_of_bigint number in
    (* if the number is less than 70 chars, print it on one line *)
    if (String.length n) < 70
    then (printf "%s\n%!" n)
    (* if the number is greater than or equal, print a \ at the end of the line
       and recursively print the rest of the number *)
    else (printf "%s\\\n%!" (String.sub n 0 69);
          print_number (bigint_of_string (String.sub n 69 ((String.length n) - 69))))


let print_stackempty () = printf "ocamldc: stack empty\n%!"


(* implement l and s, register functions
   l: Copy the value in register and push it onto the stack
   s: Pop the value off the top of the stack and store it into register *)
let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> let car, _ = Array.get register_array reg in
                 let _, cdr = Array.get register_array reg in
                 if car = true
                 then push (cdr) thestack
                 else printf "ocamldc: register '%d' () empty\n" reg 
        | 's' -> Array.set register_array reg (true, (pop thestack)) 
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()


let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

(* calls other helper functions in bigint.ml to execute *)
let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
             (* once the end of the file is reached, exit program like dc does *)
        with End_of_file -> exit 0
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

