(* Ex2.1 *)

datatype expr = IConst of int 
  | Plus of expr * expr 
  | Minus of expr * expr 
  | Multi of expr * expr 
  | Div of expr * expr 
  | Max of expr * expr 
  | Min of expr * expr 
  | Eq of expr * expr 
  | Gt of expr * expr;

fun eval (given:expr) : int =
  case given of
    IConst(e) => e (* eval -> int *)
  | Plus(e1,e2) => eval(e1) + eval(e2)
  | Minus(e1,e2) => eval(e1) - eval(e2)
  | Multi(e1,e2) => eval(e1) * eval(e2)
  | Div(e1,e2) => if eval(e2) = 0 then 0 else eval(e1) div eval(e2) (*dividido por 0 = 0*)
  | Max(e1,e2) => if eval(e1) >= eval(e2) then eval(e1) else eval(e2)
  | Min(e1,e2) => if eval(e1) <= eval(e2) then eval(e1) else eval(e2)
  | Eq(e1,e2) => if eval(e1) = eval(e2) then 1 else 0
  | Gt(e1,e2) => if eval(e1) > eval(e2) then 1 else 0;


Max(IConst 3, Plus(IConst 2, IConst 3));

(* Ex2.2 *)

(* implemente a função eval *)

datatype area = RConst of real 
  | AQuadrado of area 
  | ARetangulo of area * area 
  | ACirculo of area; (* Para calcular é dado o raio pi r2*)

fun eval (given:area) : real =
  case given of
    RConst(e) => e
  | AQuadrado(e) => eval(e) * eval(e)
  | ARetangulo(e1,e2) => eval(e1) * eval(e2)
  | ACirculo(e) => 3.14 * eval(e) * eval(e);


(* Ex2.3 *) 

datatype perimetro  = RConst of real 
  | PQuadrado of perimetro 
  | PRetangulo of perimetro * perimetro 
  | PCirculo of perimetro 
  | PTriangulo of perimetro * perimetro * perimetro;

fun eval (given:perimetro) : real =
  case given of
    RConst(e) => e
  | PQuadrado(e) => eval(e) * 4.0
  | PRetangulo(e1,e2) => eval(e1) + eval(e1) + eval(e2) + eval(e2)
  | PCirculo(e) => 2.0 * 3.14 * eval(e)
  | PTriangulo(e1,e2,e3) => eval(e1) + eval(e2) + eval(e3);


(* Ex2.4 *) FAZER


datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = IConst of int | Op1 of UnOp * Sexpr | Op2 of BinOp * Sexpr * Sexpr;

fun simplify (IConst n) = IConst n
  | simplify (Op1 (Not, Op1 (Not, e))) = simplify e
  | simplify (Op1 (Not, e)) = if simplify e = IConst 0 then IConst 1 else IConst 0
  | simplify (Op2 (Add, IConst 0, e)) = simplify e
  | simplify (Op2 (Add, e, IConst 0)) = simplify e
  | simplify (Op2 (Add, IConst e1, IConst e2)) = IConst (e1 + e2)
  | simplify (Op2 (Add, e1, e2)) = simplify (Op2 (Add, simplify e1, simplify e2))



  | simplify (Op2 (Add, e1, e2)) = simplify (Op2 (Add, simplify e1, simplify e2)

  | simplify (Op2 (Sub, e, IConst 0)) = simplify e
  | simplify (Op2 (Sub, IConst 0, e)) = simplify (Op1 (Not,e))
  | simplify (Op2 (Sub, e1, e2)) = if e1 = e2 then IConst 0 else simplify (Op2 (Add, e1, simplify (Op1 (Not, e2))))
  | simplify (Op2 (Mul, IConst 1, e)) = simplify e
  | simplify (Op2 (Mul, e, IConst 1)) = simplify e
  | simplify (Op2 (Mul, IConst 0, _)) = IConst 0
  | simplify (Op2 (Mul, _, IConst 0)) = IConst 0
  | simplify (Op2 (Gt, e1, e2)) = if simplify e1 > simplify e2 then IConst 1 else IConst 0
  | simplify (Op2 (Eq, e1, e2)) = if simplify e1 = simplify e2 then IConst 1 else IConst 0
  | simplify (Op2 (Or, e1, e2)) = if simplify e1 = IConst 1 orelse simplify e2 = IConst 1 then IConst 1 else IConst 0;

















(* Ex2.3 *)

fun count_main(x:int) =
  let
    fun count(i:int) =
      if i <= x then i :: count(i+1) else []
  in
    count(1)
  end

count_main (10)


(* Ex2.4 *)

fun pow(x:int) : int=
  let
    fun calculatePow(i:int) :int =
      i * i
  in
    calculatePow(x)
  end


pow(2)



(* Ex2.6 *)

fun split lista_passada =
    let
      fun split3listas [] (l1, l2) = (l1, l2)
      |   split3listas [x] (l1, l2) = (x::l1, l2) (*se sobrar 1 = Impar = à esquerda*)
      |   split3listas (x::y::xs) (l1, l2) = split3listas xs (x::l1, y::l2)
      val (r1,r2) = split3listas lista_passada ([],[])
    in
      (rev r1,rev r2)
    end

split([1,2,3,8,4,5])
