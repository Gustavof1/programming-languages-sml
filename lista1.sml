(* Ex 1 *)
fun cube x:real = x * x * x;

cube 2.0;

(* Ex 2 *)
fun pow (x, 0) = 1
  | pow (x, 1) = x
  | pow (x, y) = pow (x, y-1) * x;

pow (2, 0);

(* Ex 3 *)

(* chatgpt *)
fun sumLists (typelist1, typelist2) =
  let
    fun sumHelper ([], [], acc) = rev acc
      | sumHelper (x::l1, y::l2, acc) = sumHelper(xs, ys, (x+y)::acc)
  in
    sumHelper(typelist1, typelist2, [])
  end

(* função ListPair.map tipo.+ *)
fun sumLists (x, y) = ListPair.map Int.+ (x, y);

(* normal *)
fun sumLists (x,[]) = x (* só uma lista retorna ela *)
  | sumLists ([],y) = y (* só uma lista retorna ela *)
  | sumLists (itemX::list1, itemY::list2) = (itemX+itemY)::sumLists(list1,list2); (* 2 listas, item no índice 1 somado e colocado na posição 1 da lista,
                                                                                       chamada recursiva pros próximos itens colocados na próxima posição *)

sumLists([1,2,33],[2,5]);

(* Ex 4 *)
fun max [] = 0 (* se lista vazia retorna 0*)
  | max [x] = x (* se lista tem 1 elemento retorna ele mesmo *)
  | max ([], max) = max  (* se recebe tupla, lista vazia e inteiro retorna inteiro (para recursão) *)
  | max (x::list, max) = if x > max then max(list,x) (* x é item da lista, se x maior que o maior da lista sem x, retorna x, se menor, chama max com a lista sem o elemento x *)
                         else max(list,max);  

(* chatgpt *)
fun max (lista : int list) : int =
  let
    fun comparaListaEValor ([], max) = max (* se lista vazia e valor, retorna valor *)
      | comparaListaEValor (x::xs, max) =
          if x > max then comparaListaEValor(xs, x) (* se primeiro valor da lista maior que o valor passado, chama recursão com ele sendo maior *)
          else comparaListaEValor(xs, max) (* se primeiro valor menor, chama recursão com max ainda sendo max  *)
  in
    case lista of
      [] => 0 (* se lista vazia retorna 0 *)
      | x::xs => comparaListaEValor(xs, x) (* se lista com alguma coisa, chama comparador com a lista e o primeiro índice *)
  end

max [3456,3467,4,745,6234,56,1236,243,6,243,56,34,76,3457,345,7,42,67,3457,45,67,45,67,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76,4,745,6234,56,1236,243,6,243,56,34,76];

(* Ex 5 *)


(* chatgpt modificado e explicado *)
fun cumSum (lista : int list) : int list = (* Recebe lista e retorna lista *)
  let
    fun cumSumIterador ([], lista_acumulada, _) = rev lista_acumulada (* função que recebe 2 listas e um valor pra somar, primeira lista é a de entrada,
                                                                        segunda lista é a que está sendo criada, se não tem mais valor pra somar e a lista inicial é vazia
                                                                        retorna a lista reversa rev, *)
      | cumSumIterador (x::xs, lista_acumulada, sum) = (* Se ainda tem elemento na lista de entrada, e função recebeu um item pra somar *)
          let
            val proximo_item = sum + x (* proximo item é a soma do elemento na lista com a soma total *)
          in
            cumSumIterador(xs, proximo_item::lista_acumulada, proximo_item) (* Chama recursivo, passa a lista sem x, agrega o proximo item a lista que está sendo criada, e passa
                                                                            proximo item como item pra somar com o próximo elemento da lista *)
          end
  in
    cumSumIterador(lista, [], 0) (* chama iterador com lista passada, lista vazia e 0 *)
  end

cumSum [6, 10, 3, 11];

(* Ex 6 *)
fun greet x:string = if size(x) > 0 then "Hello " ^ x else "Hello nobody";

greet "f";

(* Ex 7 *)
fun charSeparador x = Char.isSpace x orelse (Char.isPunct x andalso x <> #"_" andalso x <> #"+" andalso x <> #";" andalso x <> #"?");

fun split (x:string) : string list = String.tokens charSeparador(x);

split "good morning to-you";

(* Ex 8 *)
fun allTrueVazia(lista:bool list) : bool =
    case lista of
      [] => true
    | (x::lista) => if x = false then false else allTrueVazia(lista);

fun allTrue (lista:bool list) : bool =
    case lista of
      [] => false
    | (x::lista) => if x = false then false else allTrueVazia(lista);

allTrue [];
    
allTrue [true, true, true];

(* Ex 9 *)
datatype dinheiro = Centavos of int | Reais of real | Pessoa_Dinheiro of string * real;

fun amount (p:dinheiro) : int = 
    case p of
        Centavos x => x
    |   Reais x => round(x * 100.0)
    |   Pessoa_Dinheiro (_, x) =>  round(x * 100.0);


amount (Reais(2.5))

(* Ex 10 *)

datatype Planeta = Mercurio | Venus | Terra | Marte | Jupiter | Saturno | Urano | Netuno;

fun planetDays (p:Planeta):int = 
    case p of
      Mercurio => 88
    | Venus => 225
    | Terra => 365
    | Marte => 687
    | Jupiter => 4332
    | Saturno => 10760
    | Urano => 30681
    | Netuno => 60190;

fun planetAge (idade_em_meses:int, p:Planeta):int =
    let
        val anos = idade_em_meses div 12
        val meses_sobrados = idade_em_meses mod 12
        val dias_anos = planetDays p * anos
        val dias_meses = planetDays p div 12 * meses_sobrados
    in
        dias_anos + dias_meses
    end;

planetAge(24, Jupiter)

(* Ex 11 *)
datatype btree = Leaf | Node of (btree * int * btree);

fun sumAll (Leaf) = 0
  | sumAll (Node(Leaf,valor,Leaf)) = valor
  | sumAll (Node(esq,valor,dir)) = valor + sumAll(esq) + sumAll(dir);

sumAll(Node (Node (Leaf, 1, Leaf), 6, Node (Leaf, 12, Leaf)));

(* Ex 12 *)
fun multiPairs (x, y) = ListPair.map Int.* (x, y);

multiPairs ([2, 5, 10], [4, 10, 8]);

(* Ex 13 *)
fun square x:int = x * x;
fun sum (x, y) = x + y;

fun compose (a,b,x, y) = square(sum(x,y));

compose (2,5);

