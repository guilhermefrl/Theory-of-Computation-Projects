open Scanf
open Array
open Printf

 let criaMatriz  x y=
 let a = make_matrix (x) (y) 0 in (*Cria uma matriz de m+1 por n+1, ou seja o tamanho das strings mais um, toda preenchida com zeros*)
 	a 	;;
 
 let  linha m n a=
 	(*Preenche a tabela com o tamanho das matrizes em crescente*)
  for i = 0 to m do
     for  j = 0 to n do
 
            if (i = 0) (*Se o i=0, então temos que colocar todos os carácteres da outra string, ou seja colocando a segunda string na horizontal na linha 0 da tabela*)
            then
                a.(i).(j)<-j
            else
            	if (j=0) (*Quando o j for 0, temos que fazer a mesma coisa mas para colocando agora a primeira string, colocando esta na coluna 0 da tabela*)
            	then
                a.(i).(j)<-i
                
          done;
          done;
            a ;;  

let solve s t m n  =
  let a=
  		criaMatriz (m+1)(n+1) (*Tem que haver uma fila e uma coluna extra para podermos colocar o tamanho das duas strings*)
  		in
 
 let a =
 	linha (m) (n) (a)
		in
 
  for k = 1 to n do (*Usamos dois for para corrermos a matriz em todas as direções*)
    for j = 1 to m do
 
      if s.[j-1] = t.[k-1] then (*Verifica se as letras em teste são iguais*)
        a.(j).(k) <- a.(j-1).(k-1)  (*Se elas forem iguais então não é preciso qualquer operação logo, vamos colocar na tabela o número do quadrado anterior*)

        else (*Se os caracteres são diferentes então temos que considerar todas as hipóteses e escolher a que tem custo minimo*)
        let d1 = (a.(j-1).(k) + 1) in (*Operação de Remover*)
        let d2 = (a.(j).(k-1) + 1) in (*Operação de Inserir*)
        let d3 = (a.(j-1).(k-1) + 1) in (*Operação de Substituir*)
         a.(j).(k) <- min d1 (min d2 d3) (*Aqui vimos qual das 3 operações tem o custo mais pequeno, e assim vamos inserir na posição corrente a que contêm o menor custo*)
    done;
  done;
  a.(m).(n) (*No fim vamos retornar o ultimo elemento da diagonal da matriz, pois este será o resultado mais baixo possível*)
;;

let () =
  let s=read_line() in (*Ler a String  s*)
  let t=read_line() in (*Ler a String t*)
  
	printf "%d\n" (solve s t (String.length s)  (String.length t))
;;

(*Ideia Base retirada do site: https://www.geeksforgeeks.org/edit-distance-dp-5/*)