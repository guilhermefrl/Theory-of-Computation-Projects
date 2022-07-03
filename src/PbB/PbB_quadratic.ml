open Scanf
open Array
open Printf

let criaMatriz x y=
  let a = make_matrix (x) (y) 0 in	(*Cria uma matriz toda preenchida com zeros*)
a ;;

let solve s t m n =
	
	let a = criaMatriz (m+1) (n+1) in		(*Criar a matriz de m+1 por n+1, ou seja, o tamanho das strings mais um*)
	let aux = ref 0 in
 
	for j = 0 to m do
		for  k = 0 to n do
 
			if (j=0 || k=0)
				then a.(j).(k) <- 0		(*Caso seja a primeira coluna ou linha da matriz, preencher essa posição com zero*)
 
      else 
        if (s.[j-1] = t.[k-1])		(*Caso as duas strings sejam iguais*)
				then ( a.(j).(k) <- a.(j-1).(k-1) + 1;		(*Prencher a posição, com o valor da posição diagonal anterior mais um*)
					let d1 = ref a.(j).(k) in
					aux := !(max aux d1); )		(*Guardar na variável aux, o número máximo da matriz*)
 
				else
					a.(j).(k) <- 0	(*Caso as duas strings sejam diferentes, preencher essa posição com zero*)
		done;
	done;
 
	let aux1 = !aux in
	
aux1 ;;		(*Retornar o número máximo da matriz*)
 
let () =
	let s=read_line() in (*Ler a String  s*)
	let t=read_line() in (*Ler a String t*)
 
	printf "%d\n" (solve s t (String.length s)  (String.length t))
;;

(*Ideia Retirada do Site: https://www.geeksforgeeks.org/longest-common-substring-dp-29/*)