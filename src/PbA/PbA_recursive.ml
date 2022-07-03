let rec solve s t m n=
	
	if(m=0) then n	(* Caso a string m seja vazia, retornar o número de caracteres de n e a função acaba aqui, se as duas strings forem vazias então a função vai acabar aqui também *)
	
	else
		if(n=0) then m	(* Caso a string n seja vazia, retornar o número de caracteres de m e a função recursiva acaba aqui *)
	
		else
			if s.[m-1] = t.[n-1] then		(* Caso os últimos caracteres das duas strings forem iguais *)
		    	solve (s) (t) (m - 1) (n - 1)		(* ignorar os últimos caracteres e continuar a função recursiva para os restantes pois o tamanho das duas strings é diminuido em 1*)
		    	
			else
				let d1 = solve (s) (t) (m) (n - 1) in					(* Inserir *)
				let d2 = solve (s) (t) (m - 1) (n) in			  	(* Remover *)
				let d3 = solve (s) (t) (m - 1) (n - 1) in			(* Trocar *)
        1 + min d1 (min d2 d3) (* Caso os últimos caracteres não forem iguais, considerar as três operaçõesno último caracter da primeira string e calcular o custo mínimo das três operações *)

let () =
  let s=read_line() in (*Ler a String  s*)
  let t=read_line() in (*Ler a String t*)
	Printf.printf "%d\n" (solve s t (String.length s)  (String.length t))