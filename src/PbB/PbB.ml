let () =
	let r=read_line() in (*Lê a primeira linha*)
	let t=read_line() in (*Lê a segunda linha*)
	let z = String.concat r [""; "#"] in (*Coloca um "#" no fim da primeira string, este caracter serve para sabermos onde acaba a primeira string, não pertence ao dicionáro*)
	let z1 = String.concat z [""; t] in   (*Coloca a segunda string, aseguir ao ultimo caracter da primeira*)
	let s = String.concat z1 [""; "$"] in	(*Coloca um "$" no fim da string final que já tem as duas strings lidas, este caracter serve para sabermos onde acaba a string, também um carácter que não pertence ao dicionáro*)
	let l1 = String.length r in (*O tamanho da primeira string*)
	let c1 = String.length s in (*Tamanho da string s, a final*)
	let a = Array.make_matrix (c1+1) (2) "" in (*Cria uma matriz com o tamanho do total de linhas igual ao tamanho da string mais um, e com 2 colunas*)
	let lcp = Array.make_matrix (c1+2) (2) 0 in (*Cria uma matriz com o tamanho do total de linhas igual ao tamanho da string mais dois, e com 2 colunas*)
	let rf = ref 0 in
	(*A matriz a serve para colocarmos os suffix array da string e na 2ª coluna colocamos 1 para saber que começa com caracters da primeira string, enquanto que
	na segunda metade colocamos 2, para sabermos que os caracteres iniciais são da string 2*)
		(*A Matriz lcp, a 1ª coluna serve para colocarmos o valor da comparação entre as string, e a 2º coluna colocamos o valor de que string inicial os caracteres são originários, ou seja 1 ou 2
		tendo em consideração a sua posição na matriz a*)
	for i=0 to c1 do
		let c2 = String.sub s i (c1-i) in   (*Vai subtraindo a primeira letra da string*)
		a.(i).(0) <- c2;                    (*Vai meter na matriz o c2, ou seja o suffix array sempre que se retira uma letra*)
		
		let x1 = string_of_int 1 in 
		let x2 = string_of_int 2 in
		
		if(i<(l1+1))
		then (a.(i).(1) <- x1;) (*Colocamos o valor 1 na coluna 2 da matriz a, nas linhas que são ate ao tamanho da primeira string, para sabermos que estes valores começam com caracteres da string 1, ou seja string r*)
		
		else
			a.(i).(1) <- x2;		(*Colocamos o valor 2 na coluna 2 da matriz a, nas restantes linhas que já não pertencem á string r/1 para sabermos que estes valores começam com caracteres da string 1, ou seja string s*)
	done;
	
	Array.sort compare a;                 (*Vai ordenar alfabeticamente a primeira coluna da matriz a*)

	for i=1 to c1-1 do   (*Este for começa em 1, pois não comparamos o primeiro sufix array, e acaba em c-1 pois dentro do for estamos a compara o sufix i com o i+1*)
		let aux1 = int_of_string a.(i).(1) in
		lcp.(i).(1) <- aux1;  (*Na matriz lcp na 2ª coluna, colocamos o valor 1 ou 2, de acordo com a origem do suffix array, que irá ser verificado na matriz a*)
		
		let aux3 = a.(i).(0) in (*aux3 vai ser o primeiro suffix array da coluna, que está na matriz a, e que agora queremos comparar com o seguinte*)
		let aux4 = a.(i+1).(0) in (*aux4 vai ser o segundo suffix array da coluna, que está na matriz a, e que agora queremos comparar com o anterior, o aux3*)
		let aux5 = (min (String.length r) (String.length t)) in (*aux 5 vai ser o valor mínimo entre o tamanho das duas strings originais, a r e t*)
		let aux6 = (min (String.length aux3) (String.length aux4)) in (*aux 6 vai ser o valor mínimo das duas strings que tamos a comparar atualmente no for*)
		let k3 = ref 0 in

		while((aux3.[!k3] = aux4.[!k3]) && (!k3 < aux5) && (!k3 < aux6)) do (*Este while serve para comparar as duas strings, aux3 e aux4, vai comparar até que encontre algum caracter diferente, ou chegue ao valor do tamanho das duas sub strings, ou chegue ao valor do tamanho d string mais pequena introduzida no inicio*)
			k3:=!k3+1;
			lcp.(i).(0) <- !k3; (*Na matriz lcp vamos sempre colocar o do k3 atualizado enquanto o while for true*)
		done;
	done;
	
	for k2=1 to c1-2 do
		if(!rf<lcp.(k2).(0) && lcp.(k2).(1) != lcp.(k2+1).(1)) (*Este if serve para excluirmos os valores que são originários da comparação da mesma string inicial, não serem validados, e para atualizarmos o valor de rf*)
		then 
			rf := (max lcp.(k2).(0) lcp.(k2+1).(0));
	done;
	
	Printf.printf "%d\n" !rf
;;

(*Ideia Base retirada dos sites: 
https://www.youtube.com/watch?v=zqKlL3ZpTqs
https://www.youtube.com/watch?v=53VIWj8ksyI
https://cs.stackexchange.com/questions/9555/computing-the-longest-common-substring-of-two-strings-using-suffix-arrays
*)

(*
Exemplo:

Strings: ATTGCAG e CTAGG

1º - Juntar # e $ no final da primeira e segunda string, respetivamente.

ATTGCAG#CTAGG$

2º - Fazer os sufixos da palavra anterior e colocar 1 ou 2, na segunda coluna, caso a primereira letra pertença à primeira
ou segunda string, incluindo # e $ respetivamente.

ATTGCAG#CTAGG$ | 1
TTGCAG#CTAGG$  | 1
TGCAG#CTAGG$   | 1
GCAG#CTAGG$    | 1
CAG#CTAGG$     | 1
AG#CTAGG$      | 1
G#CTAGG$       | 1
#CTAGG$        | 1
CTAGG$         | 2
TAGG$          | 2
AGG$           | 2
GG$            | 2
G$             | 2
$              | 2

3º - Ordenar os sufixos alfabeticamente.

#CTAGG$         | 1
$               | 2
AG#CTAGG$       | 1
AGG$            | 2
ATTGCAG#CTAGG$  | 1
CAG#CTAGG$      | 1
CTAGG$          | 2
G#CTAGG$        | 1
G$              | 2
GCAG#CTAGG$     | 1
GG$             | 2
TAGG$           | 2
TGCAG#CTAGG$    | 1
TTGCAG#CTAGG$   | 1

4º - Verificar dois a dois os sufixos, contando quantas letras são iguais, e restringindo a contagem ao menor número de letras
das duas strings originais e dos dois sufixos a ser verificados. Assim, a contagem é colocada na primeira coluna e na segunda 
coluna são colocados os números da segunda coluna da matriz anterior.

- | 1
0 | 2
0 | 1
2 | 2
1 | 1
0 | 1
1 | 2
0 | 1
1 | 2
1 | 1
1 | 2
0 | 2
1 | 1
1 | 1

5º - Verificar o maior número na primeira coluna da matriz anterior, sendo que só pode ser de strings orginais diferentes,
ou seja, 1 e 2.

Maior substring comum: 2

*)