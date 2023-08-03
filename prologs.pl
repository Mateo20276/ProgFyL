/*Ejercicio 1)
Se desea definir un predicado ternario maraton (Premio, Categoria, Kilometros) que relacione la categoría de un atleta, y los premios en pesos que se le deben dar en una maratón de 100 kilómetros.
• Si el atleta recorre una cantidad de kilómetros mayor que 0 y menor a 50, su
categoría es 3.
Si el atleta recorre una cantidad de kilómetros mayor o igual a 50 y menor a 100, su categoría es 2.
• Si el atleta completa la maratón, o sea recorre los 100 km, su categoría es 1.
Los premios se asignan de la siguiente forma:
• Si el atleta recorre una cantidad mayor a cero y menor a 30 km, el premio es 1000.
• Si el atleta recorre una cantidad mayor o igual a 30 km y menor a 100 km, el premio es 5000.
• Si el atleta completa la maratón (o sea, recorre los 100 km), el premio es 100000.
Por ejemplo, los siguientes predicados son verdaderos:
maraton(100000, 1, 100).
maraton(5000,2,99).
maraton(1000,3,1).
*/
premio(P,Km):-
	P =:=1000,
	Km >0,
	Km <30.

premio(P,Km):-
	P =:=5000,
	Km >=30,
	Km <100.

categoria(C,Km):-
	C =:= 3,	
	Km > 0,
	Km <50.

categoria(C,Km):-
	C =:= 2,	
	Km >= 50,
	Km <100.
	
maraton(Premio,Categoria,Kilometros):-
	categoria(Categoria,Kilometros),
	premio(Premio,Kilometros).

maraton(100000,1,100).


/*

kmPremio(Km, 1000) :- Km > 0, Km < 30.
kmPremio(Km, 5000) :- Km >= 30, Km < 100.
kmPremio(Km, 100000) :- Km = 100.

maraton(Premio, Categoria, Kilometros) :-
    Kilometros > 0, Kilometros < 50,
    Categoria is 3,
    kmPremio(Kilometros,Premio).

maraton(Premio, Categoria, Kilometros) :-
    Kilometros >= 50, Kilometros < 100,
    Categoria is 2,
    kmPremio(Kilometros,Premio).

maraton(Premio, Categoria, Kilometros) :-
    Kilometros = 100,
    Categoria is 1,
    kmPremio(Kilometros,Premio).
*/



/* Escribe un proceso recursivo que
permita decidir si un número natural N es divisible por 11.
Dado que se sabe que un número es divisible entre 11,
si y solo si la suma de los digitos de posición par menos la suma de los dígitos de posición impar es un múltiplo de 11. 
Por ejemplo: sea N = 2341675, entonces (5 + 6 + 4 + 2) (7 + 1 + 3) = 6, que no es múltiplo de 11, por lo tanto N no es divisible entre 11. 



div11(N):-
	div11Aux(N,0,0,impar).

div11Aux(0,P,I,_):-
	(P - I) mod 11 =:= 0.

div11Aux(N, P,I,impar):-
	N > 0,
	Numero is N mod 10,
	Na is (N // 10),
	Ia is I + Numero,
	div11Aux(Na,P,Ia,par). 



div11Aux(N, P,I,par):-
	N > 0,
	Numero is N mod 10,
	Na is (N // 10),
	Pa is P + Numero,
	div11Aux(Na,Pa,I,impar). */


/*Tenemos cuatro perros: un galgo, un dogo, un alano y un podenco. Éste último come más que el galgo; el alano come más que el galgo y menos que el dogo, pero éste come más que el podenco. ¿Cuál de los cuatro será más barato de mantener?.*/


concatenar([], L, L).
concatenar([Cab|Cola], L2, [Cab| Colar]):-
	concatenar(Cola,L2,Colar).

invertir_lista([], []).
invertir_lista([X|Xs], Invertida) :-
  invertir_lista(Xs, Resto),
  append(Resto, [X], Invertida).

puntaje(puntito, 1).
puntaje(frutilla, 10).
puntaje(banana, 30).
puntaje(cerezas, 50).
puntaje(fantasma, 100).

puntajePacman(Lista, PuntajeTotal) :-
  puntajePacman(Lista, 1, 0, PuntajeTotal).

puntajePacman([], _, PuntajeAcumulado, PuntajeAcumulado).
puntajePacman([bonus|T], PasosRestantes, PuntajeAcumulado, PuntajeTotal) :-
  puntajePacman(T, 6, PuntajeAcumulado, PuntajeTotal).
puntajePacman([H|T], PasosRestantes, PuntajeAcumulado, PuntajeTotal) :-
  puntaje(H, Puntos),
  NuevoPuntaje is PuntajeAcumulado + Puntos,
  NuevoPasosRestantes is PasosRestantes - 1,
  puntajePacman(T, NuevoPasosRestantes, NuevoPuntaje, PuntajeTotal).
puntajePacman([_|T], PasosRestantes, PuntajeAcumulado, PuntajeTotal) :-
  NuevoPasosRestantes is PasosRestantes - 1,
  puntajePacman(T, NuevoPasosRestantes, PuntajeAcumulado, PuntajeTotal).


listaPrimo([],[]).
listaPrimo([Cab|Cola], [Cab|Primo]):-
	es_primo(Cab),
	listaPrimo(Cola, Primo).

listaPrimo([Cab|Cola], Primo):-
	listaPrimo(Cola, Primo).
	

es_primo(2).
es_primo(3).
es_primo(P) :- 
    integer(P), 
    P > 3, 
    P mod 2 =\= 0, 
    tiene_factor(P,3).

tiene_factor(N,L) :- N mod L =:= 0.
tiene_factor(N,L) :- 
     N > L,
    L2 = L + 1,
    tiene_factor(N,L2).


potencia_recursiva(_, 0, 1).   % Caso base: si n es cero, el resultado es 1

potencia_recursiva(M, N, Resultado) :-
    N mod 2 =:= 0,   % Si n es par
    N1 is N // 2,
    potencia_recursiva(M, N1, Temp),
    Resultado is Temp * Temp.

potencia_recursiva(M, N, Resultado) :-
    N mod 2 =:= 1,   % Si n es impar
    N1 is (N - 1) // 2,
    potencia_recursiva(M, N1, Temp),
    Resultado is M * Temp * Temp.




primo(N):-
	calculo(N,2).

calculo(N,E):-
	E < N,
	N mod E =:= 0.

calculo(N,E):-
	E < N,
	Ex is (E + 1),
	calculo(N,Ex).

/*Un número es divisible por 7 cuando separando la primera cifra de la derecha, multiplicándola por 2, restando este producto de lo que queda a la izquierda y así sucesivamente, da cero o múltiplo de 7.
Por ejemplo, los siguientes números son divisibles entre 7, porque:3
32291 			última cifra 1 -> 1x2=2 	
3229-2 = 3227 	última cifra 7 -> 7x2=14 	
322-14 = 308 		última cifra 8 -> 8x2=16 
30-16 = 14 		resultado parcial 14. 
Como 14 pertenece a la tabla del 7 (7x1 … 7x10) el número original es divisible por 7.
Generar un algoritmo que decida si un número dado es múltiplo de 7 o no, utilizando este método*/


divisible(N):-
	N  < 70,
	N > 6,
	N mod 7 =:= 0.

divisible(N):-
	N < 6, !,false.

divisible(N):-
	Aux is N mod 10,
	Auxx is (Aux * 2),
	Ne is (N // 10),
	Nuevo is (Ne - Auxx),
	divisible(Nuevo).


factorial(0, 1).   % Caso base: el factorial de 0 es 1

factorial(N, Resultado) :-
    N > 0,   % Asegurarse de que N sea mayor que 0
    N1 is N - 1,
    factorial(N1, Subresultado),
    Resultado is N * Subresultado.


/* producto([2,1,3],[2,2,1],N).
N = 9*/

producto([],[],0).
producto([Ca|Co],[Cab|Col],N):-
	producto(Co,Col,N1),
	N is (N1 + (Ca*Cab)).


explota(Numero, Bomba, Resultado):-
	Numero =< Bomba,
	Resultado is [Numero].


explota(Numero, Bomba, Resultado):-
	Numero >=Bomba,
	Numaux is (Numero div Bomba),
	Numaux2 is (Numero -(Numero div Bomba)),
	explota(Numaux, Bomba, Resultado1),
	explota(Numaux2, Bomba, Resultado2),
	append(Resultado1,Resultado2,Resultado).


explota1(N, B, Resultado) :-
    explota_aux(N, B, Resultado).

explota_aux(N, B, [N|Resto]) :-
    N > B,
    N1 is N // B,
    N2 is N - N1,
    explota_aux(N1, B, Resto1),
    explota_aux(N2, B, Resto2),
    append(Resto1, Resto2, Resto).
explota_aux(N, _, [N]).

duplas([], [], []).
duplas([X|L1], [Y|L2], [[X,Y]|L3]) :-
    duplas(L1, L2, L3).


















