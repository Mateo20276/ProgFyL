/*
Todas las águilas vuelan: Vx A(x) => V(x)
Hay personas que no son buenas: Ex  P(x) ^ -B(x)
*/

/*
Un número es divisible por 7 cuando separando la primera cifra de la derecha, multiplicándola por 2, restando este producto de lo que queda a la izquierda y así sucesivamente, da cero o múltiplo de 7.
*/
multiplo(N1, N2) :-
    N1 mod N2 =:= 0;
    N2 mod N1 =:= 0.

divisibleX7(N) :- N =:= 0, !.

divisibleX7(N) :-
    N < 71,
    multiplo(N, 7), !.

divisibleX7(N) :-
    N1 is integer(N // 10),
    N2 is (N mod 10) * 2,
    N3 is N1 - N2,
    N3 >= 0,
    divisibleX7(N3).

% 􀀁empoDedicado(Ac􀀁vidad,Minutos).
tiempoDedicado(almuerzoDeNegocios,120).
tiempoDedicado(atenderCliente,10).
tiempoDedicado(atenderClienteVip,30).
tiempoDedicado(reunionConGerente,30).
% agenda(Ac􀀁vidad, HoraComienzo, MinutosComienzo).
agenda(reunionConGerente, 9, 30).
agenda(atenderClienteVip, 10, 30).
agenda(almuerzoDeNegocios, 12, 0).
agenda(atenderClienteVip, 16, 30).
agenda(atenderClienteVip, 17, 10).
%Escribir un predicado que relacione un horario con lo que debería estar haciendo el empleado.
%Por Ejemplo:
% haciendo(Hora, Minutos, Ac􀀁vidad).

haciendo(Hora, Minutos, Actividad) :-
    tiempoDedicado(Actividad, MAct),
    agenda(Actividad, HC, MC),
    MinutosConsulta is Hora * 60 + Minutos,
    MinutosComienzo is (HC * 60 + MC),
    MinutosFin is (HC * 60 + MC + MAct),
    MinutosConsulta =< MinutosFin,
    MinutosConsulta >= MinutosComienzo.

nivelMasProfundo(Elemento, Lista, Nivel) :-
    nivelMasProfundo(Elemento, Lista, 1, Nivel).

%nivelMasProfundo(Elemento, [Elemento|_], NivelActual, NivelActual).
%nivelMasProfundo(Elemento, [Cabecera|_], NivelActual, Nivel) :-
%    is_list(Cabecera),
%    NuevoNivel is NivelActual + 1,
%    nivelMasProfundo(Elemento, Cabecera, NuevoNivel, Nivel).
%nivelMasProfundo(Elemento, [_|Resto], NivelActual, Nivel) :-
%    nivelMasProfundo(Elemento, Resto, NivelActual, Nivel).


nivelMasProfundo(Elemento, Lista, Nivel) :-
    nivelMasProfundo(Elemento, Lista, 1, Nivel).

nivelMasProfundo(Elemento, [Elemento|_], NivelActual, NivelActual).
nivelMasProfundo(Elemento, [Sublista|_], NivelActual, Nivel) :-
    is_list(Sublista),
    NuevoNivel is NivelActual + 1,
    nivelMasProfundo(Elemento, Sublista, NuevoNivel, Nivel).
nivelMasProfundo(Elemento, [_|Resto], NivelActual, Nivel) :-
    nivelMasProfundo(Elemento, Resto, NivelActual, Nivel).

clasifica(Temperaturas, ListaClasificaciones) :-
    clasificaAux(Temperaturas, [], ListaClasificaciones).

clasificaAux([], ResultadoParcial, ResultadoFinal) :-
    reverse(ResultadoParcial, ResultadoFinal).
clasificaAux([Temperatura|Resto], ResultadoParcial, ResultadoFinal) :-
    clasificar(Temperatura, Clasificacion),
    actualizarContador(Clasificacion, ResultadoParcial, ResultadoParcialActualizado),
    clasificaAux(Resto, ResultadoParcialActualizado, ResultadoFinal).

clasificar(Temperatura, helado) :-
    Temperatura =< 0.
clasificar(Temperatura, frio) :-
    Temperatura > 0,
    Temperatura < 10.
clasificar(Temperatura, templado) :-
    Temperatura >= 10,
    Temperatura =< 25.
clasificar(Temperatura, calido) :-
    Temperatura > 25.

actualizarContador(Clasificacion, [], [[Clasificacion, 1]]) :- !.
actualizarContador(Clasificacion, [[Clasificacion, Cantidad]|Resto], [[Clasificacion, NuevaCantidad]|Resto]) :-
    NuevaCantidad is Cantidad + 1.
actualizarContador(Clasificacion, [[OtraClasificacion, Cantidad]|Resto], [[OtraClasificacion, Cantidad]|ResultadoParcial]) :-
    Clasificacion \= OtraClasificacion,
    actualizarContador(Clasificacion, Resto, ResultadoParcial).

buscar_elemento(Elemento, [Elemento|_]) :- !.
buscar_elemento(Elemento, [Cabecera|_]) :-
    is_list(Cabecera),
    buscar_elemento(Elemento, Cabecera).
buscar_elemento(Elemento, [_|Resto]) :-
    buscar_elemento(Elemento, Resto).




puntaje(puntito, 1).
puntaje(frutilla, 10).
puntaje(banana, 30).
puntaje(cerezas, 50).
puntaje(fantasma, 100).

puntajePacman(Lista, Puntos) :-
    puntajePacman(Lista, 0, Puntos).

puntajePacman([], Puntos, Puntos).
puntajePacman([bonus|Resto], Acumulado, Puntos) :-
    puntajeBonus(Resto, 5, Acumulado, Puntos).
puntajePacman([Objeto|Resto], Acumulado, Puntos) :-
    Objeto \= bonus,
    puntaje(Objeto, Valor),
    NuevoAcumulado is Acumulado + Valor,
    puntajePacman(Resto, NuevoAcumulado, Puntos).

puntajeBonus([], _, Puntos, Puntos).
puntajeBonus(_, 0, Puntos, Puntos).
puntajeBonus([bonus|Resto], PasosRestantes, Acumulado, Puntos) :-
    puntajeBonus(Resto, (PasosRestantes + 5), Acumulado, Puntos).
puntajeBonus([Objeto|Resto], PasosRestantes, Acumulado, Puntos) :-
    Objeto \= bonus,
    puntaje(Objeto, Valor),
    NuevoAcumulado is Acumulado + (Valor * 2),
    NuevosPasosRestantes is PasosRestantes - 1,
    puntajeBonus(Resto, NuevosPasosRestantes, NuevoAcumulado, Puntos).

count([], 0).
count([_|Tail], N) :-
   N1 is N + 1.
    count(Tail, N1),
 
	

