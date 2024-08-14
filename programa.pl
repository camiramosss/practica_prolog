% Aquí va el código.

/***********************************EJERCICIO TEG***********************************
continente(americaDelSur).
continente(americaDelNorte).
continente(asia).
continente(oceania).

estaEn(americaDelSur, argentina).
estaEn(americaDelSur, brasil).
estaEn(americaDelSur, chile).
estaEn(americaDelSur, uruguay).
estaEn(americaDelNorte, alaska).
estaEn(americaDelNorte, yukon).
estaEn(americaDelNorte, canada).
estaEn(americaDelNorte, oregon).
estaEn(asia, kamtchatka).
estaEn(asia, china).
estaEn(asia, siberia).
estaEn(asia, japon).
estaEn(oceania,australia).
estaEn(oceania,sumatra).
estaEn(oceania,java).
estaEn(oceania,borneo).

jugador(amarillo).
jugador(magenta).
jugador(negro).
jugador(blanco).

aliados(X,Y):- alianza(X,Y).
aliados(X,Y):- alianza(Y,X).
alianza(amarillo,magenta).

%el numero son los ejercitos
ocupa(argentina, magenta, 5).
ocupa(chile, negro, 3).
ocupa(brasil, amarillo, 8).
ocupa(uruguay, magenta, 5).
ocupa(alaska, amarillo, 7).
ocupa(yukon, amarillo, 1).
ocupa(canada, amarillo, 10).
ocupa(oregon, amarillo, 5).
ocupa(kamtchatka, negro, 6).
ocupa(china, amarillo, 2).
ocupa(siberia, amarillo, 5).
ocupa(japon, amarillo, 7).
ocupa(australia, negro, 8).
ocupa(sumatra, negro, 3).
ocupa(java, negro, 4).
ocupa(borneo, negro, 1).

% Usar este para saber si son limitrofes ya que es una relacion simetrica
sonLimitrofes(X, Y) :- limitrofes(X, Y).
sonLimitrofes(X, Y) :- limitrofes(Y, X).

limitrofes(argentina,brasil).
limitrofes(argentina,chile).
limitrofes(argentina,uruguay).
limitrofes(uruguay,brasil).
limitrofes(alaska,kamtchatka).
limitrofes(alaska,yukon).
limitrofes(canada,yukon).
limitrofes(alaska,oregon).
limitrofes(canada,oregon).
limitrofes(siberia,kamtchatka).
limitrofes(siberia,china).
limitrofes(china,kamtchatka).
limitrofes(japon,china).
limitrofes(japon,kamtchatka).
limitrofes(australia,sumatra).
limitrofes(australia,java).
limitrofes(australia,borneo).
limitrofes(australia,chile).

%PUNTO 1:
loLiquidaron(Jugador):-
    jugador(Jugador),
    not(ocupa(_,Jugador,_)).

%PUNTO 2:
ocupaContinente(Jugador,Continente):-
    jugador(Jugador),
    forall(estaEn(Continente, Pais), ocupa(Pais,Jugador,_)).
%para todos los paises de cada continente, el jugaor ocupa todos los paises
%de todo un universo, vas a tomar los que te interesen

%PUNTO 3:
%que todos sus paises sean de un mismo continente
seAtrinchero(Jugador,Continente):-
    jugador(Jugador),
    forall(ocupa(Pais,Jugador,_), estaEn(Continente,Pais)).

%PUNTO 4:
puedeConquistar(Jugador,Continente):-
    jugador(Jugador),
    forall((estaEn(Continente,Pais), not(ocupa(Pais,Jugador,_))), puedeAtacar(Jugador, Pais)).

puedeAtacar(Jugador,PaisAtacado):-
    ocupa(Pais,Jugador,_),
    sonLimitrofes(PaisAtacado, Pais),
    not(aliados(PaisAtacado,Pais)).

%PUNTO 5:
elQueTieneMasEjercitos(Jugador,Pais):-
    ocupa(Pais,Jugador,EjercitoConMasVida),
    forall((ocupa(_, _, OtroEjercito), EjercitoConMasVida \= OtroEjercito), EjercitoConMasVida > OtroEjercito).
%ponemos _ en el lugar del Jugador y el Pais ya que debe comparar con otros jugadores y paises, sino estaria comparando con el mismo jugador y pais, y no tiene sentido.

%PUNTO 6:
objetivo(amarillo, ocuparContinente(asia)).
objetivo(amarillo,ocuparPaises(2, americaDelSur)). 
objetivo(blanco, destruirJugador(negro)). 
objetivo(magenta, destruirJugador(blanco)). 
objetivo(negro, ocuparContinente(oceania)).
objetivo(negro,ocuparContinente(americaDelSur)). 


cumpleObjetivos(Jugador):-
    jugador(Jugador),
    forall(objetivo(Jugador, Objetivo), cumpleObjetivo(Jugador,Objetivo)).

%Objetivos:
ocuparContinente(Jugador,Continente):-
    jugador(Jugador),
    forall(estaEn(Continente, Pais), ocupa(Pais,Jugador,_)).

ocupaPaises(Jugador,Numero,Continente):-
    jugador(Jugador),
    findall(Pais, (ocupa(Pais,Jugador,_), estaEn(Continente,Pais)), ListaDePaises),
    length(ListaDePaises, NumeroDePaises),
    NumeroDePaises >= Numero.

destruirJugador(Jugador):-
    jugador(Jugador),
    not(ocupa(_,Jugador,_)).

%Si cumplen cada objetivo:
cumpleObjetivo(Jugador, ocuparContinente(Continente)):-
    jugador(Jugador),
    ocuparContinente(Jugador,Continente).   
        
cumpleObjetivo(Jugador,ocupaPaises(Numero,Continente)):-
    jugador(Jugador),
    ocupaPaises(Jugador,Numero,Continente).
    
cumpleObjetivo(Jugador,destruirJugador(JugadorADestruir)):-
    jugador(Jugador),
    destruirJugador(JugadorADestruir).

%PUNTO 7:
leInteresa(Jugador,Continente):-
    objetivo(Jugador, ocuparContinente(Continente)).

leInteresa(Jugador,Continente):-
    objetivo(Jugador, ocuparPaises(_, Continente)).

leInteresa(Jugador,Continente):-
    objetivo(Jugador,destruirJugador(_)),
    forall(ocupa(Pais, Jugador, _), estaEn(Continente,Pais)).
*/

/**********************************EJERCICIO RECETAS************************************
%receta(Nombre,Ingredientes).
receta(caramelo, [ingrediente(agua,100), ingrediente(azucar,100)]).
ingrediente(azucar).
ingrediente(agua).
calorias(azucar,1001).
calorias(agua,1).

%una receta es rapida cuando tiene menos que 4 ingredientes
rapida(Receta):-
    receta(Receta,Ingredientes),
    length(Ingredientes,Total),
    Total < 4.

%una receta es un postre si tiene mas de 250g de azucar
postre(Receta):-
    receta(Receta, Ingredientes),
    member(ingrediente(azucar,Cantidad), Ingredientes),
    Cantidad > 250.

%las recetas son triviales cuando tiene un solo ingrediente
esTrivial(Receta):-
    receta(Receta, [_]).

%el peor ingrediente es el mas calorico
elPeor(Ingredientes,PeorIngrediente):-
    member(PeorIngrediente, Ingredientes),
    calorias(PeorIngrediente, CaloriasDelPeor),
    forall(member(Ingrediente,Ingredientes), (calorias(Ingrediente, Calorias), Calorias =< CaloriasDelPeor)).

%devuelve la cantidad total de calorias de una receta
caloriasTotales(Receta, Total):-
    receta(Receta,Ingredientes),
    findall(Kcal, (member(Ingrediente,Ingredientes), calorias(Ingrediente,Kcal)), CaloriasTotales),
    sumlist(CaloriasTotales, Total). 

%la version light de una receta es aquella sin su peor ingrediente
versionLight(Receta, RecetaLight):-
    receta(Receta, Ingredientes),
    elPeor(Ingredientes, PeorIngrediente),
    findall(Ingrediente, (member(Ingrediente, Ingredientes), Ingrediente \= PeorIngrediente), RecetaLight).

%una receta es una guasada si hay algun elemento con mas de 100 kcal
guasada(Receta):-
    receta(Receta,Ingredientes),
    member(IngredienteMasCalorico, Ingredientes),
    calorias(IngredienteMasCalorico, Calorias),
    Calorias > 1000.
*/

/*****************************EJERCICIO (PARCIAL) LA CARCEL*****************************

% guardia(Nombre).
guardia(bennett).
guardia(mendez).
guardia(george).

% prisionero(Nombre, Crimen).
prisionero(piper, narcotrafico([metanfetaminas])).
prisionero(alex, narcotrafico([heroina])).
prisionero(alex, homicidio(george)).
prisionero(red, homicidio(rusoMafioso)).
prisionero(suzanne, robo(450000)).
prisionero(suzanne, robo(250000)).
prisionero(suzanne, robo(2500)).
prisionero(dayanara, narcotrafico([heroina, opio])).
prisionero(dayanara, narcotrafico([metanfetaminas])).
prisionero(pistolein, narcotrafico([a,b,c,d,e,f])).

%PUNTO 1:
% controla(Controlador, Controlado)
controla(piper, alex).
controla(bennett, dayanara).

controla(Guardia, Otro):-
    prisionero(Otro,_),
    guardia(Guardia), %agregamos para que sea 100% inversible
    not(controla(Otro, Guardia)).
%es inversible para el Otro, pero para el Guardia no lo es, para serlo debemos ligarlo por fuera del not.

%PUNTO 2:
conflictoDeIntereses(Uno, Otro):-
    not(controla(Uno,Otro)),
    not(controla(Otro,Uno)),
    controla(Uno,Tercero),
    controla(Otro,Tercero).

%PUNTO 3:
peligroso(Prisionero):-
    prisionero(Prisionero, _),
    forall(prisionero(Prisionero,Crimen), esGrave(Crimen)).

esGrave(homicidio(_)).
esGrave(narcotrafico(Drogas)) :-
    member(metanfetaminas, Drogas).
esGrave(narcotrafico(Drogas)) :-
    length(Drogas, Cantidad),
    Cantidad > 5.

%PUNTO 4:
ladronDeGuanteBlanco(Prisionero):-
    prisionero(Prisionero,_),
    forall(prisionero(Prisionero,_), (monto(Prisionero,Monto), Monto > 100000)).

monto(Prisionero,Monto):-
    prisionero(Prisionero, robo(Monto)).

%PUNTO 5:
aniosDeCondena(robo(Monto),Anios):- Anios is (Monto // 10000).
aniosDeCondena(homicidio(Persona), Anios) :- not(guardia(Persona)), Anios = 7.
aniosDeCondena(homicidio(Persona), Anios):- guardia(Persona), Anios = 9.
aniosDeCondena(narcotrafico(Drogas), Anios) :- length(Drogas, Anios).

condena(Prisionero,Anios) :-
   prisionero(Prisionero,_),
   findall(Anios, (prisionero(Prisionero,Crimen), aniosDeCondena(Crimen, Anios)), ListaDeAnios),
   sumlist(ListaDeAnios, Anios).

%PUNTO 6:
capoDiTutiLiCapi(ElCapo):-
    prisionero(ElCapo,_),
    not(controla(_,ElCapo)),
    forall((persona(Persona), Persona \= ElCapo), controlaDirectaOIndirecta(ElCapo,Persona)).

controlaDirectaOIndirecta(Persona1,Persona2):-
    controla(Persona1,Persona2).
controlaDirectaOIndirecta(Persona,PersonaIndirecta):-
    controla(Persona, Persona2),
    controlaDirectaOIndirecta(Persona2, PersonaIndirecta).

persona(Persona):- guardia(Persona).
persona(Persona):- prisionero(Persona,_).
*/

/****************************EJERCICIO (PARCIAL) PERSONAS, PARQUES Y ATRACCIONES
%persona(Nombre,Rango,Edad,Altura).
persona(nina,joven,22,1.60).
persona(marcos,ninio,8,1.32).
persona(osvaldo,adolescente,13,1.29).

atraccion(trenFantasma, 12,0).
atraccion(montaniaRusa, 0, 1.30).
atraccion(maquinaTiquetera, 0, 0).
atraccion(toboganGigante, 0, 1.50).
atraccion(rioLento, 0, 0).
atraccion(piscinaDeOlas, 5, 0).

parque(parqueDeLaCosta,trenFantasma).
parque(parqueDeLaCosta,montaniaRusa).
parque(parqueDeLaCosta,maquinaTiquetera).
parque(parqueAcuatico,toboganGigante).
parque(parqueAcuatico,rioLento).
parque(parqueAcuatico,piscinaDeOlas).

puedeSubir(Persona,Atraccion):-
    persona(Persona, _, Edad, Altura),
    atraccion(Atraccion, EdadRequerida, AlturaRequerida),
    Edad >= EdadRequerida,
    Altura >= AlturaRequerida.

esParaElle(Persona,Parque):-
    persona(Persona,_,_,_),
    parque(Parque,_),
    forall(parque(Parque,Atraccion), puedeSubir(Persona,Atraccion)).

malaIdea(GrupoEtareo, Parque):-
    parque(Parque, Atraccion),
    persona(_,GrupoEtareo, _, _),
    forall(persona(Persona, GrupoEtareo, _, _), not(puedeSubir(Persona, Atraccion))).

%PROGRAMAS
programa(parqueAcuatico, [toboganGigante, piscinaDeOlas, rioLento]).
programa(parqueDeLaCosta, [rioLento, montaniaRusa, rioLento, trenFantasma]).
programa(parqueRandom, [montaniaRusa, piscinaDeOlas, toboganGigante]).


programaLogico(Programa):-
    esBueno(Programa),
    not(atraccionRepetida(Programa)).

esBueno(Programa) :-
    programa(_,Atracciones),
    forall(member(Atraccion,Programa), member(Atraccion,Atracciones)).

hayRepetidos([X|XS]) :- member(X,XS).
hayRepetidos([_|XS]) :- hayRepetidos(XS).

atraccionRepetida(Programa) :-
    programa(Programa, Atracciones),
    hayRepetidos(Atracciones).

hastaAca(Persona, [], []).
hastaAca(Persona, [X|XS], []) :- not(puedeSubir(Persona,X)).
hastaAca(Persona, [X|XS], [X|YS]) :- puedeSubir(Persona, X), hastaAca(Persona, XS, YS).

%PASAPORTES
pasaporte(nina, basico(1000)).
pasaporte(marcos, flex(1500, karting)).
pasaporte(osvaldo, premium).

juego(pingpong, comun(200)).
juego(pool, comun(400)).
juego(karting, premium).

%para juegos comunes
puedeSubir(Persona, Atraccion):-
    juego(Atraccion, comun(Creditos)),
    pasaporte(Persona, basico(CreditosPersona)),
    CreditosPersona >= Creditos.

puedeSubir(Persona, Atraccion):-
    juego(Atraccion, comun(Creditos)),
    pasaporte(Persona, flex(CreditosPersona, _)),
    CreditosPersona >= Creditos.

puedeSubir(Persona, Atraccion):-
    juego(Atraccion, _),
    pasaporte(Persona, flex(_, Atraccion)).

%para juegos premium
puedeSubir(Persona, Atraccion):-
    juego(Atraccion, _),
    pasaporte(Persona, premium).
*/