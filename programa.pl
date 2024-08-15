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

/************************************PARCIAL 31 MINUTOS*****************************
% Cancion, Compositores,  Reproducciones
cancion(bailanSinCesar, [pabloIlabaca, rodrigoSalinas], 10600177).
cancion(yoOpino, [alvaroDiaz, carlosEspinoza, rodrigoSalinas, theWeeknd], 5209110).
cancion(equilibrioEspiritual, [danielCastro, alvaroDiaz, pabloIlabaca, pedroPeirano, rodrigoSalinas], 12052254).
cancion(tangananicaTanganana, [danielCastro, pabloIlabaca, pedroPeirano], 5516191).
cancion(dienteBlanco, [danielCastro, pabloIlabaca, pedroPeirano], 5872927). 
cancion(lala, [pabloIlabaca, pedroPeirano], 5100530).
cancion(meCortaronMalElPelo, [danielCastro, alvaroDiaz, pabloIlabaca, rodrigoSalinas], 3428854).

% Mes, Puesto, Cancion
rankingTop3(febrero, 1, lala).
rankingTop3(febrero, 2, tangananicaTanganana).
rankingTop3(febrero, 3, meCortaronMalElPelo).
rankingTop3(marzo, 1, meCortaronMalElPelo).
rankingTop3(marzo, 2, tangananicaTanganana).
rankingTop3(marzo, 3, lala).
rankingTop3(abril, 1, tangananicaTanganana).
rankingTop3(abril, 2, dienteBlanco).
rankingTop3(abril, 3, equilibrioEspiritual).
rankingTop3(mayo, 1, meCortaronMalElPelo).
rankingTop3(mayo, 2, dienteBlanco).
rankingTop3(mayo, 3, equilibrioEspiritual).
rankingTop3(junio, 1, dienteBlanco).
rankingTop3(junio, 2, tangananicaTanganana).
rankingTop3(junio, 3, lala).

%PUNTO 1
%cancion(Nombre,Compositores,Reproducciones),
hit(Cancion):-
    cancion(Cancion,_,_),
    forall(rankingTop3(Mes,_,_), rankingTop3(Mes,_,Cancion)).

%PUNTO 2
noEsReconocidaPorCriticos(Cancion):-
    cancion(Cancion, _, Reproducciones),
    not(rankingTop3(_,_,Cancion)),
    Reproducciones >  7000000.

%PUNTO 3
sonColaboradores(Compositor1, Compositor2) :-
    cancion(_,Compositores,_),
    member(Compositor1, Compositores),
    member(Compositor2, Compositores).

%PUNTO 4
% trabajador(nombre,conductor(anios)).
% trabajador(nombre,periodista(anios, titulo)).
% trabajador(nombre,reportero(anios, notas)).

trabajador(tulio, conductor(5)).
trabajador(bodoque, periodista(2,licenciatura)).
trabajador(bodoque,reportero(5,300)).
trabajador(marioHugo, periodista(10,posgrado)).
trabajador(juanin, conductor(0)).

%PUNTO 5
sueldoTotal(Trabajador, SueldoTotal):-
    trabajador(Trabajador, _),
    findall(Sueldo, sueldoPorTrabajo(Trabajador, Sueldo), ListaDeSueldos),
    sumlist(ListaDeSueldos, SueldoTotal).


sueldoPorTrabajo(Trabajador,Sueldo):-
    trabajador(Trabajador, conductor(Anios)),
    Sueldo is Anios * 1000.

sueldoPorTrabajo(Trabajador, Sueldo):-
    trabajador(Trabajador, reportero(Anios, Notas)),
    Sueldo is (Anios * 1000) + (Notas * 100).

sueldoPorTrabajo(Trabajador, Sueldo):-
    trabajador(Trabajador, periodista(Anios, licenciatura)),
    Sueldo is (Anios * 5000) + ((Anios * 5000) * 0.2).

sueldoPorTrabajo(Trabajador, Sueldo):-
    trabajador(Trabajador, periodista(Anios, posgrado)),
    Sueldo is (Anios * 5000) + ((Anios * 5000) * 0.35).

%PUNTO 6
trabajador(joako, ingeniero(0, sistemas)).
trabajador(joako, amanteDeSuNovia(6)).

sueldoPorTrabajo(Trabajador,Sueldo):-
    trabajador(Trabajador, ingeniero(Anios, sistemas)),
    Sueldo is 10000000 + Anios * 0.1.

sueldoPorTrabajo(Trabajador,Sueldo):-
    trabajador(Trabajador, amanteDeSuNovia(Meses)),
    Sueldo is Meses * 99999999999999999999999999.
*/
/**************************************************PARCIAL MOTIVACIONES PIRAMIDALES********************************************
%PUNTO 1
necesidad(respiracion, fisiologico).
necesidad(alimentacion, fisiologico).
necesidad(descanso, fisiologico).
necesidad(reproduccion, fisiologico).

necesidad(integridadFisica, seguridad).
necesidad(empleo, seguridad).
necesidad(salud, seguridad).

necesidad(amistad, social).
necesidad(afecto, social).
necesidad(intimidad, social).

necesidad(confianza, reconocimiento).
necesidad(respeto, reconocimiento).
necesidad(exito, reconocimiento).

necesidad(empatia, autorrealizacion).
necesidad(ego, autorrealizacion).
necesidad(esencia, autorrealizacion).

nivelSuperior(autorrealizacion, reconocimiento).
nivelSuperior(reconocimiento, social).
nivelSuperior(social, seguridad).
nivelSuperior(seguridad, fisiologico).

%PUNTO 2
separacionEntreNecesidades(Necesidad1, Necesidad2, Niveles):-
    necesidad(Necesidad1,Nivel1),
    necesidad(Necesidad2,Nivel2),
    separacionNiveles(Nivel1, Nivel2, Niveles).


separacionNiveles(Nivel, Nivel, 0).
separacionNiveles(Nivel1,Nivel2,Separacion):-
    nivelSuperior(Nivel2,NivelIntermedio),
    separacionNiveles(Nivel1,NivelIntermedio,SepAnterior),
    Separacion is SepAnterior + 1.

%PUNTO 3
necesita(carla, alimentacion).
necesita(carla, descansar).
necesita(carla, empleo).
necesita(juan, afecto).
necesita(juan, exito).
necesita(camila, alimentacion).
necesita(camila, descanso).
necesita(roberto, amistad).
necesita(manuel, libertad).
necesita(charly, afecto).

%PUNTO 4
necesidadMayorJerarquia2(Persona,Necesidad):-
    jerarquiaNecesidad(Persona,Necesidad,JerarquiaMax),
    forall(jerarquiaNecesidad(Persona,_,OtraJerarquia), JerarquiaMax >= OtraJerarquia).    

jerarquiaNecesidad(Persona,Necesidad,Jerarquia):-
    necesita(Persona,Necesidad),
    necesidad(Necesidad,Nivel),
    nivelBasico(NivelBasico),
    separacionNiveles(NivelBasico,Nivel,Jerarquia).

nivelBasico(Nivel):-
    nivelSuperior(_,Nivel),
    not(nivelSuperior(Nivel,_)).

%PUNTO 5
nivel(Nivel):- necesidad(_,Nivel).
persona(Persona):- necesita(Persona,_).

nivelSatisfecho(Persona,Nivel):-
    persona(Persona),
    nivel(Nivel),
    not(nivelConNecesidades(Persona,Nivel)).

nivelConNecesidades(Persona,Nivel):-
    necesita(Persona,Necesidad),
    necesidad(Necesidad,OtroNivel),
    separacionNiveles(OtroNivel,Nivel,_).

%PUNTO 7
cumpleMaslow(Persona):-
    necesita(Persona,Necesidad),
    forall(necesita(Persona,OtraNecesidad),mismoNivel(Necesidad,OtraNecesidad)).

mismoNivel(Necesidad,OtraNecesidad):-separacionEntre(Necesidad,OtraNecesidad,0).

noCumpleMaslow(Persona):-
    persona(Persona),
    necesita(Persona,Necesidad1),
    necesita(Persona,Necesidad2),
    separacionEntre(Necesidad1,Necesidad2,Separacion),
    Separacion > 1.
 
cumpleMaslowTodos:-
    not(noCumpleMaslow(_)).
cumpleMaslowTodos1:-
    forall(persona(Persona),cumpleMaslow(Persona)).
*/

/****************************************PARCIAL INFLUENCERS******************************
usuario(ana, youtube, 3000000).
usuario(ana, instagram, 2700000).
usuario(ana, tiktok, 1000000).
usuario(ana, twitch, 2).
usuario(beto, youtube, 6000000).
usuario(beto, twitch, 120000).
usuario(beto, instagram, 1100000).
usuario(cami, tiktok,2000).
usuario(dani, youtube,100000).
usuario(evelyn, instagram, 1).

influencer(Persona):-
    usuario(Persona, _, _),
    findall(Seguidores, usuario(Persona,_, Seguidores), ListaDeSeguidores),
    ListaDeSeguidores > 10000.

omnipresente(Persona):-
    usuario(Persona, _, _),
    forall(usuario(_, Red, _), usuario(Persona, Red, _)).

exclusive(Persona):-
    usuario(Persona, RedUnica, _),
    forall((usuario(Persona,Red,_), Red \= RedUnica), not(usuario(Persona, Red, _))).

%PUNTO 3
% contenido(persona, redSocial, video(personas,duracion)).
% contenido(persona, redSocial, foto(personas)).
% contenido(persona, redSocial, stream(tematica)).

contenido(ana, tiktok, video([beto,elevyn], 1)).
contenido(ana, tiktok, video([ana], 1)).
contenido(ana, instagram, foto([ana])).
contenido(beto, instagram, foto([])).
contenido(cami, twitch, stream(lol)).
contenido(cami, youtube, video([cami], 5)).
contenido(evelyn, instagram, foto([evelyn, cami])).

tematica(lol).
tematica(minecraft).
tematica(aoe).

%PUNTO 4
adictiva(RedSocial) :-
    contenido(_,RedSocial, video([_], Duracion)),
    Duracion < 3.

adictiva(RedSocial):-
    contenido(_,RedSocial, foto(Personas)),
    length(Personas, TotalPersonas),
    TotalPersonas =< 4.

adictiva(RedSocial):-
    contenido(_, RedSocial, stream(_)).

%PUNTO 5
puedenColaborar(Persona1, Persona2):-
    usuario(Persona2, _,_),
    contenido(Persona1,_, video(Participantes,_)),
    member(Persona2,Participantes),
    Persona1 \= Persona2.

puedenColaborar(Persona1, Persona2):-
    usuario(Persona2,_,_),
    contenido(Persona1,_, foto(Participantes)),
    member(Persona2, Participantes),
    Persona1 \= Persona2.

colaboran(Persona1,Persona2) :- puedenColaborar(Persona1, Persona2).
colaboran(Persona1, Persona2) :- puedenColaborar(Persona2, Persona1).

%PUNTO 6
caminoALaFama(Usuario):-
    influencer(Influencer),
    puedenColaborar(Influencer,Usuario).

caminoALaFama(Usuario) :-
    usuario(Usuario2,_,_),
    puedenColaborar(Usuario2,Usuario),
    caminoALaFama(Usuario2).

% caminoALaFama(Persona):-
%     not(influencer(Persona)),
%     influencer(Influencer),
%     contenido(Influencer,_,foto(Participantes)),
%     member(Persona,Participantes).

% caminoALaFama(Persona) :-
%     not(influencer(Persona)),
%     usuario(PersonaIntermedia,_,_),
%     contenido(PersonaIntermedia, _, foto(Participantes)),
%     member(Persona,Participantes),
%     caminoALaFama(PersonaIntermedia).

% caminoALaFama(Persona):-
%     not(influencer(Persona)),
%     influencer(Influencer),
%     contenido(Influencer,_,video(Participantes,_)),
%     member(Persona,Participantes).

% caminoALaFama(Persona) :-
%     not(influencer(Persona)),
%     usuario(PersonaIntermedia,_,_),
%     contenido(PersonaIntermedia, _, video(Participantes,_)),
%     member(Persona,Participantes),
%     caminoALaFama(PersonaIntermedia).
*/

