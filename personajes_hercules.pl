go :- hypothesize(Hercules),
      write('El personaje de la pelicula de Hercules es: '),
      write(Hercules),
      nl,
      undo.
hypothesize(titan_de_roca):- roca, !.
hypothesize(titan_de_hielo):- hielo, !.
hypothesize(titan_de_lava):- lava, !.
hypothesize(titan_de_viento):- viento, !.
hypothesize(filoctetes):- fil, !.
hypothesize(nesso):- nesso, !.
hypothesize(cerbero):- cerbero, !.
hypothesize(pegaso):- pegaso, !.
hypothesize(alcmena):- alcmena, !.
hypothesize(caliope):- caliope, !.
hypothesize(clio):- clio, !.
hypothesize(thalia):- thalia, !.
hypothesize(terpsicore):- terpsicore, !.
hypothesize(melpomene):- melpomene, !.
hypothesize(megara):- meg, !.
hypothesize(pena):- pena, !.
hypothesize(panico):- panico, !.
hypothesize(narrador):- narrador, !.
hypothesize(demetrio):- demetrio, !.
hypothesize(anfitrion):- anfitrion, !.
hypothesize(ciclope):- ciclope, !.
hypothesize(hades):- hades, !.
hypothesize(hercules):- hercules, !.
hypothesize(zeus):- zeus, !.
hypothesize(hermes):- hermes, !.
hypothesize(narciso):- narciso, !.
hypothesize(no_se_encontro_un_personaje_de_la_pelicula_de_hercules).


roca :- titan,
		   verify(parece_una_montaña),
                   verify(tiene_dos_cabezas_y_es_muy_destructivo).

hielo :- titan,
                  verify(tiene_forma_de_huesos_y_espinas_en_la_cabeza),
		   verify(simboliza_las_avalanchas_y_nieve).

lava:- titan,
		   verify(simboliza_las_erupciones_volcanicas).

viento :- titan,
		   verify(simboliza_los_remolinos_tornados_y_huracanes).

nesso :- hombre_animal,
		   verify(es_guardian_del_rio),
		   verify(es_un_centauro).

fil :- hombre_animal,
             verify(es_entrenador_de_heroes),
             verify(es_un_satiro).

cerbero :- animal,
		   verify(es_el_guardian_del_inframundo),
		   verify(es_un_perro_de_3_cabezas).

pegaso :- animal,
		   verify(es_amigo_de_hercules),
                   verify(es_un_caballo_).

alcmena :- femenino,
		   verify(es_una_campesina),
		   verify(es_madre_adoptiva_de_hercules).

caliope :- femenino,
		   verify(es_lider_de_las_musas),
		   verify(es_la_mas_alta_de_las_musas),
                   verify(es_la_musa_de_la_poesia).

clio :- femenino,
		   verify(tiene_una_pequeña_coleta),
                   verify(es_la_musa_de_la_historia).

thalia :- femenino,
                  verify(es_la_mas_baja_de_las_musas),
		   verify(es_la_musa_de_la_comedia).

terpsicore :- femenino,
		   verify(es_la_segunda_mas_baja_de_las_musas),
		   verify(su_peinado_es_con_nudos_bantu_al_frente_y_rizos_atras),
                   verify(es_la_musa_del_baile).

melpomene :- femenino,
		   verify(tiene_largo_cabello_y_rizado),
		   verify(utiliza_una_mascara_con_una_cara_melancolica),
                   verify(es_la_musa_de_la_tragedia).
meg :- inframundo,
             verify(es_cinica_sarcastica_y_cansada_del_mundo),
	     verify(al_final_de_la_pelicula_es_novia_de_hercules).

pena :- inframundo,
	           verify(es_mentiroso),
	           verify(es_un_diablillo_morado).

panico :- inframundo,
	           verify(es_miedoso),
	           verify(es_un_diablillo_azul),
	           verify(su_frase_es_bien_dicho_bicho).

narrador :- masculino,
		   verify(en_la_pelicula_solo_se_escucha_su_voz).

demetrio :- masculino,
		   verify(es_alfarero).

anfitrion :- masculino,
		   verify(es_un_campesino),
		   verify(es_padre_adoptivo_de_hercules).

ciclope :- masculino,
		   verify(tiene_un_solo_ojo),
		   verify(es_gigante),
		   verify(es_una_bola_de_celulitis).
hades :- dioses,
                   verify(es_rencoroso_y_habla_rapido),
                   verify(es_dios_del_inframundo).


hercules :- dioses,
             verify(es_considerado_un_heroe_verdadero),
	     verify(es_hijo_de_zeus_y_hera).


zeus :- dioses,
	           verify(es_dios_del_rayo),
	           verify(es_dios_de_los_cielos),
                   verify(es_papa_de_hercules).

hermes :- dioses,
		   verify(usa_lentes),
		   verify(es_el_mensajero_de_zeus).

narciso :- dioses,
		   verify(es_vanidoso),
		   verify(besa_su_reflejo_en_el_espejo),
                   verify(esta_enamorado_de_si_mismo).

titan :- verify(es_considerado_como_desastres_naturales),!.
hombre_animal :- verify(es_mitad_hombre_mitad_animal),!.
animal :- verify(es_un_animal),!.
femenino :- verify(es_de_sexo_femenino),!.
inframundo :- verify(es_sirviente_de_hades),!.
masculino :- verify(es_de_sexo_masculino),!.
dioses :- verify(es_un_dios),!.

ask(Question) :-
    write(''),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == si ; Response == s )
      ->
       assert(si(Question)) ;
       assert(no(Question)), fail).

:- dynamic si/1,no/1.

verify(S) :-
   (si(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

undo :- retract(si(_)),fail.
undo :- retract(no(_)),fail.
undo.
