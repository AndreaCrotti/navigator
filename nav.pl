#!/usr/bin/env swipl -L0 -q -g main -s

/* Schema di funzionamento generale :
	Parsing dell'input =>
	scrittura di distanze e citta su file temporaneo =>
	consultazione =>
	esecuzione di floyd Warshall e consultazione delle minime distanze =>
	calcolo progressivo del percorso ottimale =>
	scrittura su output del risultato ottenuto
*/

%% file utilizzati nella computazione
dist_file('dist_file.pl').
new('new.pl').
last('last.pl').
%% costante alta
high(1000).

%% Main function, automatically called with the shebang
main(['--help']) :-
	writeln('usage:\n ./nav.pl [stradario] [percorso], se non passati uso i valori di default').

%% if no arguments are passed
main([]) :-
	main(['stradario.txt','percorso.txt']).
	
main([Stradario, Percorso]) :-
	%% open the dist file and writes basic informations
	dist_file(D), tell(D),  
	write_pred(stradario,[Stradario]), write_pred(percorso,[Percorso]), told, consult(D),
	%% reopen the same file and append the "map"
	append(D), get_stradario(N), told, consult(D),
	%% most important part of the program, calculates minimum distances
	%% between every node
	floyd_warshall(0,N),
	%% gets the path from the input file and calculates it
	get_path(Path), path_to_idx(Path,Pidxs), shortest_path(Pidxs,Dist,ShortestIdx),
	%% finally output the result of the calculation
	to_cities(ShortestIdx,Shortest), Path=[H | _], append([H],Shortest,Shortest2), to_out(Shortest2,Dist).

%% parsing input
get_stradario(N) :-
	stradario(X), see(X), get_num(N), get_cities(N,[]), get_tab(N,N,[]), seen.

get_path(Path) :-
	percorso(X), see(X), get_percorso([],Init), seen, maplist(atom_codes,Path,Init).

get_percorso(Temp,P) :-
	get_word([],W),
	(W == end_of_file ->
		reverse(Temp,P);
		get_percorso([W | Temp],P)
	).
	
%% fa il parsing delle citta e scrive in output con out_cities
get_cities(0,Temp) :- reverse(Temp,X), out_cities(X), out_idx(X,0).
get_cities(N,Temp) :-
	N > 0, M is N-1, get_word([],C), get_cities(M,[C | Temp]).

get_num(N) :-
	get_word([],M), number_chars(N,M).

%% richiamo N volte get_row, costruendo ricorsivamente la tabella finale
get_tab(N,0,T) :- reverse(T,Tab),to_dist(0,N,Tab).
get_tab(N,Count,T) :-
	Count > 0, M is Count-1, get_row(N,[],C), get_tab(N,M,[C | T]).

get_row(0,T,C) :- reverse(T,C).
get_row(N,T,C) :-
	N > 0, M is N-1, get_num(X), get_row(M,[X | T],C).

%% scrivo in output la lista di citta
out_cities(Cities) :-
	maplist(atom_codes,Y,Cities), term_to_atom(Y,T), %% trasformo liste di caratteri in atomi
	string_concat(T,').',Z), string_concat('cities(',Z,Ris), writeln(Ris).

%% scrive le associazioni fra citta' e indice
out_idx([],_).
out_idx([C | Rest],Count) :-
	list_to_string(C,S), write_pred(idx,[S,Count]),
	Coun is Count+1, out_idx(Rest, Coun).
	
%% caso base, termina la computazione quando ho scritto tutto
to_dist(N,N,_). 
to_dist(Count,N,[T | Ab]) :-
	NewC is Count + 1, write_dist(0,Count,T), to_dist(NewC,N,Ab).

write_dist(_,_,[]).
write_dist(Count,C,[H | T]) :-
	H >= 0,NewC is Count+1,	write_pred(dist,[C,Count,H]), write_dist(NewC,C,T).
write_dist(Count,C,[H | T]) :-
	H < 0, NewC is Count+1, write_dist(NewC,C,T). %% non scrivo nulla in caso di distanza negativa



%% given a path it returns the shortest distances and the city traversed
shortest_path(Path,D,Tappe) :-
	%% FIXME High is just a stupid high costant
	Path = [H,H1 | T], sh_path(H,H1,D1,Tappe1), high(High),
	(D1 == High ->
		write('percorso non totalmente percorribile'),
			(var(D) -> 
				D = 0); %% necessario nel caso già la seconda tappa sia irraggiungibile
		(T == [] ->
			Tappe = Tappe1, D = D1; %% caso di soli due elementi 
			shortest_path([H1 | T],D2,Tappe2), %% caso di 3 o piu elementi
			D is D1 + D2, append(Tappe1,Tappe2,Tappe)
		)
	).

%% chiama il predicato min_dist con K da 0 a Len e consulta il risultato
floyd_warshall(Len,Len) :- last(Last), consult(Last).
floyd_warshall(K,Len) :-
	last(Last), consult(Last), new(New), tell(New), cities(C),
	path_to_idx(C,I), min_dist(K,I,I), told, rename_file(New,Last), J is K+1, floyd_warshall(J,Len).

%% calculates the minimum distance and writes it to file
min_dist(_,[],_). %% caso base
min_dist(K,[_ | T1], []) :- cities(C), path_to_idx(C,I), min_dist(K,T1,I). %% ho consumato la sottolista

min_dist(0,Cities,[C2 | T2]) :-
		Cities=[C1 | _], not(dist(C1,C2,_)), high(High), write_min_dist(C1,C2,High,[C2]), min_dist(0,Cities,T2).
min_dist(0,Cities, [C2 | T2]) :-
	Cities=[C1 | _], dist(C1,C2,Dist), write_min_dist(C1,C2,Dist,[C2]), min_dist(0,Cities,T2).

min_dist(K, Cities, [C2 | T2]) :-
	Cities=[C1|_], K > 0, J is K-1,
	sh_path(C1,C2,Diretto,InfraD), sh_path(C1,J,Before,InfraB), sh_path(J,C2,After,InfraA), S is Before+After, 
	(min(Diretto,S,Diretto) ->
 		write_min_dist(C1,C2,Diretto,InfraD), min_dist(K,Cities,T2);
		append(InfraB,InfraA,Total), write_min_dist(C1,C2,S,Total), min_dist(K,Cities,T2)
	).

write_min_dist(C1,C2,D,Infra) :-
	write_pred(sh_path,[C1,C2,D,Infra]).

%% ottiene una parola alla volta
get_word(Temp, Word) :-
	get_char(C),
	(C == '.' -> 
		reverse(Temp,Word);
	C == end_of_file ->
		Word=C;
	get_word([C | Temp],Word)
	).

min(X,Y,X) :- Y >= X.
min(_,Y,Y).

%% mappa la lista di citta nella corrispondente lista di indici
path_to_idx(P,Idx) :- maplist(idx,P,Idx).

%% dall'indice alla citta
to_city(Idx,City) :-
	cities(X), nth0(Idx,X,City).

to_cities(Idxs,Cities) :-
	maplist(to_city,Idxs,Cities).

list_to_string(L,S) :-
	atom_codes(S,L).

freccie([H | T]) :-
	(T \== [] -> 
		write(H), write('.'), freccie(T); 
		writeln(H)
	).

%% Dato un predicato e i suoi argomenti lo scrive su output nella forma corretta
write_pred(Name,[Last | T]) :- Name == [], T == [], write_term(Last,[quoted(true)]), writeln(').').
write_pred(Name,[Arg | Rest]) :-
	(Name \== [] ->
		write(Name), write('('), write_pred([],[Arg | Rest]) ;
		write_term(Arg,[quoted(true)]), write(','), write_pred(Name,Rest)
	).	
	
%% scrive su output il risultato dati percorso iniziale, calcolato e distanza totale
to_out(Short,Dist) :-
	freccie(Short), writeln(Dist).