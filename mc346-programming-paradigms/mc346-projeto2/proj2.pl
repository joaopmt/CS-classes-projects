% Artur Lima 166916
% Joao Pedro Martins 176117
% --------------------------- 

% Tenta gerar a string S que eh a concatenacao das strings S1 e S2 por
% uma substring em comum Ss de tamanho >= 4 que esta no fim de S1 e no
% comeco de S2
% unite(+String1,+String2,-UnionString)
unite(S1,S2,Sf) :- string_length(S1,L), unite(S1,S2,L,Sf).
unite(S1,S2,L,Sf) :-
    L >= 4,
    ((sub_string(S1, _, L, 0, Ss), !, sub_string(S2, _, _, _, Ss), !,
      sub_string(S1, _, _, L, Ss1), !, sub_string(S2, L, _, 0, Ss2), !,
      string_concat(Ss1,Ss,S), string_concat(S,Ss2,Sf))
	-> true
	; (LL is L-1, unite(S1,S2,LL,Sff), Sf = Sff)).


% Inner loop que tenta unir a string no indice I do db
% com cada uma das outras strings, indexadas por J.
% Se for possivel unir a string em I com a string em J,
% a string em J eh substituida por "" (valor dummy) e
% a string em I eh substituida pela uniao S no db dinamico
% iter_inner(+Index1,+Index2,+Range)
iter_inner(_,Range,Range).
iter_inner(I,I,Range) :- II is I+1, iter_inner(I,II,Range).
iter_inner(I,J,Range) :-
    s(I,S1), s(J,S2), (unite(S1,S2,S)
                      -> retract(s(J,S2)), assertz(s(J,"")),
                         retract(s(I,S1)), assertz(s(I,S)),
                         iter_inner(I,0,Range)
                      ; JJ is J+1, iter_inner(I,JJ,Range)).


% itera em I chamando iter_inner com indices (I,0) a cada iteracao
% iter_outer(+Index,+Range)
iter_outer(Range,Range).
iter_outer(I,Range) :- II is I+1, iter_inner(I,0,Range), iter_outer(II,Range).


% Limpa o database dinamico
clear_db() :- retractall(s(_,_)).


% Unifica Size com o numero de strings no database
% get_dbsize(-Size)
get_dbsize(Size) :- findall(_, s(_,_), L), length(L, Size).


% Unifica L com a lista de todas as strings do database
% db_to_list(-List)
db_to_list(L) :-  findall(S, s(_,S), L).


% Coloca as strings recebidas no BD, indexadas de 0 a n-1
% db_input(+List)
db_input(XS) :- db_input(XS,0).
db_input([], _).
db_input([X|XS], I) :- assertz(s(I,X)), II is I+1, db_input(XS, II).


% True se a string eh apenas um dummy e nao deve ser printada
% is_dummy(+String)
is_dummy("").


% deletelast pq o ultimo valor da lista eh um \n que não queremos pegar
% deletelast(+List1,-List2)
deletelast([X|XS],YS) :- dl(XS,YS,X).
dl([],[],_).
dl([X1|XS], [X0|YS],X0) :- dl(XS,YS,X1).


% O predicado s(Indice,String) indexa as strings e sera usado
% como banco de dados dinamico
:- dynamic s/2.

main :- read_string(user_input,_,String), main_function(String).

main_function(String) :-
    clear_db(),
    split_string(String,"\n","",List),
    deletelast(List,L),
    db_input(L),
    get_dbsize(Size),
    iter_outer(0,Size),
    db_to_list(LL),
    exclude(is_dummy, LL, LLL),nl,
    atomic_list_concat(LLL, '\n', Input),
    write(Input).
