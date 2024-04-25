:- [wordlist].

remove_first(_, [], []) :- !.
remove_first(X, [X | Tail], Tail) :- !.
remove_first(X, [Y | Tail], [Y | NewTail]) :- remove_first(X, Tail, NewTail).

count_frequency(List, Counts) :-
    msort(List, Sorted),
    clumped(Sorted, Counts).

score([], [], _, []).
score([G|Gs], [T|Ts], W, [S|Ss]) :-
  (   G == T ->
      S = f
    ; member(G, W) ->
      ( remove_first(G, W, W),
        S = p
      )
    ; S = i
  ),
  score(Gs, Ts, W, Ss).
score(G, T, S) :-
  atom_chars(G, Gc),
  atom_chars(T, Tc),
  score(Gc, Tc, Tc, Sc),
  atom_chars(S, Sc).

entropy(G, E) :-
  findall(S, (words(T), score(G, T, S)), Ss),
  length(Ss, L),
  count_frequency(Ss, Pairs),
  pairs_values(Pairs, Cs),
  maplist({L}/[C, R] >> (R is C / L), Cs, Ps),
  foldl([P, A, R] >> (R is A + (P * log(P))), Ps, 0, NE),
  E is -NE.

possible([], [], [], _).
possible([G|Gs], [i|Rs], [W|Ws], FW) :- G \= W, \+ member(G, FW), possible(Gs, Rs, Ws, FW), !.
possible([G|Gs], [p|Rs], [W|Ws], FW) :- G \= W, member(G, FW), possible(Gs, Rs, Ws, FW).
possible([G|Gs], [f|Rs], [G|Ws], FW) :- possible(Gs, Rs, Ws, FW).

all_possible(Gs, Ks, W) :-
  maplist({W}/[G, K] >> (atom_chars(G, Gc), atom_chars(K, Kc), possible(Gc, Kc, W, W)), Gs, Ks).

max_entropies_given(Gs, Ks, ME) :-
  findall(E-W, (words(W), atom_chars(W, Wc), all_possible(Gs, Ks, Wc), entropy(W, E)), Es),
  keysort(Es, SEs), % this could be improved to O(n) instead of O(nlogn) but it doesn't really matter
  reverse(SEs, [_-ME|_]).
