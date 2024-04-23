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
