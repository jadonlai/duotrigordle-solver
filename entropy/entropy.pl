:- [wordlist].

remove_first(_, [], []) :- !.
remove_first(X, [X | Tail], Tail) :- !.
remove_first(X, [Y | Tail], [Y | NewTail]) :- remove_first(X, Tail, NewTail).

count_frequency(List, Counts) :-
    msort(List, Sorted),
    clumped(Sorted, Counts).

% Score the fully correct letters of a given word
% G - guessed word
% T - true word
% R - string of remaining letters
% S - final score
compute_remaining([], [], []).
compute_remaining([G|Gs], [T|Ts], Rs) :-
  G == T,
  !,
  compute_remaining(Gs, Ts, Rs).
compute_remaining([_|Gs], [T|Ts], [R|Rs]) :-
  R = T,
  compute_remaining(Gs, Ts, Rs).

% Score the correct letters of a given word
% G - guessed word
% T - true word
% R - string of remaining letters
% S - final score
score([], [], _, []).
score([G|Gs], [T|Ts], R, [S|Ss]) :-
  (   G == T ->
      S = f
    ; member(G, R) ->
      ( remove_first(G, R, R),
        S = p
      )
    ; S = i
  ),
  score(Gs, Ts, R, Ss).
score(G, T, S) :-
  atom_chars(G, Gc),
  atom_chars(T, Tc),
  compute_remaining(Gc, Tc, R),
  score(Gc, Tc, R, Sc),
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
possible([G|Gs], [i|Ss], [W|Ws], R) :- G \= W, possible(Gs, Ss, Ws, R).
possible([G|Gs], [p|Ss], [W|Ws], R) :- G \= W, member(G, R), possible(Gs, Ss, Ws, R).
possible([G|Gs], [f|Ss], [G|Ws], R) :- possible(Gs, Ss, Ws, R).

all_possible(Gs, Ks, W) :-
  maplist({W}/[G, K] >> (atom_chars(G, Gc), atom_chars(K, Kc), compute_remaining(Gc, W, R), possible(Gc, Kc, W, R)), Gs, Ks).

max_entropies_given(Gs, Ks, ME) :-
  findall(E-W, (words(W), atom_chars(W, Wc), all_possible(Gs, Ks, Wc), entropy(W, E)), Es),
  print(Es),
  keysort(Es, SEs), % this could be improved to O(n) instead of O(nlogn) but it doesn't really matter
  reverse(SEs, [_-ME|_]).
