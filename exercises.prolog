search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

% search if there's tow consecutive X in the list
search2 (X , cons (X , cons (X , _))).
search2 (X , cons (_ , Xs)) :- search2 (X , Xs).

% search_two (Elem , List )
% looks for two occurrences of Elem with any element in between!
search_two(X, cons(X, cons(Y, cons(X, _)))) :- Y \= X.
search_two(X, cons(_, Xs)) :- search_two(X, Xs).

% search_anytwo (Elem , List )
% looks for any Elem that occurs two times , anywhere
search_anytwo(X, cons(X, Xs)) :- search(X, Xs).
search_anytwo(X, cons(_, Xs)) :- search_anytwo(X, Xs).

%%% size (List , Size )
% Size will contain the number of elements in List, written using notation zero, s(zero), s(s(zero))
size(nil, zero).
size(cons(H, T), s(O)) :- size(T, O).

% sum_list (List , Sum)
sum(zero, N, N).
sum(s(N), M, s(O)) :- sum(N, M, O).

sum_list(nil, zero).
sum_list(cons(H, T), R) :- sum_list(T, O), sum(H, O, R).

% Max is the biggest element in List
% Suppose the list has at least one element
gt(s(_), zero).
gt(s(M), s(N)) :- gt(M, N).

gte(M, N) :- gt(M, N).
gte(M, M).

max(cons(H, nil), H). 
max(cons(H, T), Max) :- max(T, Max), gt(Max, H).
max(cons(H, T), H) :- max(T, Max), gt(H, Max).
max(cons(H, T), H) :- max(T, H).

% gt2 -- the greatest between two numbers.
gt2(A, A, A).
gt2(A, B, A) :- gt(A, B).
gt2(A, B, B) :- gt(B, A).

max2(cons(H, T), R) :- max2(T, H, R).
max2(nil, M, M). 
max2(cons(H, T), M, R) :- gt2(H, M, R2), max2(T, R2, R).

%min-max
lt2(A, B, A) :- gt2(A, B, B). % gt(B, A)
lt2(A, B, B) :- gt2(A, B, A). % gt(A, B)
                              % lt2(A, A, A).

min-max(cons(H, T), RMin, RMax) :- min-max(T, H, H, RMin, RMax).
min-max(nil, Min, Max, Min, Max).
min-max(cons(H, T), Min, Max, RMin, RMax) :- lt2(H, Min, A), gt2(H, Max, B), min-max(T, A, B, RMin, RMax). 


% are the two lists exactly the same ?
same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).
% I suppose `same(X, X)` is too general. It will also consider non-lists.

% all elements in List1 are bigger than those in List2, 1 by 1
all_bigger(nil, nil).
all_bigger(cons(H1, T1), cons(H2, T2)) :- gt(H1, H2), all_bigger(T1, T2).

% List1 should contain elements all also in List2
sublist(nil, L).
sublist(cons(H, T), L) :- search(H, L), sublist(T, L).

% seq(N,E, List ) --> List is [E,E ,... ,E] with size N
seq(zero, _ , nil).
seq(s(N), E, cons(E ,T)) :- seq(N , E, T).

% seqR2 (N, List ) --> is [0 ,1 ,... ,N -1]
seqR(zero, nil).
seqR(s(N), cons(N, L)) :- seqR(N, L). 

% seqR2()
last(nil, E, cons(E, nil)).
last(cons(H, T), E, cons(H, L)) :- last(T, E, L).

seqR2(zero, nil).
seqR2(s(N), X) :- seqR2(N, L), last(L, N, X). 

% last/2 last element of a list
last(cons(H, nil), H).
last(cons(H, T), L) :- last(T, L).

% ----------------------------------------------------------------------

% map [a, b, c, ...] --> [a+1, b+1, c+1, ...]  
f(X, Y) :- sum(X, s(zero), Y). % attempt to generalize...
map(nil, nil).
map(cons(H, T), cons(H2, R)) :- map(T, R), f(H, H2). 
% USAGE: map(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), R).
%        map(nil, R).

% all elements grater than zero.
filter(nil, nil).
filter(cons(H, T), cons(H, R)) :- gt(H, zero), filter(T, R).
filter(cons(H, T), R) :- gte(zero, H), filter(T, R).
% USAGE: filter(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), R).
%        filter(cons(zero, cons(zero, nil)), R).

% size of a list.
count(nil, zero).
count(cons(H, T), s(N)) :- count(T, N).

% how many elements grater than zero?
count_gt(L, R) :- filter(L, R1), count(R1, R).
% USAGE: count_gt(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), R).
%        count_gt(cons(zero, cons(zero, nil)), R).

% find elements grater than zero
find_gt(L, R) :- filter(L, R1), search(R, R1).
% USAGE: count_gt(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), R).
%        count_gt(cons(zero, cons(zero, nil)), R).

% list without the N last elements 
drop_right(L, N, nil) :- count(L, N).
drop_right(cons(H, T), N, cons(H, R)) :- drop_right(T, N, R).
% USAGE: drop_right(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(zero), R).
%        drop_right(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), zero, R).
%        drop_right(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(s(zero)), R).        

% drop element while they're grater than zero.
drop_while_gt(nil, nil).
drop_while_gt(cons(H, T), cons(H, T)) :- gte(zero, H).
drop_while_gt(cons(H, T), R) :- gt(H, zero), drop_while_gt(T, R).
% USAGE: drop_while_gt(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), R).

% partion list in two w.r.t the first element grater than zero. 
partition_gt(nil, nil, nil).
partition_gt(cons(H, T), cons(H, A), B) :- gt(H, zero), partition_gt(T, A, B).
partition_gt(cons(H, T), nil, cons(H, T)) :- gte(zero, H).
% USAGE: partition_gt(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), A, B).        

% reverse the list
reverse(nil, nil).
reverse(cons(H, T), R) :- reverse(T, R1), last(R1, H, R).
% USAGE: reverse(cons(a, cons(b, cons(c, nil))), R).

% drop the first N elements
drop(L, zero, L).
drop(cons(H, T), s(N), R) :- drop(T, N, R).
% USAGE: drop(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(zero), R).
%        drop(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), zero, R).
%        drop(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(s(zero)), R).        

% take the first N elements
take(L, zero, nil).
take(cons(H, T), s(N), cons(H, R1)) :- take(T, N, R1).
% USAGE: take(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(zero), R).
%        take(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), zero, R).
%        take(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), s(s(zero)), R).        

% zip two lists together
zip(nil, nil, nil).
zip(cons(H1, T1), cons(H2, T2), cons((H1, H2), R)) :- zip(T1, T2, R).
% USAGE: zip(cons(a, cons(b, cons(c, nil))), cons(d, cons(e, cons(f, nil))), R).
