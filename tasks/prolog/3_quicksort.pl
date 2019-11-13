% Определить предикат qsort(L, K) который для заданного списка целых чисел возвращает отсортированный 
qsort( [], [] ).
qsort( [H|U], S ) :- splitBy(H, U, L, R), qsort(L, SL), qsort(R, SR), append(SL, [H|SR], S).


splitBy( _, [], [], []).
splitBy( H, [U|T], [U|LS], RS ) :- U =< H, splitBy(H, T, LS, RS).
splitBy( H, [U|T], LS, [U|RS] ) :- U  > H, splitBy(H, T, LS, RS).

?- qsort([2,1,4,2,51,5],R).
R = [1, 2, 2, 4, 5, 51] 
