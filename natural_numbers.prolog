succ(N, s(N)).

sum(zero, N, N).
sum(s(N), M, s(O)) :- sum(N, M, O).

mul(zero, N, zero).
mul(s(N), M, O2) :- mul(N, M, O), sum(O, M, O2).

dec(s(N), N).

factorial(zero, s(zero)).
factorial(N, O) :- factorial(N, 02), mul(02, s(N), O).

greater(s(_), zero).
greater(s(M), s(N)) :- greater(M, N).

range(A, B, C) :- greater(C, A), greater(B, C).