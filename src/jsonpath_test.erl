-module(jsonpath_test).
-export([bench/4]).

bench(M, F, A, N) when N > 0 ->
        L = test_loop(M, F, A, N, []),
        Length = length(L),
        Min = lists:min(L),
        Max = lists:max(L),
        Med = lists:nth(round((Length / 2)), lists:sort(L)),
        Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
        io:format("Range: ~b - ~b mics~n"
                  "Median: ~b mics~n"
                  "Average: ~b mics~n",
                  [Min, Max, Med, Avg]),
        Med.

test_loop(_M, _F, _A, 0, List) ->
        List;
test_loop(M, F, A, N, List) ->
        {T, _Result} = timer:tc(M, F, A),
        test_loop(M, F, A, N - 1, [T|List]).
