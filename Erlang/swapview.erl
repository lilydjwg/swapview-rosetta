-module(swapview).
-export([swap_print/0]).

is_integer(S) ->
    try
        _ = list_to_integer(S),
        true
    catch error:badarg ->
        false
    end.

filesize(Size) ->
    Units = "KMGT",
    {Filesize, Unit} = filesize(Size, 0),
    if Size < 0 -> Fixsize = -1.0 * Filesize;
       true -> Fixsize = 1.0 * Filesize
    end,
    if Unit > 0 ->
           float_to_list(Fixsize, [{decimals, 1}])++[lists:nth(Unit, Units)]++"iB";
       Unit =:= 0 ->
           float_to_list(Fixsize, [{decimals, 0}])++"B"
    end.

filesize(Size, Unit) ->
    Left = abs(Size),
    if Unit < 4, Left > 1100 ->
           filesize(Left / 1024, Unit + 1);
       true ->
           {Size, Unit}
    end.

swap_print() ->
    Ret = getswap(),
    io:format("~5s ~9s ~s~n", ["PID", "SWAP", "COMMAND"]),
    [io:format("~5s ~9s ~ts~n", [element(1, E), filesize(element(3, E)), unicode:characters_to_list(list_to_binary(element(2, E)))])
     || E <- Ret],
    Total = lists:foldl(fun(E, Sum) -> element(3, E) + Sum end, 0, Ret),
    io:format("Total: ~8s~n", [filesize(Total)]).

getswap() ->
    {ok, F} = file:list_dir_all("/proc"),
    Pids = lists:filter(fun is_integer/1, F),
    S = lists:map(fun getswapfor/1, Pids),
    lists:keysort(3, [E || E <- S, element(3, E) > 0]).

getswapfor(Pid) ->
    case file:open("/proc/"++Pid++"/cmdline", [read]) of
        {ok, Comm} ->
            case io:get_line(Comm, "") of
                eof -> Cmd = "";
                Str -> Cmd = lists:map(fun(0) ->32;(C) -> C end, Str)
            end;
        {error, _} -> Cmd = ""
    end,
    case file:open("/proc/"++Pid++"/smaps", [read]) of
        {ok, L} -> Size = getswapsize(L) * 1024;
        {error, _} -> Size = 0
    end,
    {Pid, Cmd, Size}.

getswapsize(L) ->
    getswapsize(L, 0).

getswapsize(L, Acc) ->
    case io:get_line(L, "") of
        {error, _} -> file:close(L), 0;
        eof -> file:close(L), Acc;
        "Swap:"++Remainder ->
            Num = string:sub_word(Remainder, 1),
            getswapsize(L, Acc + list_to_integer(Num));
         _ ->
            getswapsize(L, Acc)
    end.
