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
    [io:format("~5s ~9s ~s~n", [element(1, E), filesize(element(3, E)), element(2, E)])
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
                Str ->
                    case lists:last(Str) of
                        0 -> Cmd_ = lists:sublist(Str, length(Str)-1);
                        _ -> Cmd_ = Str
                    end,
                    Cmd = re:replace(Cmd_, "\\x00", " ", [global, {return, list}])
            end;
        {error, _} -> Cmd = ""
    end,
    H = line_server:get_handle("/proc/"++Pid++"/smaps"),
    Size = getswapsize(H, 0) * 1024,
    {Pid, Cmd, Size}.

getswapsize(H, Acc) ->
    case line_server:get_lines(H) of
        eof -> Acc;
        Lines ->
            Size = swapsize(Lines),
            getswapsize(H, Acc + Size)
    end.

swapsize(Lines) ->
    swapsize(Lines, 0).

swapsize([H|T], Acc) ->
    Hsize = getswapsize_bin(H),
    swapsize(T, Acc + Hsize);
swapsize([], Acc) ->
    Acc.

getswapsize_bin(<<"Swap:",  Sizebin/binary>>) ->
    Sizestr = erlang:binary_to_list(Sizebin),
    erlang:list_to_integer(string:sub_word(Sizestr, 1));
getswapsize_bin(_) ->
    0.
