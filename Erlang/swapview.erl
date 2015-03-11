-module(swapview).
-export([swap_print/0, getswapfor/2]).

is_integer(S) ->
    case string:to_integer(S) of
        {_, []} ->
            true;
        _ ->
            false
    end.

filesize(Size) ->
    Units = "KMGT",
    case filesize(Size, 0) of
        {Fixsize, 0} ->
            io_lib:format("~.0fB", [Fixsize]);
        {Fixsize, Unit} when Unit > 0 ->
            io_lib:format("~.1f~ciB", [Fixsize, lists:nth(Unit, Units)])
    end.

filesize(Size, Unit) 
  when Size < 0 ->
    {Size1, Unit1} = filesize(-Size, Unit),
    {-Size1, Unit1};
filesize(Size, Unit)
  when Unit < 4, Size > 1100 ->
    filesize(Size / 1024, Unit + 1);
filesize(Size, Unit) ->
    {Size, Unit}.

swap_print() ->
    Ret = getswap(),
    io:format("~5s ~9s ~s~n", ["PID", "SWAP", "COMMAND"]),
    [io:format("~5s ~9s ~s~n", [Pid, filesize(Size), Cmd]) || {Pid, Cmd, Size} <- Ret],
    Total = lists:sum([Size || {_,_,Size} <- Ret]),
    io:format("Total: ~8s~n", [filesize(Total)]).

getswap() ->
    {ok, F} = file:list_dir_all("/proc"),
    Pids = lists:filter(fun is_integer/1, F),
    [spawn(?MODULE, getswapfor, [self(), Pid]) || Pid <- Pids],
    S = [read_result() || _ <- Pids],
    lists:keysort(3, [E || E = {_,_,Size} <- S, Size > 0]).

read_result() ->
    receive
        Data ->
            Data
    end.

read_file(File) ->
    case file:read(File, 4096) of
        {ok, Data} ->
            Data ++ read_file(File);
        eof ->
            []
    end.

getswapfor(Master, Pid) ->
    case file:open("/proc/"++Pid++"/cmdline", [read, raw, read_ahead]) of
        {ok, File} ->
            Cmd =
                try
                    string:join(string:tokens(read_file(File), [0]), " ")
                after
                    file:close(File)
                end,

            case file:open("/proc/"++Pid++"/smaps", [read, raw, read_ahead]) of
                {ok, File2} ->
                    Size =
                        try
                            getswapsize(File2, 0) * 1024
                        after
                            file:close(File2)
                        end,
                    Master ! {Pid, Cmd, Size};
                {error, _} ->
                    Master ! 0
            end;
        {error, _} ->
            Master ! 0
    end.

getswapsize(File, Acc) ->
    case file:read_line(File) of
        eof -> Acc;
        {ok, "Swap:" ++ Rest} ->
            {Size, _} = string:to_integer(string:strip(Rest, left, 32)),
            getswapsize(File, Acc+Size);
        {ok, _} ->
            getswapsize(File, Acc)
    end.
