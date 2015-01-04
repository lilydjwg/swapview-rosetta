%%%-------------------------------------------------------------------
%%% File    : line_server.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description :
%%%
%%% Created : 25 Sep 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(line_server).

-export([get_handle/1,get_lines/1,close/1]).

%% -compile([native]).

-define(BUFFER_SIZE, 70000).

get_handle(FileName) ->
  erlang:spawn_monitor(fun() -> start(FileName) end).

get_lines({Pid,MRef}) ->
  Pid ! {get_lines, MRef, self()},
  receive
    {{Data,Prep}, MRef} ->
      case divide_lines(Data) of
	[H|T] ->
	  [<<Prep/binary,H/binary>>|T];
	[] ->
	  [Prep]
      end;
    {eof,MRef} ->
      eof;
    {'DOWN', MRef, _Type, Pid, _Info} ->
      erlang:error(file_closed)
  end.

close({Pid,_}) ->
  Pid ! stop.

start(FileName) ->
    {ok,F} = file:open(FileName,[raw,binary]),
    serve_lines(F,<<>>).

serve_lines(File,Rest) ->
  {Data, NewRest} =
    get_more_data(File,Rest),
  receive
    {get_lines, Ref, Pid} ->
      Pid ! {Data, Ref},
      serve_lines(File,NewRest);
    stop ->
      file:close(File)
  end.

get_more_data(File,Rest) ->
  case file:read(File, ?BUFFER_SIZE) of
    eof when size(Rest) =:= 0 ->
      {eof,<<>>};
    eof ->
      {{<<>>,Rest},<<>>};
    {error, _} ->
      {eof, <<>>};
    {ok,Bin} ->
      case get_last_line(Bin) of
	{Lines,NR} ->
	  {{Lines,Rest},NR};
	NR ->
	  get_more_data(File,<<Rest/binary,NR/binary>>)
      end
  end.

get_last_line(Bin) ->
  get_last_newline(Bin,size(Bin)-1).

get_last_newline(Bin,S) ->
  case Bin of
    <<Lines:S/binary,10,Rest/binary>> ->
      {Lines,Rest};
    _ ->
      if S =< 0 -> Bin;
	 true -> get_last_newline(Bin,S-1)
      end
  end.

divide_lines(Bin) ->
    divide_lines(Bin, 0, []).

divide_lines(Bin, S, Acc) ->
  case Bin of
    <<_:S/binary,10,_/binary>> ->
      L = S,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,10,_/binary>> ->
      L = S+1,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,10,_/binary>> ->
      L = S+2,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,10,_/binary>> ->
      L = S+3,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,10,_/binary>> ->
      L = S+4,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,_,10,_/binary>> ->
      L = S+5,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,_,_,10,_/binary>> ->
      L = S+6,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,_,_,_,10,_/binary>> ->
      L = S+7,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,_,_,_,_,10,_/binary>> ->
      L = S+8,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_,_,_,_,_,_,_,_,_,10,_/binary>> ->
      L = S+9,
      <<New:L/binary,_,Rest/binary>> = Bin,
      divide_lines(Rest,0,[New|Acc]);
    <<_:S/binary,_:80,_/binary>> ->
      divide_lines(Bin,S+10,Acc);
    <<_:S/binary,_,_/binary>> ->
      divide_lines(Bin,S+1,Acc);
    _ ->
      lists:reverse([Bin|Acc])
  end.
