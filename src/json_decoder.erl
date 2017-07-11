%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 11:59 AM
%%%-------------------------------------------------------------------
-module(json_decoder).
-author("regupathy.b").

-define(JSON_OBJECT_OPEN,16#7B).
-define(JSON_OBJECT_CLOSE,16#7D).
-define(JSON_ARRAY_OPEN,16#5B).
-define(JSON_ARRAY_CLOSE,16#5D).
-define(JSON_KEY_SPLITTER,16#3A).
-define(JSON_NEXT,16#2C).

-define(SPACE, 16#20).
-define(NEWLINE,16#0A).
-define(CarriageReturn,16#0D).
-define(TapSpace,16#09).
-define(DOUBLE_QUATE,16#22).

-export([scan/2]).

scan(<<>>,[]) ->  {incomplete,[]};
scan(Source,[]) ->  handle_response(router(Source,[],[]));
scan(Source,{DataHolder,Stack}) ->  handle_response(resume(Source,DataHolder,Stack)).

resume(Source,DataHolder,[{int,Acc}|Stack]) -> integer(Source,Acc,DataHolder,[int]++Stack);
resume(Source,DataHolder,[{float,Acc}|Stack]) -> float(Source,Acc,DataHolder,[float]++Stack);
resume(Source,DataHolder,[{string,Acc}|Stack]) -> string(Source,Acc,DataHolder,[string]++Stack);
resume(Source,DataHolder,[true|Stack]) -> true(Source,DataHolder,[true]++Stack);
resume(Source,DataHolder,[false|Stack]) -> false(Source,DataHolder,[false]++Stack);
resume(Source,DataHolder,Stack) -> router(Source,DataHolder,Stack).

handle_response({complete,JsonData}) -> {complete,JsonData};
handle_response({incomplete,DataHolder,Stack}) -> {incomplete,{DataHolder,Stack}};
handle_response({wrong_data,State,H}) -> {wrong_data,{State,H}}.

router(<<?JSON_ARRAY_OPEN,Rest/binary>> ,DataHolder,Stack) ->  router(Rest,start_array(DataHolder),[array]++Stack);
router(<<?JSON_OBJECT_OPEN,Rest/binary>>,DataHolder,Stack) ->  router(Rest,start_object(DataHolder),[object]++Stack);
router(<<?JSON_ARRAY_CLOSE,Rest/binary>>,DataHolder,[array|Stack]) -> router(Rest,finish(DataHolder),Stack);
router(<<?JSON_OBJECT_CLOSE,Rest/binary>>,DataHolder,[object|Stack]) -> router(Rest,finish(DataHolder),Stack);
router(<<?JSON_KEY_SPLITTER,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<?JSON_NEXT,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<?DOUBLE_QUATE,Rest/binary>>,DataHolder,Stack) ->  string(Rest,[],DataHolder,[string]++Stack);
router(<<$t,Rest/binary>>,DataHolder,Stack) ->  true(Rest,DataHolder,[true]++Stack);
router(<<$f,Rest/binary>>,DataHolder,Stack) ->  false(Rest,DataHolder,[false]++Stack);
router(<<$0,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$0],DataHolder,[int]++Stack);
router(<<$1,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$1],DataHolder,[int]++Stack);
router(<<$2,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$2],DataHolder,[int]++Stack);
router(<<$3,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$3],DataHolder,[int]++Stack);
router(<<$4,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$4],DataHolder,[int]++Stack);
router(<<$5,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$5],DataHolder,[int]++Stack);
router(<<$6,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$6],DataHolder,[int]++Stack);
router(<<$7,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$7],DataHolder,[int]++Stack);
router(<<$8,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$8],DataHolder,[int]++Stack);
router(<<$9,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$9],DataHolder,[int]++Stack);
router(<<$.,Rest/binary>>,DataHolder,Stack) ->  float(Rest,[$.],DataHolder,[float]++Stack);
router(<<$-,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$-],DataHolder,Stack);
router(<<$+,Rest/binary>>,DataHolder,Stack) ->  integer(Rest,[$+],DataHolder,Stack);
router(<<?NEWLINE,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<?CarriageReturn,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<?TapSpace,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<?SPACE,Rest/binary>>,DataHolder,Stack) ->  router(Rest,DataHolder,Stack);
router(<<>>,DataHolder,[]) ->  {complete,DataHolder};
router(<<>>,DataHolder,Stack) ->  {incomplete,DataHolder,Stack};
router(<<H,_/binary>>,_DataHolder,[State|_Stack]) -> {wrong_data,State,H}.

string(<<?DOUBLE_QUATE,Rest/binary>>,Acc,DataHolder,[string|Stack]) ->  router(Rest,insert(iolist_to_binary(lists:reverse(Acc)),DataHolder),Stack);
string(<<>>,Acc,DataHolder,[string|Stack]) -> {incomplete,DataHolder,[{string,Acc}]++Stack};
string(<<H,Rest/binary>>,Acc,DataHolder,Stack) ->  string(Rest,[H|Acc],DataHolder,Stack).

true(<<$r,$u,$e,Rest/binary>>,DataHolder,[true|Stack]) -> router(Rest,insert(true,DataHolder),Stack);
true(<<$u,$e,Rest/binary>>,DataHolder,[true|Stack]) -> router(Rest,insert(true,DataHolder),Stack);
true(<<$e,Rest/binary>>,DataHolder,[true|Stack]) -> router(Rest,insert(true,DataHolder),Stack);
true(<<>>,DataHolder,Stack) ->{incomplete,DataHolder,Stack};
true(<<H,_/binary>>,_DataHolder,[State|_Stack]) -> {wrong_data,State,H}.

false(<<$a,$l,$s,$e,Rest/binary>>,DataHolder,[false|Stack]) -> router(Rest,insert(false,DataHolder),Stack);
false(<<$l,$s,$e,Rest/binary>>,DataHolder,[false|Stack]) -> router(Rest,insert(false,DataHolder),Stack);
false(<<$s,$e,Rest/binary>>,DataHolder,[false|Stack]) -> router(Rest,insert(false,DataHolder),Stack);
false(<<$e,Rest/binary>>,DataHolder,[false|Stack]) -> router(Rest,insert(false,DataHolder),Stack);
false(<<>>,DataHolder,Stack) ->{incomplete,DataHolder,Stack};
false(<<H,_/binary>>,_DataHolder,[State|_Stack]) -> {wrong_data,State,H}.

integer(<<$0,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$0|Acc],DataHolder,Stack);
integer(<<$1,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$1|Acc],DataHolder,Stack);
integer(<<$2,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$2|Acc],DataHolder,Stack);
integer(<<$3,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$3|Acc],DataHolder,Stack);
integer(<<$4,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$4|Acc],DataHolder,Stack);
integer(<<$5,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$5|Acc],DataHolder,Stack);
integer(<<$6,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$6|Acc],DataHolder,Stack);
integer(<<$7,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$7|Acc],DataHolder,Stack);
integer(<<$8,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$8|Acc],DataHolder,Stack);
integer(<<$9,Rest/binary>>,Acc,DataHolder,Stack) ->  integer(Rest,[$9|Acc],DataHolder,Stack);
integer(<<$.,Rest/binary>>,Acc,DataHolder,[int|Stack]) ->  float(Rest,[$.|Acc],DataHolder,[float]++Stack);
integer(<<>>,Acc,DataHolder,[int|Stack]) ->  {incomplete,DataHolder,[{int,Acc}]++Stack};
integer(Rest,Acc,DataHolder,[int|Stack]) ->  router(Rest,insert(lists:reverse(Acc),DataHolder),Stack).

float(<<$0,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$0|Acc],DataHolder,Stack);
float(<<$1,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$1|Acc],DataHolder,Stack);
float(<<$2,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$2|Acc],DataHolder,Stack);
float(<<$3,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$3|Acc],DataHolder,Stack);
float(<<$4,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$4|Acc],DataHolder,Stack);
float(<<$5,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$5|Acc],DataHolder,Stack);
float(<<$6,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$6|Acc],DataHolder,Stack);
float(<<$7,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$7|Acc],DataHolder,Stack);
float(<<$8,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$8|Acc],DataHolder,Stack);
float(<<$9,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$9|Acc],DataHolder,Stack);
float(<<$e,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$e|Acc],DataHolder,Stack);
float(<<$E,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$E|Acc],DataHolder,Stack);
float(<<$-,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$-|Acc],DataHolder,Stack);
float(<<$+,Rest/binary>>,Acc,DataHolder,Stack) ->  float(Rest,[$+|Acc],DataHolder,Stack);
float(<<>>,Acc,DataHolder,[float|Stack]) ->  {incomplete,DataHolder,[{float,Acc}]++Stack};
float(Rest,Acc,DataHolder,[float|Stack]) ->  router(Rest,insert(lists:reverse(Acc),DataHolder),Stack).

start_object(Stack) -> [{object, []}] ++ Stack.

start_array(Stack) -> [{array, []}] ++ Stack.

finish([{object, []}]) -> [{}];
finish([{object, []}|Rest]) -> insert([{}], Rest);
finish([{object, Pairs}]) -> lists:reverse(Pairs);
finish([{object, Pairs}|Rest]) -> insert(lists:reverse(Pairs),Rest);
finish([{array, Values}]) -> lists:reverse(Values);
finish([{array, Values}|Rest]) -> insert(lists:reverse(Values),Rest);
finish(_) -> erlang:error(badarg).

insert(Value, []) -> Value;
insert(Key,[{object, Pairs}|Rest]) ->  [{object, Key, Pairs}]++Rest;
insert(Value,[{object, Key, Pairs}|Rest]) ->  [{object, [{Key, Value}]++Pairs}]++Rest;
insert(Value,[{array, Values}|Rest]) ->  [{array, [Value]++Values}]++Rest;
insert(Val, Re) ->  io:format("~n~n  Val ~p ~n~n ~p",[Val,Re]), erlang:error(badarg).




