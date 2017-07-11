%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2015 12:17 PM
%%%-------------------------------------------------------------------
-module(json_encoder).
-author("regupathy.b").

%% API
-export([build/1]).

-spec build(DataSet::term()) -> {ok,Data::binary()} | {wrong_data,Reason::any()}.
build(DataSet) -> {ok,form_json(DataSet)}.


whichJsonType([]) -> empty;
whichJsonType(A)when is_tuple(A) -> jsonElement;
whichJsonType(A)when is_tuple(hd(A)) -> jsonObject;
whichJsonType(A)when is_list(hd(A)) -> jsonGroup;
whichJsonType(A)when is_binary(hd(A)) -> jsonGroup;
whichJsonType(_) -> value.

form_json(JsonData) ->
  case whichJsonType(JsonData) of
    empty -> [];
    jsonElement -> formJsonObject([JsonData]);
    jsonObject -> formJsonObject(JsonData);
    jsonGroup -> formJsonGroup(JsonData);
    value -> JsonData;
    _ -> []
  end.

formJsonGroup(JsonData) -> formJsonGroup(JsonData,<<"[">>,<<>>).

formJsonGroup([[]],String,_) -> <<String/binary, "]">>;
formJsonGroup([],String,_) -> <<String/binary, "]">>;
formJsonGroup([H|T],String,Prefix) ->
  NewString = <<String/binary,Prefix/binary,(form_json(H))/binary>>,
  formJsonGroup(T,NewString,<<",">>).

formJsonObject(JsonData)when is_tuple(hd(JsonData)) ->
  <<<<"{">>/binary,(formJsonElements(JsonData))/binary,<<"}">>/binary>>;
formJsonObject(JsonData) ->  <<<<"\"">>/binary,(getBin(JsonData))/binary, <<"\"">>/binary>>.

formJsonElements(JsonData) -> formJsonElements(JsonData,<<>>,<<>>).
formJsonElements([],String,_Prefix) -> String;
formJsonElements([{Key,Value}|Rest],String,Prefix) ->
  JsonKey = getBin(Key),
  JsonValue = formJSONvalue(Value),
  NewString = <<String/binary,Prefix/binary,<<"\"">>/binary,JsonKey/binary,<<"\" : ">>/binary,JsonValue/binary>> ,
  formJsonElements(Rest,NewString,<<",">>).

formJSONvalue(Value) -> formJSONvalue(whichJsonType(Value),Value).

formJSONvalue(value,Value)when is_integer(Value) ->  Val = getBin(Value), <<Val/binary>>;
formJSONvalue(value,Value) -> Val = getBin(Value), <<<<"\"">>/binary,Val/binary,<<"\"">>/binary>>;
formJSONvalue(jsonGroup,Value) -> formJsonGroup(Value);
formJSONvalue(jsonObject,Value) -> formJsonObject(Value);
formJSONvalue(_,Value) -> getBin(Value).

getBin(Value) when is_atom(Value) -> atom_to_binary(Value,latin1);
getBin(Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
getBin(Value) when is_float(Value) -> float_to_binary(Value);
getBin(Value) when is_binary(Value)-> Value;
getBin(Value) when is_list(Value)-> list_to_binary(Value);
getBin(Value) ->term_to_binary(Value).

