# Json data
JSON Binary streaming data parser and creator

# JSON Encoder
 json_encoder has a export method build/1. it accept list of Erlang term as input.
    
    1> json_encoder:build([{key,value}]).
    2> { "key" : "value"}

# JSON Decoder
  json decoder will parse the binary json data and convert it to Erlang term. It support streaming parsing 
  
      1> {incomplete,ParseData} = json_decoder:scan(<<"{ \"key\" : \"">>,[]).
      2> {complete,Data} = json_decoder:scan(<<"value\"}">>,ParseData).
      3> Data.
      4> [{key,<<"value">>}].
