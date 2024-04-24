-module(erlaws_utils).

-export([
    get_timestamp/0,
    encode_hex/1,
    lookup/2,
    lookup/3
]).

get_timestamp() ->
    {{Y,M,D},{H,Mi,S}} = calendar:universal_time(),
    list_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ", [Y,M,D,H,Mi,S])).

encode_hex(Bin) ->
     binary:encode_hex(Bin, lowercase).

lookup(Key, List) ->
    lookup(Key, List, null).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} when Result =/= null ->
            Result;
        _ ->
            Default
    end.
