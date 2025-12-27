-module(gossip_ffi).
-export([safe_send/1]).

%% Safely send HTTP request, catching any unexpected errors
safe_send(Req) ->
    try
        case gleam@httpc:send(Req) of
            {ok, Resp} -> {ok, Resp};
            {error, _} -> {error, nil}
        end
    catch
        _:_ -> {error, nil}
    end.
