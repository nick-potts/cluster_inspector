-module(discovery_ffi).
-export([resolve_all_ips/1, get_local_ip/0]).

%% Resolve a hostname to all its IP addresses
%% Returns list of IP address strings
resolve_all_ips(Hostname) when is_binary(Hostname) ->
    resolve_all_ips(binary_to_list(Hostname));
resolve_all_ips(Hostname) when is_list(Hostname) ->
    %% Try IPv6 first (Railway private network), then IPv4
    case inet:getaddrs(Hostname, inet6) of
        {ok, Addrs} when length(Addrs) > 0 ->
            [format_ip(Addr) || Addr <- Addrs];
        _ ->
            %% Fallback to IPv4
            case inet:getaddrs(Hostname, inet) of
                {ok, Addrs} ->
                    [format_ip(Addr) || Addr <- Addrs];
                {error, _} ->
                    []
            end
    end.

%% Get local IP address
get_local_ip() ->
    case inet:getif() of
        {ok, IfList} ->
            %% Find first non-loopback address
            case find_non_loopback(IfList) of
                {ok, Ip} -> {ok, format_ip(Ip)};
                error -> {error, not_found}
            end;
        {error, _} ->
            {error, not_found}
    end.

find_non_loopback([]) ->
    error;
find_non_loopback([{Ip, _, _} | Rest]) ->
    case Ip of
        {127, _, _, _} -> find_non_loopback(Rest);
        {0, 0, 0, 0} -> find_non_loopback(Rest);
        _ -> {ok, Ip}
    end.

%% Format IP tuple to binary string
format_ip({A, B, C, D}) ->
    %% IPv4
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
format_ip({A, B, C, D, E, F, G, H}) ->
    %% IPv6
    list_to_binary(io_lib:format("~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b", 
                                  [A, B, C, D, E, F, G, H])).
