-module(cluster_supervisor_ffi).
-export([poll_and_connect/2, start_distribution/2, format_nodes/1]).

%% Format list of nodes as string
format_nodes(Nodes) ->
    list_to_binary(lists:join(", ", [atom_to_list(N) || N <- Nodes])).

%% Poll DNS for node IPs and connect to discovered nodes
poll_and_connect(Query, NodeBasename) ->
    %% Resolve DNS query to get all IPs
    QueryStr = binary_to_list(Query),
    case inet:getaddrs(QueryStr, inet6) of
        {ok, Addrs} when length(Addrs) > 0 ->
            connect_to_nodes(Addrs, NodeBasename);
        _ ->
            %% Fallback to IPv4
            case inet:getaddrs(QueryStr, inet) of
                {ok, Addrs} ->
                    connect_to_nodes(Addrs, NodeBasename);
                {error, _} ->
                    []
            end
    end.

%% Connect to nodes at the given IP addresses
connect_to_nodes(Addrs, NodeBasename) ->
    BaseAtom = binary_to_atom(NodeBasename, utf8),
    lists:foreach(
        fun(Addr) ->
            NodeName = make_node_name(BaseAtom, Addr),
            %% Don't connect to ourselves
            case NodeName =:= node() of
                true -> ok;
                false ->
                    %% Try to connect (ignore failures - node might not be up yet)
                    net_kernel:connect_node(NodeName)
            end
        end,
        Addrs
    ),
    erlang:nodes().

%% Create a node name from basename and IP address
make_node_name(Basename, Addr) ->
    AddrStr = format_ip(Addr),
    list_to_atom(atom_to_list(Basename) ++ "@" ++ AddrStr).

%% Format IP tuple to string
format_ip({A, B, C, D}) ->
    %% IPv4
    lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
format_ip({A, B, C, D, E, F, G, H}) ->
    %% IPv6 - use compressed format for Railway
    lists:flatten(io_lib:format("~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b", 
                                [A, B, C, D, E, F, G, H])).

%% Start Erlang distribution with the given node name and cookie
start_distribution(NodeName, Cookie) ->
    case net_kernel:start([binary_to_atom(NodeName, utf8), shortnames]) of
        {ok, _Pid} ->
            erlang:set_cookie(node(), binary_to_atom(Cookie, utf8)),
            {ok, node()};
        {error, {already_started, _Pid}} ->
            erlang:set_cookie(node(), binary_to_atom(Cookie, utf8)),
            {ok, node()};
        {error, Reason} ->
            {error, Reason}
    end.
