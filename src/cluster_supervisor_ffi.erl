-module(cluster_supervisor_ffi).
-export([poll_and_connect/2, start_distribution/2, format_nodes/1, get_local_ip/0]).

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
%% Use longnames since Railway hostnames contain dots (e.g., x.railway.internal)
start_distribution(NodeName, Cookie) ->
    %% First, ensure EPMD is running (it's needed for distribution)
    ensure_epmd_started(),
    
    %% Set kernel inet_dist_listen options to use a fixed port range
    %% This ensures nodes can connect to each other on known ports
    application:set_env(kernel, inet_dist_listen_min, 9100),
    application:set_env(kernel, inet_dist_listen_max, 9200),
    
    case net_kernel:start([binary_to_atom(NodeName, utf8), longnames]) of
        {ok, _Pid} ->
            erlang:set_cookie(node(), binary_to_atom(Cookie, utf8)),
            {ok, node()};
        {error, {already_started, _Pid}} ->
            erlang:set_cookie(node(), binary_to_atom(Cookie, utf8)),
            {ok, node()};
        {error, Reason} ->
            {error, Reason}
    end.

%% Ensure EPMD is started - it's required for Erlang distribution
ensure_epmd_started() ->
    case erl_epmd:names() of
        {ok, _} ->
            %% EPMD is already running
            ok;
        {error, _} ->
            %% Try to start EPMD as a daemon
            case os:find_executable("epmd") of
                false ->
                    %% EPMD not found in path, try common locations
                    ok;
                EpmdPath ->
                    %% Start EPMD as a daemon
                    os:cmd(EpmdPath ++ " -daemon"),
                    timer:sleep(500) % Give it time to start
            end
    end.

%% Get the local IP address by getting hostname and resolving it
get_local_ip() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            %% Try to resolve our hostname to get our IP
            case inet:getaddr(Hostname, inet6) of
                {ok, Ip} -> 
                    {ok, list_to_binary(format_ip(Ip))};
                {error, _} ->
                    case inet:getaddr(Hostname, inet) of
                        {ok, Ip} -> 
                            {ok, list_to_binary(format_ip(Ip))};
                        {error, _} ->
                            %% Fallback: try to get any non-loopback interface
                            get_local_ip_from_interfaces()
                    end
            end;
        {error, _} ->
            get_local_ip_from_interfaces()
    end.

%% Fallback: get IP from network interfaces
get_local_ip_from_interfaces() ->
    case inet:getif() of
        {ok, IfList} ->
            case find_non_loopback_ipv6(IfList) of
                {ok, Ip} -> {ok, list_to_binary(format_ip(Ip))};
                error ->
                    case find_non_loopback_ipv4(IfList) of
                        {ok, Ip} -> {ok, list_to_binary(format_ip(Ip))};
                        error -> {error, nil}
                    end
            end;
        {error, _} ->
            {error, nil}
    end.

find_non_loopback_ipv6([]) -> error;
find_non_loopback_ipv6([{{0,0,0,0,0,0,0,1}, _, _} | Rest]) ->
    find_non_loopback_ipv6(Rest);
find_non_loopback_ipv6([{Ip = {_,_,_,_,_,_,_,_}, _, _} | _]) ->
    {ok, Ip};
find_non_loopback_ipv6([_ | Rest]) ->
    find_non_loopback_ipv6(Rest).

find_non_loopback_ipv4([]) -> error;
find_non_loopback_ipv4([{{127,_,_,_}, _, _} | Rest]) ->
    find_non_loopback_ipv4(Rest);
find_non_loopback_ipv4([{{0,0,0,0}, _, _} | Rest]) ->
    find_non_loopback_ipv4(Rest);
find_non_loopback_ipv4([{Ip = {_,_,_,_}, _, _} | _]) ->
    {ok, Ip};
find_non_loopback_ipv4([_ | Rest]) ->
    find_non_loopback_ipv4(Rest).
