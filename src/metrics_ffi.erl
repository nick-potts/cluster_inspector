-module(metrics_ffi).
-export([
    get_memory_info/0,
    get_cpu_load/0,
    get_process_count/0,
    get_uptime_seconds/0,
    ensure_os_mon_started/0
]).

%% Ensure os_mon application is started
ensure_os_mon_started() ->
    case application:ensure_all_started(os_mon) of
        {ok, _} -> nil;
        {error, _} -> nil  % May already be started
    end.

%% Get memory information
%% Returns {ok, {TotalMB, UsedMB, Percent}} or {error, nil}
get_memory_info() ->
    try
        case memsup:get_system_memory_data() of
            MemData when is_list(MemData) ->
                Total = proplists:get_value(total_memory, MemData, 0),
                Free = proplists:get_value(free_memory, MemData, 0),
                %% Also check available_memory if free_memory is 0
                Available = case Free of
                    0 -> proplists:get_value(available_memory, MemData, 0);
                    _ -> Free
                end,
                Used = Total - Available,
                TotalMB = Total div (1024 * 1024),
                UsedMB = Used div (1024 * 1024),
                Percent = case Total of
                    0 -> 0.0;
                    _ -> (Used / Total) * 100.0
                end,
                {ok, {TotalMB, UsedMB, Percent}};
            _ ->
                {error, nil}
        end
    catch
        _:_ -> {error, nil}
    end.

%% Get CPU load averages (1, 5, 15 min)
%% Returns {ok, {Load1, Load5, Load15}} or {error, nil}
%% Load values are percentages (0-100+)
get_cpu_load() ->
    try
        %% cpu_sup:avg1/5/15 return values where 256 = 1.00 load
        Load1 = cpu_sup:avg1() / 256.0 * 100.0,
        Load5 = cpu_sup:avg5() / 256.0 * 100.0,
        Load15 = cpu_sup:avg15() / 256.0 * 100.0,
        {ok, {Load1, Load5, Load15}}
    catch
        _:_ -> {error, nil}
    end.

%% Get number of OS processes
get_process_count() ->
    try
        {ok, cpu_sup:nprocs()}
    catch
        _:_ -> 
            %% Fallback to Erlang process count
            {ok, erlang:system_info(process_count)}
    end.

%% Get system uptime in seconds
get_uptime_seconds() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.
