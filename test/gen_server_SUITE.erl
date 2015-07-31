-module(gen_server_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [{group, gen_server_termination_test}].

groups() ->
    [{gen_server_termination_test,
      [should_terminate_gracefully_when_stopping_oneself,
       should_not_terminate_after_parent_normal_exit,
       should_crash_after_parent_abnormal_exit,
       should_terminate_after_trapping_exit,
       should_crash_with_abnormal_after_trapping_exit]}].

should_terminate_gracefully_when_stopping_oneself(_Config) ->
    [begin
         %% GIVEN
         {ok, Pid} = gen_server_callback:start_link(TrapExit, self()),
         
         %% WHEN
         gen_server_callback:stop(),
         
         %% THEN
         receive
             {Pid, terminate_called, normal} ->
                 ok
         after 5000 ->
                 throw(fail)
         end
     end || TrapExit <- [true, false]].

should_not_terminate_after_parent_normal_exit(_Config) ->
    %% GIVEN
    {ok, Pid} = gen_server_callback:start_link(false, self()),
    ParentPid = spawn(fun() -> link(Pid),
                               receive
                                   stop -> exit(normal)
                               end
                      end),
    %% WHEN

    ParentPid ! stop,

    %% THEN
    ?assert (is_process_alive(Pid)).


should_crash_after_parent_abnormal_exit(_Config) ->
    %% GIVEN
    process_flag(trap_exit, true),
    {ok, Pid} = gen_server_callback:start_link(false, self()),
    ParentPid = spawn(fun() -> link(Pid),
                               receive
                                   stop -> exit(abnormal)
                               end
                      end),
    
    %% WHEN
    ParentPid ! stop,
    
    %% THEN
    receive
        {'EXIT', Pid, abnormal} ->
            ok
    after 10000 ->
            throw(fail)
    end.

should_terminate_after_trapping_exit(_Config) ->
    %% GIVEN
    {ok, Pid} = gen_server_callback:start_link(true, self()),
    ParentPid = spawn(fun() -> link(Pid),
                               receive
                                   stop -> exit(stop_normal)
                               end
                      end),
    
    %% WHEN
    ParentPid ! stop,
    
    %% THEN
    receive
        {Pid, terminate_called, normal} ->
            ok
    after 5000 ->
            throw(fail)
    end.

should_crash_with_abnormal_after_trapping_exit(_Config) ->
    %% GIVEN
    process_flag(trap_exit, true),
    {ok, Pid} = gen_server_callback:start_link(true, self()),
    ParentPid = spawn(fun() -> link(Pid),
                               receive
                                   stop -> exit(stop_abnormal)
                               end
                      end),
    
    %% WHEN
    ParentPid ! stop,
    
    %% THEN
    receive
        {'EXIT', Pid, abnormal} ->
            ok
    after 5000 ->
            throw(fail)
    end.
