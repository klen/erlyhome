%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Randgen OTP application.
%%  @end                      
%% -------------------------------------------------------------------
-module(randgen_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).


%% @spec start(Type, StartArgs) -> {ok, Pid} | Error
%% @doc This function is called whenever an application
%%      is started using application:start/1,2, and should start the processes
%%      of the application.
%%      Create random generators.
start(_Type, _StartArgs) ->
    case supervisor:start_link({local, randgen_sup}, randgen_sup, []) of
        { ok, Pid } ->
            { ok, Num } = application:get_env(num_gen),
            { ok, 0 }= seed(Num),
            { ok, Pid };
        Error ->
            Error
    end
.


%% @spec stop(State) -> ok
%% @doc This function is called whenever an application
%%      has stopped.
stop(_State) ->
  ok.


%% @spec seed(Num) -> ok 
%% @doc  Spawn num of random generator processes
seed(0) ->
    { ok, 0 }
;
seed(Num) ->
    randgen_seed:create(),
    seed(Num - 1)
.
