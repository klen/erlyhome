%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Interface module for API application.
%%  @end                      
%% -------------------------------------------------------------------
-module(api).
-author('Kirill Klenov <horneds@gmail.com>').
-export([
        start/0,
        stop/0,
        fetch/0,
        save/1
    ]).

-define(API_ROOT, api_sup).


%% @spec start() -> ok | Pid 
%% @doc  start application
start() ->
    case whereis(?API_ROOT) of
        undefined ->
            ok = application:start(api);
        Pid ->
            Pid
    end
.
  

%% @spec stop() -> ok 
%% @doc  stop application
stop() -> 
    application:stop(api),
    application:unload(api)
.


%% @spec fetch() -> Result 
%% @doc  Start application if it not loaded
%%       and get saved sequences
fetch() ->
    start(),
    api_server:get()
.


%% @spec save(Value) ->  ok
%% @doc  Push value to api_server
save(Value) ->
    start(),
    api_server:put(Value),
    ok
.
