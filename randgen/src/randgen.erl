%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          [http://github.com/klen]
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Starts the randgen application.
%%  @end                      
%% -------------------------------------------------------------------
-module(randgen).
-export([
    start/0,
    stop/0
    ]).

-define(APPLICATION, ?MODULE).


%% @spec start() -> ok
%% @doc  Start application
start() ->
    application:start(?APPLICATION)
.


%% @spec stop() -> ok 
%% @doc  stop and unload application
stop() ->
    application:stop(?APPLICATION),
    application:unload(?APPLICATION)
.
