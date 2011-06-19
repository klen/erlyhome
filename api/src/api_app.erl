%%%-------------------------------------------------------------------
%%% @author Kirill KLenov <horneds@gmail.com>
%%%         [http://github.com/klen]
%%%
%%% @copyright 2011 horneds
%%%
%%% @doc Api OTP application.
%%% @end
%%%-------------------------------------------------------------------
-module(api_app).
-author('Kirill Klenov <horneds@gmail.com>').
-behavior(application).
-export([
    start/2,
    stop/1
]).

-define(APPLICATION, ?MODULE).


%% @spec start(_Type::term(), _StartArgs::term()) -> {ok, Pid} | ignore | { error, Error }
%% @doc This function is called whenever an application
%%      is started using application:start/1,2, and should start the processes
%%      of the application.
start(_Type, _StartArgs) ->
    case api_sup:start_link() of
        {ok, Pid} ->
            error_logger:info_msg("api_sup started ~p ~n", [ Pid ]),
            {ok, Pid};
        Error ->
            error_logger:error_msg("error start api_sup ~p ~n", [ Error ]),
            {error, Error}
    end
.


%% @spec stop( _State::term() ) -> ok
%% @doc This function is called whenever an application
%%      has stopped.
stop(_State) ->
    ok
.
