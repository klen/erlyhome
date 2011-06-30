%%%-------------------------------------------------------------------
%%% @author Kirill KLenov <horneds@gmail.com>
%%%         [http://github.com/klen]
%%%
%%% @copyright 2011 horneds
%%%
%%% @doc Api root supervisor. Spawn server process.
%%% @end
%%%-------------------------------------------------------------------
-module(api_sup).
-author('Kirill Klenov <horneds@gmail.com>').
-behavior(supervisor).
-export([
        start_link/0
]).
-export([
        init/1
]).

-define( SERVER, ?MODULE ).
-define( DEFAULT_LIMIT, 10 ).


%% @spec start_link() -> {ok, Pid} | ignore | { error, Other }
%% @doc Supervisor start link
start_link() ->
    supervisor:start_link({ local, ?SERVER }, ?MODULE, [  ])
.


%% @spec init([]) -> {ok, { RestartStrategy, Children } }
%% @doc Get application configuration. And init supervisor.
init([]) ->

    % Parse application options

    % >> are you sure that here we will always have {ok, _} as a result
    % Im not sure, but error here is the result of incorrectly configured application.
    % In this situation, exception is better (IMHO). 
    % ----------
    
    { ok, Opts } = get_config(),
    Limit = proplists:get_value(num_seq, Opts, ?DEFAULT_LIMIT),

    % >> just style change
    % >> if there's more than one child, the following style is better (IMHO)
    % Ok, not problem for me, because i have no coding style in erlang
    % ----------
    
    { ok, { { one_for_one, 4, 3600 }, [
                { api_server, { api_server, start_link, [ Limit ] }, 
                    permanent, 2000, worker, [ api_server ] }
                %, {...}
                %, {...}
            ] } }
.


%% @spec get_config() -> { ok, Terms } 
%% @doc  Get application config from conf or app file.
get_config() ->
    case file:consult("priv/api.conf") of
        { ok, Terms } ->
            { ok, Terms };
        { error, Error } ->
            error_logger:info_msg("error ~p", [ Error ]),
            Terms = application:get_all_env(api),
            % >> I'd better use here ?MODULE instead of 'api'
            % ?MODULE here have value 'api_sup' will it work?
            % ----------
            
            { ok, Terms }
    end
.
