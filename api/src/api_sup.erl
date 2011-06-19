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


%% @spec start_link() -> {ok, Pid} | ignore | { error, Other }
%% @doc Supervisor start link
start_link() ->
    supervisor:start_link({ local, ?SERVER }, ?MODULE, [  ])
.


%% @spec init([]) -> {ok, { RestartStrategy, Children } }
%% @doc Get application configuration. And init supervisor.
init([]) ->

    % Parse application options
    { ok, Opts } = get_config(),
    { ok, Limit } = opt(Opts, num_seq),

    SequenceServer = { api_server, { api_server, start_link, [ Limit ] }, permanent, 2000, worker, [ api_server ] },
    Children = [ SequenceServer ],
    RestartStrategy = { one_for_one, 4, 3600 },

    { ok, { RestartStrategy, Children } }
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
            { ok, Terms }
    end
.


%% @spec opt(Opts, Key) -> { ok, Value } | { undefined, undefined }
%% @doc  get option from options list
opt([ { Key, Value } | _Rest ], Key) ->
    { ok, Value } 
;
opt([ _H | [] ], _Key) ->
    { undefined, undefined }
;
opt([ _H | Rest ], Key) ->
    opt(Rest, Key)
.
