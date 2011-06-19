%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Root supervisor for random generators
%%  @end                      
%% -------------------------------------------------------------------
-module(randgen_sup).
-behaviour(supervisor).
-export([
    start_child/1
]).
-export([
    init/1
]).

-define(SERVER, ?MODULE).


%% @spec start_child(Args) -> Term
%% @doc  Spawn random generator process
start_child([ Timeout ]) ->
    {ok, Child} = supervisor:start_child( ?SERVER, [ Timeout ] ),
    error_logger:info_msg("Child ~p started.~n", [ Child ])
.


%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using
%%      supervisor:start_link/[2,3], this function is called by the new process
%%      to find out about restart strategy, maximum restart frequency and child
%%      specifications.
init([]) ->
    % Start inets for http
    inets:start(),

    % Parse application options
    Opts = application:get_all_env(randgen),
    { ok, Uri } = opt(Opts, uri),
    { ok, Max } = opt(Opts, max_value),

    RandomSeed = { randgen_seed, { randgen_seed, start_link, [ Uri, Max ] },
                    temporary, brutal_kill, worker, [ randgen_seed ]},
    Children = [ RandomSeed ],
    RestartStrategy = { simple_one_for_one, 0, 1},
    {ok, { RestartStrategy, Children }}.


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
