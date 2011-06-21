%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Random seed process. OTP gen_server behavior.
%%  @end                      
%% -------------------------------------------------------------------
-module(randgen_seed).
-behavior(gen_server).
-export([
    create/0,
    start_link/3
    ]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("randgen.hrl").


%% @spec init(Args) -> { ok, State, Timeout } 
%% @doc  init random generator process
init([ Uri, MaxValue, Timeout ]) ->
    random:seed(now()),
    { ok, #seed{uri=Uri, max_value=MaxValue, timeout=Timeout}, Timeout }
.


%% @spec start_link(Uri, Max, Timeout) -> Term
%% @doc  Start process
start_link(Uri, Max, Timeout) ->
    gen_server:start_link(?MODULE, [ Uri, Max, Timeout ], [])
.


%% @spec create() -> { ok, Term } 
%% @doc  Create random generator process.
create() ->
    Timeout = random:uniform(10000) + 1000,													
    randgen_sup:start_child([ Timeout ])
.


%% @spec handle_call(Call, From, State) -> { reply, ok, State } 
%% @doc  Dummy handle_call.
handle_call(_Call, _From, State) ->
    { reply, ok, State } 
.


%  > where is it used?
%  It's simple interface for using in future
%  ---------
%% @spec handle_cast(delete, State) -> { stop, normal, State } 
%% @doc  Stop random generator process.
handle_cast( delete, State ) ->
    error_logger:info_msg("~p stopped.", [ self() ]),
    { stop, normal, State }
;
%% @spec handle_cast(Cast, State) -> { noreply, State } 
%% @doc  Dummy handle_cast
handle_cast( _Cast, State ) ->
    { noreply, State }
.


%% @spec handle_info(timeout, State) -> { ok, State, Timeout } 
%% @doc  Random process timeout tick.
handle_info(timeout, State) ->
    seed(State),
    { noreply, State, State#seed.timeout }
;


%% @spec handle_info( Msg, State) -> { noreply, State }
%% @doc Unknown info handler.
%%      Log message and return State.
handle_info(Msg, State) ->
    error_logger:error_msg("unknown info ~p ~n", [Msg]),
    { noreply, State }.


%% @spec terminate(Reason, Args) -> ok
%% @doc  terminate process
terminate( _Reason, _State ) ->
    ok
.


%% @spec code_change(OldVsn, State, Extra) -> { ok, State } 
%% @doc  Code change handler
code_change( _OldVsn, State, _Extra ) ->
    { ok, State }
.


%% @spec seed(State) -> ok 
%% @doc  Generate random number and make http request on api.
seed(#seed{ max_value=Max, uri=Uri }) ->
    Value = random:uniform(Max),
    Request={ Uri, [], "application/json", integer_to_list(Value) },
    case http:request(put, Request, [], []) of
        {ok, _Result} ->
            error_logger:info_msg("Tick ~p ~n", [ [ self(), Value ] ]);
        {error, Error} ->
            error_logger:error_msg("Error ~p ~n", [ [ self(), Error ] ])
    end
.
