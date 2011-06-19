%%%-------------------------------------------------------------------
%%% @author Kirill KLenov <horneds@gmail.com>
%%%         [http://github.com/klen]
%%%
%%% @copyright 2011 horneds
%%%
%%% @doc Main application server. Take number and saves sequence.
%%% @end
%%%-------------------------------------------------------------------
-module(api_server).
-author('Kirill Klenov <horneds@gmail.com>').
-behavior(gen_server).
-export([
    start_link/1,
    update_state/2,
    get/0,
    put/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-include("api.hrl").
-include_lib("eunit/include/eunit.hrl").


%% @spec start_link( Limit ) -> {ok, Pid}
%% @doc Starts the server.
start_link( Limit ) ->
    error_logger:info_msg("Starting api with limit: ~p", [ Limit ]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Limit, [])
.


%% @spec get() -> Result
%% @doc Interface for get saved sequences.
get() ->
    {ok, Result} = gen_server:call( ?SERVER, get ),
    Result
.


%% @spec put(Number::list()) -> Response
%% @doc  Put number in sequences.
put(Number) when is_list(Number) ->
    gen_server:cast(?SERVER, {put, Number})
;
%% @spec put(Number::integer()) -> Response
%% @doc  Convert number in integer and put.
put(Number) when is_integer(Number) ->
    put(integer_to_list(Number) )
.


%% @spec init( Limit ) -> { ok, State::record() }
%% @doc Initialize server.
init( Limit ) ->
    error_logger:info_msg("Init api with limit: ~p", [ Limit ]),
    { ok, #state{ limit=Limit } }
.


%% @spec handle_call( get, _From, State) -> { ok, State::record() }
%% @doc Get saved sequences
handle_call(get, _From, State) ->
    Store = get_store(State),
    { reply, { ok, Store }, State }
.


%% @spec handle_cast( { put, Number }, State::record()) -> { noreply, State::record() }
%% @doc Increase sequence and save results in server state.
handle_cast( { put, Number }, State ) ->
    { ok, NewState } = update_state(Number, State),
    {noreply, NewState}
;


%% @spec handle_cast( Msg, State) -> { noreply, State }
%% @doc Unknown cast handler.
%%      Log message and return State.
handle_cast(Msg, State) ->
    error_logger:error_msg("unknown cast ~p ~n", [Msg]),
    {noreply, State}
.


%% @spec handle_info( Msg, State) -> { noreply, State }
%% @doc Unknown info handler.
%%      Log message and return State.
handle_info(Msg, State) ->
    error_logger:error_msg("unknown info ~p ~n", [Msg]),
    {noreply, State}.


%% @spec terminate( _Reason, _State) -> ok
%% @doc Allways returned ok.
terminate( _Reason, _State ) ->
    ok
.


%% @spec code_change( _OldVsn, State, _Extra ) -> { ok, State }
%% @doc Allways returned { ok, State }.
code_change( _OldVsn, State, _Extra ) ->
    { ok, State }
.


%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------


update_state(Number, State = #state{seq = []}) ->
    { ok, State#state{ seq = [ Number ] } }
;
%% @spec update_state(Number, State) -> { ok, State } 
%% @doc  Get number and return new or old state.
update_state(Number, State) ->
    [ Max | _Rest ] = State#state.seq,
    [ IMax, INumber ] = [ list_to_integer(Max), list_to_integer(Number) ],
    if
        INumber > IMax ->

            % Continue sequence
            { ok, State#state{ seq = [ Number | State#state.seq ] } };

        true ->

            % Create new sequence and update store
            { ok, State#state{ seq = [ Number ], store = get_store(State) } }

    end
.

%% @spec get_store(State) -> Store 
%% @doc  Update store from current sequence and return sorted.
get_store(#state{ store=Store, limit=Limit, seq=Seq }) ->
    SortStore = lists:sort(fun (X, Y)  -> length(X) > length(Y) end, [ Seq | Store ]),
    lists:sublist(SortStore, Limit)
.


%%--------------------------------------------------------------------
%% Some simple tests.
%%--------------------------------------------------------------------
get_store_test() ->
    [
        ?assert( get_store(#state{ store=[["1", "2"]], seq=["1", "2", "3"]}) =:= [["1", "2", "3"], ["1", "2"]])
    ]
.

update_state_test() ->
    [
        ?assert(update_state("3", #state{ store=[["2", "1"]], seq=["3", "2", "1"]}) =:= { ok, #state{
            store=[["3", "2", "1"], ["2", "1"]],
            seq = ["3"]
        } })
    ]
.
