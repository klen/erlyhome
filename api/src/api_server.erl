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
    update_store/1,
    start_link/1,
    put_number/2,
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

-record( state, { cur_seq=[], stored_seq=[], limit=10 } ).

-include_lib("eunit/include/eunit.hrl").


%% @spec start_link( Limit ) -> {ok, Pid}
%% @doc Starts the server.
start_link(Limit) when is_integer(Limit) ->
    error_logger:info_msg("Starting api with limit: ~p", [ Limit ]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Limit, [])
;
start_link(SLimit) when is_list(SLimit) ->
    Limit = list_to_integer(SLimit),
    start_link(Limit)
.


%% @spec get() -> Result
%% @doc Interface for get saved sequences.
get() ->
    {ok, Result} = gen_server:call( ?MODULE, get ),
    Result
.


%% @spec put(Number::list()) -> Response
%% @doc  Put number in sequences.
put(Number) when is_list(Number) ->
    % why don't we use numbers???
    gen_server:cast(?MODULE, {put, Number})
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
    Store = update_store(State),
    { reply, { ok, Store }, State }
.


%% @spec handle_cast( { put, Number }, State::record()) -> { noreply, State::record() }
%% @doc Increase sequence and save results in server state.
handle_cast( { put, Number }, State ) ->
    { ok, NewState } = put_number(Number, State),
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


put_number(Number, State = #state{cur_seq = []}) ->
    { ok, State#state{ cur_seq = [ Number ] } }
;
%% @spec put_number(Number, State) -> { ok, State } 
%% @doc  Get number and return new or old state.
put_number(Number, State) ->
    [ Max | _Rest ] = State#state.cur_seq,
    [ IMax, INumber ] = [ list_to_integer(Max), list_to_integer(Number) ],
    if
        INumber > IMax ->

            % Continue sequence
            { ok, State#state{ cur_seq = [ Number | State#state.cur_seq ] } };

        true ->

            % Create new sequence and update store
            { ok, State#state{ cur_seq = [ Number ], stored_seq = update_store(State) } }

    end
.

%% @spec update_store(State) -> Store 
%% @doc  Update store from current sequence and return sorted.
update_store(#state{ stored_seq=Store, limit=Limit, cur_seq=Seq }) ->
    SortStore = lists:sort(fun (X, Y) -> length(X) > length(Y) end, [ Seq | Store ]),
    lists:sublist(SortStore, Limit)
.


%%--------------------------------------------------------------------
%% Some simple tests.
%%--------------------------------------------------------------------
update_store_test() ->
    [
        ?assert( update_store(#state{ stored_seq=[["1", "2"]], cur_seq=["1", "2", "3"]}) =:= [["1", "2", "3"], ["1", "2"]])
    ]
.

put_number_test() ->
    [
        ?assert(put_number("3", #state{ stored_seq=[["2", "1"]], cur_seq=["3", "2", "1"]}) =:= { ok, #state{
            stored_seq=[["3", "2", "1"], ["2", "1"]],
            cur_seq = ["3"]
        } })
    ]
.
