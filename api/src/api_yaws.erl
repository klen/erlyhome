%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc YAWS module
%%  @end                      
%% -------------------------------------------------------------------

-module(api_yaws).
-export([
    out/1
    ]).

-include("yaws_api.hrl").


%% @spec out(Arg) ->{ content, Mime, Body } | { status, Status } 
%% @doc  Base request parsing
out(Arg) ->
    Req = Arg#arg.req, 
    Method = Req#http_request.method,
    out(Arg, Method)
.

%% @spec out(Arg, Method) -> { content, Mime, Body } | { status, Status } 
%% @doc  API REST
out(_Arg, 'GET') ->
    Response = api:fetch(),
    { content, "application/json", json2:encode(Response) }
;
out(Arg, 'PUT') ->
    { ok, Value } = json2:decode_string(binary_to_list( Arg#arg.clidata )),
    case Value of
        undefined ->
            { status, 400 };
        _ ->
            api:save( Value ),
            { status, 200 }
    end
;
out(_Arg, _Method) ->
    { status, 405 }
.
