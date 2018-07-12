%%==============================================================================
%% The Erlang Language Server
%%==============================================================================
-module(erlang_ls_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(ranch_protocol).
-behaviour(gen_statem).

%%==============================================================================
%% Exports
%%==============================================================================
%% ranch_protocol callbacks
-export([ start_link/4 ]).

%% gen_statem callbacks
-export([ callback_mode/0
        , code_change/4
        , init/1
        , terminate/3
        ]).

%% gen_statem state functions
-export([ connected/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { socket
               , transport
               , length
               , body
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================
-spec start_link(ramnch:ref(), any(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%==============================================================================
%% gen_statem callbacks
%%==============================================================================
-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

-spec init({ranch:ref(), any(), module(), any()}) -> no_return().
init({Ref, Socket, Transport, _Opts}) ->
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, true}, {packet, 0}]),
  gen_statem:enter_loop( ?MODULE
                       , []
                       , connected
                       , #state{ socket    = Socket
                               , transport = Transport
                               }
                       ).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec terminate(any(), atom(), state()) -> any().
terminate(_Reason, _StateName, #state{ socket    = Socket
                                     , transport = Transport
                                     }) ->
  Transport:close(Socket),
  ok.

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================
-spec connected(gen_statem:event_type(), any(), state()) -> any().
connected(info, {tcp, Socket, TcpData}, #state{ socket = Socket } = State) ->
  case State#state.length of
    undefined ->
      {Headers, Body} = cow_http:parse_headers(TcpData),
      BinLength       = proplists:get_value( <<"content-length">>, Headers),
      Length          = binary_to_integer(BinLength),
      handle_body_part(State#state{ length = Length, body = Body });
    _ ->
      OldBody = State#state.body,
      handle_body_part(State#state{ body = <<OldBody/binary, TcpData/binary>> })
  end;
connected(info, {tcp_closed, _Socket}, _State) ->
  {stop, normal};
connected(info, {'EXIT', _, normal}, _State) ->
  keep_state_and_data;
connected(info, {tcp_error, _, Reason}, _State) ->
  {stop, Reason}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_body_part(state()) -> any().
handle_body_part(#state{body = Body, length = Length} = State) ->
  case byte_size(Body) < Length of
    true  -> {keep_state, State};
    false -> handle_request(State#state{ length = undefined })
  end.

-spec handle_request(state()) -> no_return().
handle_request(#state{ socket    = Socket
                     , body      = Body
                     } = State) ->
  Request   = parse_data(Body),
  Method    = maps:get(<<"method">>, Request),
  Params    = maps:get(<<"params">>, Request),
  lager:debug("[Handling request] [method=~s] [params=~p]", [Method, Params]),
  case handle_request(Method, Params, State) of
    {Result, NewState} ->
      RequestId = maps:get(<<"id">>, Request),
      ok = erlang_ls_protocol:response(Socket, RequestId, Result),
      {keep_state, NewState};
    {NewState}         ->
      {keep_state, NewState}
  end.

-spec handle_request(binary(), map(), state()) ->
  {any(), state()} | {state()}.
handle_request(<<"initialize">>, _Params, State) ->
  Result = #{ capabilities =>
                #{ hoverProvider => false
                 , completionProvider =>
                     #{ resolveProvider => false
                      , triggerCharacters => [<<":">>, <<"#">>]
                      }
                 , textDocumentSync => 1
                 , definitionProvider => true
                 }
            },
  {Result, State};
handle_request(<<"initialized">>, _, State) ->
  {State};
handle_request(<<"textDocument/didOpen">>, Params, State) ->
  ok = erlang_ls_text_synchronization:did_open(State#state.socket, Params),
  {State};
handle_request(<<"textDocument/didChange">>, Params, State) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    []                      -> ok;
    [#{<<"text">> := Text}] ->
      {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
      ok = erlang_ls_buffer:set_text(Buffer, Text)
  end,
  {State};
handle_request(<<"textDocument/hover">>, _Params, State) ->
  {null, State};
handle_request(<<"textDocument/completion">>, Params, State) ->
  Position     = maps:get(<<"position">> , Params),
  Line         = maps:get(<<"line">>     , Position),
  Character    = maps:get(<<"character">>, Position),
  TextDocument = maps:get(<<"textDocument">>  , Params),
  Uri          = maps:get(<<"uri">>      , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  Result       = erlang_ls_buffer:get_completions(Buffer, Line, Character),
  {Result, State};
handle_request(<<"textDocument/didSave">>, Params, State) ->
  ok = erlang_ls_text_synchronization:did_save(State#state.socket, Params),
  {State};
handle_request(<<"textDocument/definition">>, Params, State) ->
  Position     = maps:get(<<"position">>    , Params),
  Line         = maps:get(<<"line">>        , Position),
  Character    = maps:get(<<"character">>   , Position),
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  {M, F, A}    = erlang_ls_buffer:get_mfa(Buffer, Line, Character),
  Which = code:which(M),
  Source = list_to_binary(proplists:get_value( source
                                             , M:module_info(compile))),
  DefUri = <<"file://", Source/binary>>,
  {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Which, [abstract_code]),
  Result = case [ AL || {function, AL, AF, AA, _} <- AC, F =:= AF, A =:= AA] of
             [DefLine] ->
               #{ uri => DefUri
                , range => erlang_ls_protocol:range(erl_anno:line(DefLine) - 1)
                };
             [] ->
               null
           end,
  {Result, State};
handle_request(RequestMethod, _Params, #state{socket = Socket} = State) ->
  Message = <<"Method not implemented: ", RequestMethod/binary>>,
  Method  = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  erlang_ls_protocol:notification(Socket, Method, Params),
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  {State}.

-spec parse_data(binary()) -> map().
parse_data(Body) ->
  jsx:decode(Body, [return_maps]).