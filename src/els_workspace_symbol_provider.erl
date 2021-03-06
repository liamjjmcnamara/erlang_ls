-module(els_workspace_symbol_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({symbol, Params}, State) ->
  %% TODO: Version 3.15 of the protocol introduces a much nicer way of
  %%       specifying queries, allowing clients to send the symbol kind.
  #{ <<"query">> := Query} = Params,
  {modules(Query), State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec modules(binary()) -> [symbol_information()].
modules(Query) ->
  All = els_db:list(modules),
  F = fun({Module, Uri}) ->
          filename:extension(Uri) =:= <<".erl">> andalso
            re:run(atom_to_binary(Module, utf8), Query) =/= nomatch
      end,
  lists:map(fun symbol_information/1, lists:filter(F, All)).

-spec symbol_information({atom(), uri()}) -> symbol_information().
symbol_information({Module, Uri}) ->
  Range = #{from => {1, 1}, to => {1, 1}},
  #{ name => atom_to_binary(Module, utf8)
   , kind => ?SYMBOLKIND_MODULE
   , location => #{ uri => Uri
                  , range => els_protocol:range(Range)
                  }
   }.
