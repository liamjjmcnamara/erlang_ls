%%==============================================================================
%% PropEr tests
%%==============================================================================
-module(prop_statem).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(proper_statem).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper_contrib/include/proper_contrib_statem.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-compile(export_all).
-compile(nowarn_export_all).

%%==============================================================================
%% Defines
%%==============================================================================
-define(HOSTNAME, {127,0,0,1}).
-define(PORT    , 10000).

%%==============================================================================
%% Initial State
%%==============================================================================
initial_state() ->
  #{ connected => false
   , shutdown  => false
   , documents => []
   }.

%%==============================================================================
%% Weights
%%==============================================================================
weight(_S, shutdown) -> 1;
weight(_S, exit)     -> 1;
weight(_S, _Cmd)     -> 5.

%%==============================================================================
%% Commands
%%==============================================================================

%%------------------------------------------------------------------------------
%% Connect
%%------------------------------------------------------------------------------
connect() ->
  els_client:start_link(tcp, {?HOSTNAME, ?PORT}).

connect_args(_S) ->
  [].

connect_pre(#{connected := Connected} = _S) ->
  not Connected.

connect_next(S, _R, _Args) ->
  S#{connected => true}.

connect_post(_S, _Args, Res) ->
  ?assertMatch({ok, _Pid}, Res),
  true.

%%------------------------------------------------------------------------------
%% Initialize
%%------------------------------------------------------------------------------
initialize(RootUri, InitOptions) ->
  els_client:initialize(RootUri, InitOptions).

initialize_args(_S) ->
  [ els_proper_gen:root_uri()
  , els_proper_gen:init_options()
  ].

initialize_pre(#{connected := Connected} = _S) ->
  Connected.

initialize_next(S, _R, _Args) ->
  S.

initialize_post(#{shutdown := true}, _Args, Res) ->
  assert_shutdown_error(Res),
  true;
initialize_post(_S, _Args, Res) ->
  Expected = #{ capabilities =>
                  #{ hoverProvider => true
                   , completionProvider =>
                       #{ resolveProvider => false
                        , triggerCharacters => [<<":">>, <<"#">>, <<"?">>]
                        }
                   , textDocumentSync =>
                       #{ openClose => true
                        , change    => ?TEXT_DOCUMENT_SYNC_KIND_FULL
                        , save      => #{includeText => true}
                        }
                   , definitionProvider      => true
                   , referencesProvider      => true
                   , documentSymbolProvider  => true
                   , workspaceSymbolProvider => true
                   , didChangeWatchedFiles =>
                       #{ dynamicRegistration => false
                        }
                   }
              },
  ?assertEqual(Expected, maps:get(result, Res)),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didOpen
%%------------------------------------------------------------------------------
did_open(Uri, LanguageId, Version, Text) ->
  els_client:did_open(Uri, LanguageId, Version, Text).

did_open_args(_S) ->
  [els_proper_gen:uri(), <<"erlang">>, 0, els_proper_gen:tokens()].

did_open_pre(#{connected := Connected} = _S) ->
  Connected.

did_open_next(S, _R, [Uri, _, _, _]) ->
  maps:put(documents, maps:get(documents, S) ++ [Uri], S).

did_open_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didSave
%%------------------------------------------------------------------------------
did_save(Uri) ->
  els_client:did_save(Uri).

did_save_args(_S) ->
  [els_proper_gen:uri()].

did_save_pre(#{connected := Connected} = _S) ->
  Connected.

did_save_next(S, _R, _Args) ->
  S.

did_save_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didClose
%%------------------------------------------------------------------------------
did_close(Uri) ->
  els_client:did_close(Uri).

did_close_args(_S) ->
  [els_proper_gen:uri()].

did_close_pre(#{connected := Connected} = _S) ->
  Connected.

did_close_next(S, _R, _Args) ->
  S.

did_close_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% Shutdown
%%------------------------------------------------------------------------------
shutdown() ->
  els_client:shutdown().

shutdown_args(_S) ->
  [].

shutdown_pre(#{connected := Connected} = _S) ->
  Connected.

shutdown_next(S, _R, _Args) ->
  S#{shutdown => true}.

shutdown_post(#{shutdown := true}, _Args, Res) ->
  assert_shutdown_error(Res),
  true;
shutdown_post(_S, _Args, Res) ->
  ?assertMatch(#{result := null}, Res),
  true.

%%------------------------------------------------------------------------------
%% Shutdown
%%------------------------------------------------------------------------------
exit() ->
  els_client:exit().

exit_args(_S) ->
  [].

exit_pre(#{connected := Connected} = _S) ->
  Connected.

exit_next(S, _R, _Args) ->
  %% We disconnect to simulate the server goes down
  catch disconnect(),
  S#{shutdown => false, connected => false}.

exit_post(S, _Args, Res) ->
  ExpectedExitCode = case maps:get(shutdown, S, false) of
                       true  -> 0;
                       false -> 1
                     end,
  els_test_utils:wait_for(halt_called, 1000),
  ?assert(meck:called(els_utils, halt, [ExpectedExitCode])),
  ?assertMatch(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% Disconnect
%%------------------------------------------------------------------------------
disconnect() ->
  els_client:stop().

disconnect_args(_S) ->
  [].

disconnect_pre(#{connected := Connected} = _S) ->
  Connected.

disconnect_next(S, _R, _Args) ->
  S#{connected => false}.

disconnect_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%==============================================================================
%% The statem's property
%%==============================================================================
prop_main() ->
  Config = #{ setup_fun    => fun setup/0
            , teardown_fun => fun teardown/1
            , cleanup_fun  => fun cleanup/0
            },
  proper_contrib_statem:run(?MODULE, Config).

%%==============================================================================
%% Setup
%%==============================================================================
setup() ->
  meck:new(els_compiler_diagnostics, [no_link, passthrough]),
  meck:new(els_dialyzer_diagnostics, [no_link, passthrough]),
  meck:new(els_utils, [no_link, passthrough]),
  meck:expect(els_compiler_diagnostics, diagnostics, 1, []),
  meck:expect(els_dialyzer_diagnostics, diagnostics, 1, []),
  Self    = erlang:self(),
  HaltFun = fun(_X) -> Self ! halt_called, {noresponse, #{}} end,
  meck:expect(els_utils, halt, HaltFun),
  application:ensure_all_started(erlang_ls),
  application:set_env(erlang_ls, index_otp, false),
  application:set_env(erlang_ls, index_deps, false),
  file:write_file("/tmp/erlang_ls.config", <<"">>),
  lager:set_loglevel(lager_console_backend, warning),
  ok.

%%==============================================================================
%% Teardown
%%==============================================================================
teardown(_) ->
  meck:unload(els_compiler_diagnostics),
  meck:unload(els_dialyzer_diagnostics),
  meck:unload(els_utils),
  ok.

%%==============================================================================
%% Cleanup
%%==============================================================================
cleanup() ->
  catch disconnect(),
  %% Restart the server, since though the client disconnects the
  %% server keeps its state.
  els_server:reset_state(),
  ok.

%%==============================================================================
%% Helper functions
%%==============================================================================

assert_shutdown_error(Res) ->
  ?assertMatch(#{error := #{code := _, message := _}}, Res).

meck_matcher_integer(N) ->
  meck_matcher:new(fun(X) -> X =:= N end).
