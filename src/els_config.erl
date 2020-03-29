-module(els_config).

%% API
-export([ do_initialize/4
        , initialize/3
        , get/1
        , set/2
        , start_link/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_FILE, "erlang_ls.config").
-define(REBAR_CONFIG_FILE, "rebar.config").
-define( DEFAULT_EXCLUDED_OTP_APPS
       , [ "diameter"
         , "megaco"
         , "snmp"
         , "wx"
         ]
       ).
-define(SERVER, ?MODULE).

%% TODO: Refine names to avoid confusion
-type key()   :: apps_dirs
               | apps_paths
               | capabilities
               | code_lenses
               | code_reload
               | deps_dirs
               | deps_paths
               | diagnostics
               | include_dirs
               | include_paths
               | otp_apps_exclude
               | otp_path
               | otp_paths
               | plt_path
               | project_type
               | rebar_config
               | root_uri
               | search_paths.

-type path()  :: file:filename().
-type state() :: #{ apps_dirs        => [path()]
                  , apps_paths       => [path()]
                  , code_lenses      => [els_code_lens:lens_id()]
                  , code_reload      => map() | 'disabled'
                  , deps_dirs        => [path()]
                  , deps_paths       => [path()]
                  , diagnostics      => [els_diagnostics:diagnostic_id()]
                  , include_dirs     => [path()]
                  , include_paths    => [path()]
                  , otp_apps_exclude => [string()]
                  , otp_path         => path()
                  , otp_paths        => [path()]
                  , plt_path         => path()
                  , project_type     => undefined | rebar
                  , rebar_config     => map() | 'disabled'
                  , root_uri         => uri()
                  , search_paths     => [path()]
                  }.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(uri(), map(), map()) -> ok.
initialize(RootUri, Capabilities, InitOptions) ->
  RootPath = els_utils:to_list(els_uri:path(RootUri)),
  ElsConfig = consult_config(config_paths(RootPath, InitOptions)),
  RebarConfig = maybe_rebar_config(RootPath),
  do_initialize(RootUri, Capabilities, ElsConfig, RebarConfig).

-spec do_initialize(uri(), map(), {undefined|path(), map()}, map()) -> ok.
do_initialize(RootUri, Capabilities, {ConfigPath, Config}, RebarConfig) ->
  RootPath = els_utils:to_list(els_uri:path(RootUri)),

  % Allow override of rebar.confg
  DepsDirs = resolve_config(deps_dirs, Config, RebarConfig),

  OtpPath         = maps:get("otp_path", Config, code:root_dir()),
  AppsDirs        = maps:get("apps_dirs", Config, ["."]),
  IncludeDirs     = maps:get("include_dirs", Config, ["include"]),
  Macros          = maps:get("macros", Config, []),
  DialyzerPltPath = maps:get("plt_path", Config, undefined),
  OtpAppsExclude  = maps:get( "otp_apps_exclude"
                            , Config
                            , ?DEFAULT_EXCLUDED_OTP_APPS
                            ),
  CodeLenses = maps:get("code_lenses", Config, els_code_lens:default_lenses()),
  Diagnostics =
    maps:get("diagnostics", Config, els_diagnostics:default_diagnostics()),
  ExcludePathsSpecs = [[OtpPath, "lib", P ++ "*"] || P <- OtpAppsExclude],
  ExcludePaths = els_utils:resolve_paths(ExcludePathsSpecs, RootPath, true),
  lager:info("Excluded OTP Applications: ~p", [OtpAppsExclude]),
  CodeReload = maps:get("code_reload", Config, disabled),

  %% Passed by the LSP client
  ok = set(root_uri     , RootUri),
  ok = set(capabilities , Capabilities),
  %% Read from the erlang_ls.config file
  ok = set(config_path  , ConfigPath),
  ok = set(otp_path     , OtpPath),
  ok = set(deps_dirs    , DepsDirs),
  ok = set(apps_dirs    , AppsDirs),
  ok = set(include_dirs , IncludeDirs),
  ok = set(macros       , Macros),
  ok = set(plt_path     , DialyzerPltPath),
  ok = set(code_reload  , CodeReload),
  %% Calculated from the above
  ok = set(apps_paths   , project_paths(RootPath, AppsDirs, false)),
  ok = set(deps_paths   , project_paths(RootPath, DepsDirs, false)),
  ok = set(include_paths, include_paths(RootPath, IncludeDirs, false)),
  ok = set(otp_paths    , otp_paths(OtpPath, false) -- ExcludePaths),
  ok = set(code_lenses  , CodeLenses),
  ok = set(diagnostics  , Diagnostics),
  %% All (including subdirs) paths used to search files with file:path_open/3
  ok = set( search_paths
          , lists:append([ project_paths(RootPath, AppsDirs, true)
                         , project_paths(RootPath, DepsDirs, true)
                         , include_paths(RootPath, IncludeDirs, false)
                         , otp_paths(OtpPath, true)
                         ])
          ),
  ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec get(key()) -> any().
get(Key) ->
  gen_server:call(?SERVER, {get, Key}).

-spec set(key(), any()) -> ok.
set(Key, Value) ->
  gen_server:call(?SERVER, {set, Key, Value}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
  {reply, any(), state()}.
handle_call({get, Key}, _From, State) ->
  Value = maps:get(Key, State, undefined),
  {reply, Value, State};
handle_call({set, Key, Value}, _From, State0) ->
  State = maps:put(Key, Value, State0),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec config_paths(path(), map()) -> [path()].
config_paths( RootPath
            , #{<<"erlang">> := #{<<"config_path">> := ConfigPath0}}) ->
  ConfigPath = els_utils:to_list(ConfigPath0),
  lists:append([ possible_config_paths(ConfigPath)
               , possible_config_paths(filename:join([RootPath, ConfigPath]))
               , default_config_paths(RootPath)]);
config_paths(RootPath, _Config) ->
  default_config_paths(RootPath).

-spec default_config_paths(path()) -> [path()].
default_config_paths(RootPath) ->
  GlobalConfigDir = filename:basedir(user_config, "erlang_ls"),
  [ filename:join([RootPath, ?DEFAULT_CONFIG_FILE])
  , filename:join([GlobalConfigDir, ?DEFAULT_CONFIG_FILE])
  ].

%% @doc Bare `Path' as well as with default config file name suffix.
-spec possible_config_paths(path()) -> [path()].
possible_config_paths(Path) ->
  [ Path, filename:join([Path, ?DEFAULT_CONFIG_FILE]) ].

-spec consult_config([path()]) -> {undefined|path(), map()}.
consult_config([]) -> {undefined, #{}};
consult_config([Path | Paths]) ->
  lager:info("Reading erlang_ls config file. path=~p", [Path]),
  Options = [{map_node_format, map}],
  try yamerl:decode_file(Path, Options) of
      [] -> {Path, #{}};
      [Config] -> {Path, Config}
  catch
    Class:Error ->
      lager:warning( "Could not read config file: path=~p class=~p error=~p"
                   , [Path, Class, Error]),
      consult_config(Paths)
  end.

-spec include_paths(path(), string(), boolean()) -> [string()].
include_paths(RootPath, IncludeDirs, Recursive) ->
  Paths = [ els_utils:resolve_paths([[RootPath, Dir]], RootPath, Recursive)
            || Dir <- IncludeDirs
          ],
  lists:append(Paths).

-spec project_paths(path(), [string()], boolean()) -> [string()].
project_paths(RootPath, Dirs, Recursive) ->
  Paths = [ els_utils:resolve_paths( [ [RootPath, Dir, "src"]
                                     , [RootPath, Dir, "test"]
                                     , [RootPath, Dir, "include"]
                                     ]
                                   , RootPath
                                   , Recursive
                                   )
            || Dir <- Dirs
          ],
  lists:append(Paths).

-spec otp_paths(path(), boolean()) -> [string()].
otp_paths(OtpPath, Recursive) ->
  els_utils:resolve_paths( [ [OtpPath, "lib", "*", "src"]
                           , [OtpPath, "lib", "*", "include"]
                           ]
                         , OtpPath
                         , Recursive
                         ).

-spec maybe_rebar_config(path()) -> map().
maybe_rebar_config(RootPath) ->
  RebarConfigPath = filename:join([RootPath, ?REBAR_CONFIG_FILE]),
  lager:info("Trying to read rebar.config file. path=~p", [RebarConfigPath]),
  case filelib:is_regular(RebarConfigPath) of
    true  -> parse_rebar_config(RebarConfigPath);
    false -> #{}
  end.

-spec parse_rebar_config(path()) -> map().
parse_rebar_config(RebarConfigPath) ->
  set(project_type, rebar),
  case file:consult(RebarConfigPath) of
    {ok, RebarConfig} ->
      extract_rebar_config(RebarConfig);
    {error, Error} ->
      lager:warning( "Could not consult file ~p: ~p", [RebarConfigPath, Error]),
      #{}
  end.

-spec extract_rebar_config(list()) -> map().
extract_rebar_config(RebarConfig) ->
  lists:foldl(fun process_rebar_option/2, #{}, RebarConfig).

-spec process_rebar_option(term(), map()) -> map().
process_rebar_option({Key, Value}, Acc) ->
  maps:update(Key, Value, Acc).

-spec resolve_config(atom(), map(), map()) -> [path()].
resolve_config(deps_dirs, Config, RebarConfig) ->
  RebarDeps = case maps:get(deps_dir, RebarConfig, undefined) of
    undefined -> [];
    DepsDir -> [DepsDir]
  end,
  maps:get("deps_dirs", Config, RebarDeps).
