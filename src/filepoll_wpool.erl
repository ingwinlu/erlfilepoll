-module(filepoll_wpool).
-behaviour(gen_server).
-record(state,{watchers=[], files=[]}).

-export([start_link/0,stop/1,pull/1,start_watcher/2]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%%%Client API
%%Starts Server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%Stop Server
stop(Pid) ->
    gen_server:call(Pid, terminate).
    
%%Pulls new Events
pull(Pid) ->
    gen_server:call(Pid, pull).

%%start watching file
start_watcher(Pid, File) ->
    gen_server:call(Pid, {start_watch, File}).

%%%Server API
init([]) ->
    {ok, #state{}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(pull, _From, State) ->
    Events = lists:map(
        fun(Watcher) ->
            case filepoll_watcher:pull(Watcher) of
                {File, deleted, Time} ->
                    timer:send_after(0,{stop_watch,Watcher}),
                    {File, deleted, Time};
                Default ->
                    Default
            end
        end,
        State#state.watchers
    ),
    {reply,Events,State};
handle_call({start_watch, File}, _From, State) ->
    case lists:member(File,State#state.files) of
        false -> %%not watched yet, start the machinery!
            {ok, NewPid} = filepoll_watcher:start_link(File),
            Watchers = [NewPid|State#state.watchers],
            Files = [File|State#state.files],
            {reply, ok, #state{watchers=Watchers, files=Files}};
        _ -> %%already watched, ignore
            {reply, already_running, State}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({stop_watch, WatcherPid}, State) ->
    File = filepoll_watcher:file(WatcherPid),
    Watchers = lists:delete(WatcherPid, State#state.watchers),
    Files = lists:delete(File, State#state.files),
    {noreply, #state{watchers=Watchers, files=Files}};
handle_info(Msg,State) ->
    error_logger:error_msg("Unknown Message: ~p~n",[Msg]),
    {noreply,State}.

code_change(_OldVsn, Clients, _Extra) ->
    {ok, Clients}.

terminate(normal, State) ->
    error_logger:info_msg("Last state: ~w~n",[State]),
    ok.
%%private
