-module(filepoll).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-record(
    state,
        {directory, 
        regex, 
        watchlist=[],
        lastpoll=0}
    ).

-export([start_link/2,pull/1,stop/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%%%Client API
%%Starts Server
start_link(Directory, Regex) ->
    gen_server:start_link(?MODULE, [Directory, Regex], []).

%%Pull List of Files that changed
pull(Pid) ->
    gen_server:call(Pid, pull).
%%Stop Server
stop(Pid) ->
    gen_server:call(Pid, terminate).

%%%Server
init([Directory, Regex]) ->
    {ok, #state{directory=Directory, regex=Regex,watchlist=[], lastpoll=0}}.
handle_call(
        pull,
        _From, 
        State) ->
    %check for new files
    { NewPollTime, NewActions, NewWatchers  } = get_new_watchers(State#state.directory,State#state.regex,State#state.lastpoll),
    %check old files
    Updates = get_updates(State#state.watchlist),
    %erlang:display(Updates),
    %send action list
    {reply,NewActions++Updates,State#state{lastpoll=NewPollTime, watchlist=State#state.watchlist++NewWatchers}};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg,State) ->
    error_logger:error_msg("Unknown Message: ~p~n",[Msg]),
    {noreply,State}.

code_change(_OldVsn, Clients, _Extra) ->
    {ok, Clients}.

terminate(normal, State) ->
    error_logger:info_msg("Last state: ~w~n",[State]),
    ok.

%%%Private
get_new_watchers(Directory, Regex, LastPollTime) ->
    NewPollTime = {date(), time()},
    {NewActions,NewWatchers} = filelib:fold_files(
        Directory, 
        Regex, 
        true, 
        fun(File, {Actions, Watchers}) -> 
            {ok, FileInfo} = file:read_file_info(File),
            FileCtime = FileInfo#file_info.ctime,
            if 
                FileCtime>LastPollTime ->
                    NewAction = {File, created, FileCtime},
                    {ok, NewWatcher} = filepoll_watcher:start_link(File),
                    {[NewAction|Actions],[NewWatcher|Watchers]};
                true ->
                    {Actions,Watchers}
            end
        end,
        {[],[]}
    ),
    %erlang:display(NewActions),
    %erlang:display(NewWatchers),
    { NewPollTime, NewActions, NewWatchers}.

get_updates(WatchList) ->
    %erlang:display(WatchList),
    lists:foldl(
        fun(Watcher, Actionlist) ->
            %erlang:display(Watcher),
            [filepoll_watcher:update(Watcher)|Actionlist] end,
        [],
        WatchList
    ).