-module(filepoll).
-behaviour(gen_server).
-record(
    state,
        {
        wpool,
        directory, 
        regex,
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
    {ok, Wpool} = filepoll_wpool:start_link(),
    {ok, #state{wpool=Wpool,directory=Directory, regex=Regex, lastpoll=0}}.

handle_call(pull, _From, State) ->
    %check for new files
    NewPollTime = handle_new_files(State),
    %pull updates
    %error_logger:info_msg("pull updates: ~n",[]),
    Updates = filepoll_wpool:pull(State#state.wpool),
    %send action list
    {reply,Updates,State#state{lastpoll=NewPollTime}};
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
handle_new_files(State) ->
    NewPollTime = {date(), time()},
    filelib:fold_files(
        State#state.directory, 
        State#state.regex, 
        true, 
        fun(File, _) -> 
            filepoll_wpool:start_watcher(State#state.wpool,File)
        end,
        []
    ),
    NewPollTime.
