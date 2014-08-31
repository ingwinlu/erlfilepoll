-module(filepoll_watcher).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-record(state,{file, lastpoll=0, pulled=false}).

-export([start_link/1,stop/1,pull/1,file/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%%%Client API
%%Starts Server
start_link(File) ->
    gen_server:start_link(?MODULE, [File], []).

%%Stop Server
stop(Pid) ->
    gen_server:call(Pid, terminate).

%%update information on file
pull(Pid) ->
    gen_server:call(Pid, pull).
%%get File
file(Pid) ->
    gen_server:call(Pid, file).

%%%Server API
init([File]) ->
    NewPollTime = {date(), time()},
    {ok, #state{file=File, lastpoll=NewPollTime}}.

handle_call(pull, _From, State) ->
    NewPollTime = {date(), time()},
    NewState = State#state{lastpoll=NewPollTime, pulled=true},
    case file:read_file_info(State#state.file) of
        {ok, FileInfo} ->             
            FileMtime = FileInfo#file_info.mtime,
            if 
                FileMtime>State#state.lastpoll ->
                    {reply,{State#state.file, modified, FileMtime},NewState};
                State#state.pulled == false ->
                    {reply,{State#state.file, created, FileInfo#file_info.ctime},NewState};
                true ->
                    {reply,{State#state.file, unchanged, NewPollTime},NewState}
            end;
        _              -> 
            {reply,{State#state.file, deleted, NewPollTime},NewState}
    end;
handle_call(file, _From, State) ->
    {reply,State#state.file, State};
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

%%private
