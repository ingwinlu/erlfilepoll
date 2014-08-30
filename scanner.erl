-module(scanner).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-record(state,{directory, regex, timeout=1000, files=[], poll=0}).

-export([start_link/3,pull/1,stop/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%%%Client API
%%Starts Server
start_link(Directory, Regex, Timeout) ->
    gen_server:start_link(?MODULE, [Directory, Regex, Timeout], []).

%%Pull List of Files that changed
pull(Pid) ->
    gen_server:call(Pid, pull).
%%Stop Server
stop(Pid) ->
    gen_server:call(Pid, terminate).

%%%Server
init([Directory, Regex, Timeout]) ->
    timer:send_after(0,scan),
    {ok, #state{directory=Directory, regex=Regex, timeout=Timeout, files=[], poll=0}}.

handle_call(
        pull,
        _From, 
        State) ->
    {reply,State#state.files,State#state{files=[]}};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(
        scan,
        State
        ) ->
    {New_files,New_Poll} = get_new_files(State#state.directory,State#state.regex, State#state.poll),
    MergedFiles = lists:merge(State#state.files,New_files),
    timer:send_after(State#state.timeout,self(),scan),
    {noreply,State#state{files=MergedFiles,poll=New_Poll}};
handle_info(Msg,State) ->
    error_logger:error_msg("Unknown Message: ~p~n",[Msg]),
    {noreply,State}.

code_change(_OldVsn, Clients, _Extra) ->
    {ok, Clients}.

terminate(normal, State) ->
    error_logger:info_msg("Last state: ~w~n",[State]),
    ok.

%%%Private
get_new_files(Directory, Regex, LastPollTime) ->
    NewPollTime = calendar:datetime_to_gregorian_seconds({ date(), time() }),
    NewFiles = filelib:fold_files(
        Directory, 
        Regex, 
        true, 
        fun(File, Acc) -> 
            {ok, FileInfo} = file:read_file_info(File),
            FileMtime = calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime),
            if 
                FileMtime>LastPollTime ->
                    [File|Acc];
                true ->
                    Acc
            end
        end,
        []
    ),
    { NewFiles, NewPollTime }.

