erlfilepoll
===========

erlang file poller

## Usage ##
Add it to your `rebar.config` deps:

    {'filepoll',    ".*",    {git, "git@github.com:ingwinlu/erlfilepoll.git"}}

Start the filepoller and specify a directory and a regex to match against:

    > {ok,Pid} = filepoll:start_link(".",".*").
    {ok,<0.35.0>}

Pull information:
    > filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",created,{{2014,8,31},{15,8,53}}},
     {"./filepoll.erl",created,{{2014,8,31},{12,39,23}}},
     {"./filepoll_wpool.erl",created,{{2014,8,31},{15,9,2}}},
     {"./filepoll.beam",created,{{2014,8,31},{15,9,12}}},
     {"./filepoll_wpool.beam",created,{{2014,8,31},{15,9,12}}},
     {"./filepoll_watcher.beam",created,{{2014,8,31},{15,9,12}}}]

Return Tupels can either be _created_, _unchanged_, _modified_, _deleted_.

##TODO
Implement periodic updates