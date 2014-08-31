erlfilepoll
===========

Queries if files in watched directory have been created, modified, deleted.

## Usage ##
Add to your `rebar.config` deps:

    {'filepoll',    ".*",    {git, "git://github.com/ingwinlu/erlfilepoll.git"}}

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

filepoll handels the addition of files, filepoll_wpool keeps track of all watchers/workers, filepoll_watcher does the actual work. All of them implement gen_server.

Return Tupels can either be _created_, _unchanged_, _modified_, _deleted_.

##TODO
* Implement periodic updates
* Write Tests
* Write Edoc

##Full Example
    1> {ok,Pid} = filepoll:start_link(".",".*").
    {ok,<0.35.0>}
    2> filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",created,{{2014,8,31},{15,8,53}}},
     {"./filepoll.erl",created,{{2014,8,31},{12,39,23}}},
     {"./filepoll_wpool.erl",created,{{2014,8,31},{15,9,2}}},
     {"./filepoll.beam",created,{{2014,8,31},{15,9,12}}},
     {"./filepoll_wpool.beam",created,{{2014,8,31},{15,9,12}}},
     {"./filepoll_watcher.beam",created,{{2014,8,31},{15,9,12}}}]
    3> filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,9,42}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,9,42}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,9,42}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,9,42}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,9,42}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,9,42}}}]
    4> filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,9,44}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,9,44}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,9,44}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,9,44}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,9,44}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,9,44}}}]
    5> filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,9,52}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,9,52}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,9,52}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,9,52}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,9,52}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,9,52}}}]
    6> filepoll:pull(Pid).
    [{"./New Text Document.txt",created,
      {{2014,8,31},{15,10,35}}},
     {"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,10,36}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,10,36}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,10,36}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,10,36}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,10,36}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,10,36}}}]
    7> filepoll:pull(Pid).
    [{"./New Text Document.txt",unchanged,
      {{2014,8,31},{15,11,3}}},
     {"./filepoll_watcher.erl",unchanged,{{2014,8,31},{15,11,3}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,11,3}}},
     {"./filepoll_wpool.erl",modified,{{2014,8,31},{15,11,2}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,11,3}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,11,3}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,11,3}}}]
    8> filepoll:pull(Pid).
    [{"./New Text Document.txt",deleted,
      {{2014,8,31},{15,11,13}}},
     {"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,11,13}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,11,13}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,11,13}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,11,13}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,11,13}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,11,13}}}]
    9> filepoll:pull(Pid).
    [{"./filepoll_watcher.erl",unchanged,
      {{2014,8,31},{15,11,15}}},
     {"./filepoll.erl",unchanged,{{2014,8,31},{15,11,15}}},
     {"./filepoll_wpool.erl",unchanged,{{2014,8,31},{15,11,15}}},
     {"./filepoll.beam",unchanged,{{2014,8,31},{15,11,15}}},
     {"./filepoll_wpool.beam",unchanged,{{2014,8,31},{15,11,15}}},
     {"./filepoll_watcher.beam",unchanged,
      {{2014,8,31},{15,11,15}}}]
    10>

