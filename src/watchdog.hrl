-ifndef(watchdog).
-define(watchdog, true).

-record(wd_children, {pidmap, ets}).
-record(wd_child, {id, starttime, startups, total_uptime}).

-endif.
