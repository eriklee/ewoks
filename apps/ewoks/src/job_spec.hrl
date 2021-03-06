-ifndef(JOB_SPEC_HRL).
-define(JOB_SPEC_HRL, 1).

-type schedule() ::
    one_off
  | persistent
  | {cron, 
       {hourly, integer()} % minute to schedule
     | {daily, integer()} % hour to schedule
    }.

-type status() ::
    running
  | stopped
  | restarting.

-record(job_config, {
    job_id :: string(),
    src_path :: string(),
    cmd :: string(),
    args :: [ string() | binary() ],
    env :: [{nonempty_string(), string()}],
    schedule :: schedule(),
    status :: status()
   }).

-endif.
