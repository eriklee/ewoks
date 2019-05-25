-ifndef(JOB_SPEC_HRL).
-define(JOB_SPEC_HRL, 1).

-type schedule() ::
    one_off
  | persistent
  | {cron, 
       {hourly, integer()} % minute to schedule
     | {daily, integer()} % hour to schedule
    }.

-record(job_config, {
    job_id :: integer(),
    src_path :: string(),
    cmd :: string(),
    args :: [ string() | binary() ],
    env :: [{nonempty_string(), string()}],
    schedule :: schedule()
   }).

-endif.
