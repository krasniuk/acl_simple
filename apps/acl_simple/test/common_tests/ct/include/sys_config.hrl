-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-define(START_ENV,
    [
        {acl_simple, [
            {listen_port, 1913},
            {timer_cache, 2000},
            {timer_allow_roles, 4000}
        ]},
        {poolboy, [
            {pools, [
                {pg_pool, [
                    {name, {local, pg_pool}},
                    {strategy, fifo},
                    {worker_module, acl_simple_pg},
                    {size, 10},
                    {max_overflow, 5}
                ], [
                    {host, "127.0.0.1"},
                    {port, 5432},
                    {username, "trembita"},
                    {password, "trembita"},
                    {database, "civil"}]}
            ]}
        ]},
        {lager, [
            {handlers, [
                {lager_console_backend, [{level, debug}]},
                {lager_file_backend, [
                    {file, "error.log"},
                    {level, error},
                    {size, 1073741824}, %% 1Gb
                    {date, "$D0"},
                    {count, 20}
                ]},
                {lager_file_backend, [
                    {file, "warning.log"},
                    {level, warning},
                    {size, 1073741824}, %% 1Gb
                    {date, "$D0"},
                    {count, 20}
                ]},
                {lager_file_backend, [
                    {file, "info.log"},
                    {level, info},
                    {size, 1073741824}, %% 1Gb
                    {date, "$D0"},
                    {count, 7}
                ]},
                {lager_file_backend, [
                    {file, "debug.log"},
                    {level, debug},
                    {size, 1073741824}, %% 1Gb
                    {date, "$D0"},
                    {count, 2}
                ]}
            ]},
            {crash_log, "crash.log"},
            {crash_log_msg_size, 65536},
            {crash_log_size, 10485760},
            {crash_log_date, "$D0"},
            {crash_log_count, 5},
            {error_logger_redirect, true},
            {error_logger_hwm, 400}
        ]}
    ]).