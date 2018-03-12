{
    application, xchgsvr,
    [
        {description, "This is bg xchg server."},
        {vsn, "1.0a"},
        {modules,
        [
            bg_xchgsvr_app,
            bg_xchgsvr_sup
        ]},
        {registered, [bg_xchgsvr_sup]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {bg_xchgsvr_app, []}},
        {start_phases, []},
        {env, [
                {env, dev},
                {xchg_http_port, 8886},
                {xchg_server_ips, []}
            ]}
    ]
}.
