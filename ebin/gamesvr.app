{
    application, gamesvr,
    [
        {description, "This is bg game server."},
        {vsn, "1.0a"},
        {modules,
        [
            bg_gamesvr_app,
            bg_gamesvr_sup
        ]},
        {registered, [bg_gamesvr_sup]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {bg_gamesvr_app, []}},
        {start_phases, []},
        {env, [
                {env, dev}
            ]}
    ]
}.
