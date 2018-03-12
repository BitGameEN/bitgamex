{
    application, gatesvr,
    [
        {description, "This is bg gate server."},
        {vsn, "1.0a"},
        {modules,
        [
            bg_gatesvr_app,
            bg_gatesvr_sup
        ]},
        {registered, [bg_gatesvr_sup]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {bg_gatesvr_app, []}},
        {start_phases, []},
        {env, [
                {env, dev}
            ]}
    ]
}.
