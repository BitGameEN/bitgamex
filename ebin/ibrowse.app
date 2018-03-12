{application, ibrowse,
        [{description, "Erlang HTTP client application"},
         {vsn, "4.3"},
         {registered, [ibrowse_sup, ibrowse]},
         {applications, [kernel,stdlib]},
	 {env, [
            {default_max_sessions, 200},
            {default_max_pipeline_size, 10},
            {default_max_attempts, 3}
	       ]},
	 {mod, {ibrowse_app, []}},
         {maintainers, ["Chandrashekhar Mullaparthi"]},
         {licenses, ["GPLv2", "BSD"]},
	 {modules, []},
         {links, [{"Github", "https://github.com/cmullaparthi/ibrowse"}]}
        ]
}.
