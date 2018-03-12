{application, timer2,
 [
  {description, "TIMER2"},
  {vsn, "1"},
  {registered, [timer2_tab, timer2_ref_tab, timer2_pid_tab]},
  {id, "TIMER2"},
  {applications, [
                  kernel,
                  stdlib,
                  syntax_tools,
                  compiler,
                  lager,
                  gproc
                 ]},
  {mod, { timer2_app, []}},
  {env, [
          %% Define the number of acceptors and processors
          {timer2_acceptors, 20},
          {timer2_processors, 80}
         ]}
 ]}.
