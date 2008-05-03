{application, fuzed,
  [{description, "Fuzed master node"},
   {vsn, "0.0.0"},
   {modules, [fuzed_app, fuzed_supervisor, resource_fountain, resource_pool, pool_sweeper, fuzed_code_monitor, response_error_logger, logger]},
   {registered, [resource_fountain, resource_pool, fuzed_supervisor, pool_sweeper, fuzed_code_monitor]},
   {applications, [kernel, stdlib]},
   {mod, {fuzed_app, []}},
   {start_phases, []}
  ]}.
