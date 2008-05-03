{application, id2,
  [{description, "Fuzed master node"},
   {vsn, "0.0.0"},
   {modules, [id2_app, id2_supervisor, resource_fountain, resource_pool, pool_sweeper, id2_code_monitor, response_error_logger, logger]},
   {registered, [resource_fountain, resource_pool, id2_supervisor, pool_sweeper, id2_code_monitor]},
   {applications, [kernel, stdlib]},
   {mod, {id2_app, []}},
   {start_phases, []}
  ]}.
