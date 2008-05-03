{application, fuzed_node,
  [{description, "FUZED Handler Node"},
   {vsn, "0.4.0"},
   {modules, [fuzed_node_app, fuzed_node_supervisor, resource_manager]},
   {registered, [resource_manager, fuzed_node_supervisor]},
   {applications, [kernel, stdlib]},
   {mod, {fuzed_node_app, []}},
   {start_phases, []}
  ]}.