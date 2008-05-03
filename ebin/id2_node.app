{application, id2_node,
  [{description, "ID2 Handler Node"},
   {vsn, "0.4.0"},
   {modules, [id2_node_app, id2_node_supervisor, resource_manager]},
   {registered, [resource_manager, id2_node_supervisor]},
   {applications, [kernel, stdlib]},
   {mod, {id2_node_app, []}},
   {start_phases, []}
  ]}.