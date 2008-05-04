{application, fuzed_frontend,
  [{description, "FUZED Frontend Node"},
   {vsn, "0.5.0"},
   {modules, [fuzed_frontend, fuzed_frontend_supervisor, frontend_responder,
              frontend_yaws]},
   {registered, [master_beater]},
   {applications, [kernel, stdlib]},
   {mod, {fuzed_frontend_app, []}},
   {start_phases, []}
  ]}.
