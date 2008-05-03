{application, id2,
  [{description, "ID2 Master Node (Katamari)"},
   {vsn, "0.4.0"},
   {modules, [id2_app, id2_supervisor, resource_fountain, resource_pool, pool_sweeper, id2_code_monitor, xle_thrift_responder,
              xleThriftService_thrift, xleThriftService_types, test_handler, test_service, thrift_binary_protocol, thrift_buffered_transport, thrift_client, thrift_framed_transport, thrift_processor, thrift_protocol, thrift_service, thrift_socket_server, thrift_socket_transport, thrift_transport, response_error_logger, logger]},
   {registered, [resource_fountain, resource_pool, id2_supervisor, pool_sweeper, id2_code_monitor]},
   {applications, [kernel, stdlib]},
   {mod, {id2_app, []}},
   {start_phases, []}
  ]}.
