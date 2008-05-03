-module(id2_tools).
-compile(export_all).

testing_bootstrap() -> 
  spawn( fun() -> 
    error_logger:info_msg("Boostrapping a local server for testing purposes."),
    start_remote_server(),
    start_nodes(),
    error_logger:info_msg("System started.")
  end).

start_remote_server() ->
  start_fountain(),
  json_server:test().
  
start_fountain() -> resource_fountain:start().

fountain_pool_for_node(Node) -> resource_fountain:pool_for_details(node_api:details(Node)).
add_to_fountain(Node) -> resource_pool:add(fountain_pool_for_node(Node), Node), Node.

start_nodes() -> 
  Maker = fun() -> 
    WP = port_wrapper:wrap("xlew ruby -I./rlibs ./rlibs/xle_node.rb"), 
    add_to_fountain(WP)
  end,
  Killer = fun(X) -> 
    resource_pool:remove(X, fountain_pool_for_node(X)) 
  end,
  resource_manager:start_link(Maker, Killer, 2).