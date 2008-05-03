
require 'rubygems'
require 'thrift'
require 'thrift/transport/tsocket'
require 'thrift/protocol/tbinaryprotocol'
$:.push('/p/share/xle_rpc/gen-rb')
require 'XleThriftService'

host = 'id2-dev.powerset.com'
port = 9100

query = "sh8k r00lz"
details = {"grammar"=>"query", "kind"=>"xle", "tags"=>"feng"}
parameters = {"restrict_to_url"=>""}

transport = TBufferedTransport.new(TSocket.new(host, port))
protocol = TBinaryProtocol.new(transport)
client = XleThriftService::Client.new(protocol)
transport.open
puts client.send(:parseQuery, query, parameters, details)
transport.close
