options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: id2 leave -n NAME"
  
  opts.on("-n NAME", "--name NAME", "Node name (hostname will automatically be appended)") do |n|
    options[:name] = n
  end
end.parse!
  
node = options[:name] + "@" + `hostname`.strip

cmd = %Q{erl -noshell -name nodekiller -eval "rpc:call('#{node}', erlang, halt, []), halt()."}
puts(cmd)
exec(cmd)