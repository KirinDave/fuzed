options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: id2 start [options]"
  
  opts.on("-n NAME", "--name NAME", "Node name") do |n|
    options[:name] = n
  end
  
  opts.on("-d", "--detached", "Run as a daemon") do
    options[:detached] = true
  end

  opts.on("-h", "--heartbeat", "Start with a heartbeat.") do
    $stderr.puts "WARNING! Heartbeats not supported with this build!"
  end
end.parse!
  
command = ARGV[0]

detached = options[:detached] ? '-detached' : ''
nodename = options[:name] || DEFAULT_MASTER_NODE

if nodename !~ /@/
  abort "Please specify fully qualified node name e.g. -n master@id2.tools.powerset.com"
end

cmd = %Q{erl -boot start_sasl \
             #{detached} \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -name '#{nodename}' \
             -config '#{ID2_ROOT}/conf/id2_base' \
             -setcookie #{cookie_hash(nodename)} \
             -kernel start_boot_server true \
             -run id2 start}.squeeze(' ')
puts cmd
exec(cmd)
