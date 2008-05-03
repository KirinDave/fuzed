options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed local [options]"
  
  opts.on("-o NUMNODES", "--num_nodes NUMNODES", "Number of nodes to run") do |n|
    options[:num_nodes] = n
  end
  
  opts.on("-t", "--tags TAGSTRING", "Comma-separated list of tags to apply to any nodes started") do |n|
    options[:tags] = n
  end
end.parse!
  
command = ARGV[0]

num_nodes = options[:num_nodes]
tagarg = options[:tags] ? %{ -fuzed_node tags '"#{options[:tags]}"' } : ""
cmd = %Q{erl -boot start_sasl \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -sname local_console_#{$$} \
             #{tagarg} \
             -run stack start}.squeeze(' ')
puts(cmd)
exec(cmd)