options = { :kind => :all, :name => "funtimes" }

OptionParser.new do |opts|
  opts.banner = "Usage: id2 upgrade [options]"
  
  opts.on("-n NAME", "--name NAME", "Node name") do |n|
    options[:name] = n
  end
  
  opts.on("-m NAME", "--master NAME", "Master node name") do |n|
    options[:master_name] = n
  end
  
  opts.on("-r", "--ruby_only", "Only reload Ruby segment") do |n|
    options[:kind] = :ruby
  end
  
  opts.on("-e", "--erlang_only", "Only reload Erlang segment") do |n|
    options[:kind] = :erlang
  end
end.parse!
  
command = ARGV[0]

name = options[:name]
master = options[:master_name] || DEFAULT_MASTER_NODE
kind = options[:kind]

if master !~ /@/
  abort "Please specify fully qualified master node name e.g. -m master@id2.tools.powerset.com"
end

cmd = %Q{erl -noshell -name '#{name}' -setcookie #{cookie_hash(master)} \
             -eval "rpc:call('#{master}', id2_code_monitor, global_upgrade, ['#{kind}']), halt()."}.squeeze(' ')
puts(cmd)
exec(cmd)