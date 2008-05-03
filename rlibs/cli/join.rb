def create_spec_list(options)
  spec_list = []
  result = ""
  result.concat("/p/bin/xlew ") if options[:xlew]
  result.concat("ruby -I/p/libexec/fuzed #{FUZED_ROOT + "/rlibs/pontoon.rb "}")
  raise "You must specify an fuzed file to join the fuzed cluster!" unless options[:fuzedfile]
  result.concat(options[:fuzedfile] + " ")
  result.concat(%{#{options[:remote_ruby] || DEFAULT_REMOTE_RUBY} })
  result.concat(%{--tags="#{options[:tags]}" }) if options[:tags]
  roles = []
  roles << "production" if options[:production]
  roles << options[:roles] if options[:roles]
  result.concat(%{--roles="#{roles.join(",")}"}) unless roles.empty?
  result.concat(%{ -- }) # Separates required arguments from optionals
  spec_list = if options[:spec]
    specs = options[:spec].split(/\|\|/)
    specs.map {|x| result + x}
  else
    [result]
  end
  spec_list.map {|x| x.gsub(%r|\\|,'\\\\\\').gsub(%r|([\"\'])|, '\\\\\1')} # Make sure to escape things properly.
end

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed command [options]"
  
  opts.on("-z HOSTNAME", "--magic HOSTNAME", "Set smart details based off of a hostname") do |n|
    options[:master_name] = "master@#{n}"
    options[:remote_ruby] = "http://#{n}:9001/code"
  end
  
  opts.on("-n NAME", "--name NAME", "Node name") do |n|
    options[:name] = n
  end
  
  opts.on("-f FUZEDFILE", "--fuzedfile FILENAME", "FUZED spec file to use to serve nodes") do |n|
    options[:fuzedfile] = n
  end
  
  opts.on("-m NAME", "--master NAME", "Master node name") do |n|
    options[:master_name] = n
  end
  
  opts.on("-s", "--spec SPECSTRING", "||-separated list of arguments to apply to your FUZED node") do |n|
    options[:spec] = n
  end
  
  opts.on("-c NUMNODES", "--clone NUMTIMES", "Number of clones of your spec to make") do |n|
    options[:num_nodes] = n
  end
  
  opts.on("-t", "--tags TAGSTRING", "Comma-separated list of tags to apply to any nodes started") do |n|
    options[:tags] = n
  end
  
  opts.on("-x", "--xlew", "Make sure to run the node in the powerset nl environment") do |n|
    options[:xlew] = true
  end
  
  opts.on("-d", "--detached", "Run as a daemon") do
    options[:detached] = true
  end
  
  opts.on("-p", "--produciton", "Classify these nodes as production") do
    options[:production] = true
  end
  
  opts.on("-r", "--roles ROLES", "Extra roles (use -p for production role)") do |v|
    options[:roles] = v
  end
  
  opts.on("-i", "--inet", "Load code over internet via master code server") do
    options[:inet] = true
  end
  
  opts.on("--remote_ruby LOCATION", "Load Ruby code from the given URL or file location") do |x|
    options[:remote_ruby] = x
  end

  opts.on("-h", "--heartbeat", "Start with a heartbeat.") do
    $stderr.puts "WARNING! Heartbeats not supported with this build!"
  end
end.parse!
  
command = ARGV[0]

detached = options[:detached] ? '-detached' : ''
master = options[:master_name] || DEFAULT_MASTER_NODE
nodename = options[:name] || DEFAULT_NODE_NAME

if master !~ /@/
  abort "Please specify fully qualified master node name e.g. -m master@fuzed.tools.powerset.com"
end

spec = %{[} + create_spec_list(options).map {|x| %{"#{x}"}}.join(",") + %{]}
num_nodes = options[:num_nodes] || 1

inet = 
if options[:inet]
  # Ruby has an awesome bug that makes me have to shell out to it, I'm not insane, I swear
  ip = `ruby -e "require 'resolv'; puts Resolv.getaddress('#{master.split('@').last}') rescue ''"`.chomp
  ip = `ruby -e "require 'resolv'; puts Resolv.getaddress('#{master.split('@').last.split('.').first}') rescue ''"`.chomp if ip == ''
  
  abort("Could not resolve #{master.split('@').last} to an IP address") unless ip
  
  " -loader inet -hosts '#{ip}'"
else
  ""
end

remote_ruby = options[:remote_ruby] || DEFAULT_REMOTE_RUBY

cmd = %Q{erl -boot start_sasl \
             #{detached} \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -name '#{nodename}' \
             -setcookie #{cookie_hash(master)} \
             -fuzed_node master "'#{master}'" \
             -fuzed_node spec '#{spec}' \
             -fuzed_node num_nodes #{num_nodes} \
             #{inet} \
             -config '#{FUZED_ROOT}/conf/fuzed_base' \
             -run fuzed_node start}.squeeze(' ')
puts cmd
exec(cmd)
