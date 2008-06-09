
# Takes "x=y|z=1.2.3|tags=1,2" into
# [{<<"x">>, <<"y">>}, {<<"z">>, <<"1.2.3">>},
#  {<<"tags">>, [<<"1">>, <<"2">>]}]
def details_from_string(str)
  clauses = str.strip.split(/\|/).compact
  forms = clauses.map do |clause|
    sides = clause.split(/=/)
    raise "Malformed clause: \"#{clause}\"!" unless sides.size == 2
    lside,rside = sides
    %|{<<"#{purge(lside)}">>, #{rside_details(lside,rside)}}|
  end
  "'[" + forms.join(",") + "]'"
end

def rside_details(lstring,rstring)
  if lstring == "roles" || lstring == "tags"
    "[" + rstring.split(',').compact.map {|x| "<<\"" + purge(x) + "\">>"}.join(',') + "]"
  else
    # TODO: implement versions properly
    "\<\<\"" + purge(rstring) + "\"\>\>"
  end
end

def purge(str)
  str.gsub(/"/, '\"').gsub(/'/, '\'')
end


options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed command [options]"
  
  opts.on("-z HOSTNAME", "--magic HOSTNAME", "Set smart details based off of a hostname") do |n|
    options[:master_name] = "master@#{n}"
  end
  
  opts.on("-n NAME", "--name NAME", "Node name") do |n|
    options[:name] = n
  end
  
  opts.on("-m NAME", "--master NAME", "Master node name") do |n|
    options[:master_name] = n
  end
  
  opts.on("-s", "--spec SPECSTRING", "||-separated list of arguments to apply to your FUZED node") do |n|
    options[:spec] = n
  end
  
  opts.on("-d", "--detached", "Run as a daemon") do
    options[:detached] = true
  end

  opts.on("-r", "--docroot ROOT", "Docroot for server. Default is /tmp") do |dir|
    options[:docroot] = dir
    raise "No such directory for docroot!" unless File.directory?(dir)
  end
  
  opts.on("--ssl-key KEY", "SSL key file.") do |key|
    options[:ssl_key] = key
  end
  
  opts.on("--ssl-cert CERT", "SSL cert file.") do |cert|
    options[:ssl_cert] = cert
  end

  opts.on("-p", "--port PORT", "Port for web server.") do |dir|
    options[:port] = dir
  end

  opts.on("-f", "--frontend-responder MODULE", "Module to use for Pool calculation.") do |mod|
    options[:module] = mod
  end

  opts.on("-a", "--appmod-specs APPMOD_SPECS", "List of triples, {path, module, role}.") do |specs|
    options[:appmod_specs] = specs
  end

  opts.on("-j", "--json-api ROLE", "A role to bind to a json-rpc /api") do |role|
    raise "Don't mix -a and -j!" unless options[:appmod_specs].nil?
    options[:appmod_specs] = "[{" + %["api",api_responder,#{role.to_s}] + "}]"
  end

  opts.on("-?", "--help", "Display arguments.") do
    puts opts
    exit(0)
  end
end.parse!
  
detached = options[:detached] ? '-detached' : ''
master = options[:master_name] || DEFAULT_MASTER_NODE
nodename = options[:name] || DEFAULT_NODE_NAME
docroot = options[:docroot] || "/tmp"
spec = options[:spec] || "kind=normal"
details = details_from_string(spec)
port = options[:port] || "8080"
mod = if options[:module]
        "-fuzed_frontend responder #{options[:module]}"
      else
        ""
      end

fuzed_appspecs = if options[:appmod_specs]
                   "-fuzed_frontend appmods '" + options[:appmod_specs] + "'"
                 else
                   ""
                 end

if master !~ /@/
  abort "Please specify fully qualified master node name e.g. -m master@fuzed.tools.powerset.com"
end

ssl_details = ''
if options[:ssl_key] && options[:ssl_cert]
  ssl_details << %Q{-fuzed_frontend ssl_key '"#{options[:ssl_key]}"' }
  ssl_details << %Q{-fuzed_frontend ssl_cert '"#{options[:ssl_cert]}"' }
end

cmd = %Q{erl -boot start_sasl \
             #{detached} \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -name '#{nodename}' \
             -setcookie #{cookie_hash(master)} \
             -fuzed_frontend master "'#{master}'" \
             -fuzed_frontend details #{details} \
             -fuzed_frontend docroot '"#{docroot}"' \
             #{ssl_details} \
             -fuzed_frontend port #{port} \
             #{fuzed_appspecs} \
             #{mod} \
             -config '#{FUZED_ROOT}/conf/fuzed_base' \
             -run fuzed_frontend start}.squeeze(' ')
puts cmd
exec(cmd)
