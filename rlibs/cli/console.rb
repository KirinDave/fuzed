require 'id2_auto_config'

environment = ID2AutoConfig.new
options = {:target => :default, :master => :default}
OptionParser.new do |opts|
  opts.banner = "Usage: id2 console [target/master]"
  opts.on("-t", "--target TARGETNODE", "Node to target, defaults to your cluster's master or id2-dev") do |arg|
    options[:target] = arg
  end
  
  opts.on("-m", "--master TARGETNODE", "Master node for cookie generation. Defaults to your target.") do |arg|
    options[:master] = arg
  end
end.parse!
options[:target] = environment.master_nodename if options[:target] == :default
options[:master] = options[:target] if options[:master] == :default

line = %{erl -setcookie #{cookie_hash(options[:master])} -remsh #{options[:target]} -name 'id2-shell-#{rand(1000)}'}
puts line
exec(line)


