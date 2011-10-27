require 'fuzed_auto_config'

environment = FUZEDAutoConfig.new
options = {:target => :default, :master => :default}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed console [target/master]"
  opts.on("-t", "--target TARGETNODE", "Node to target, defaults to your cluster's master or fuzed-dev") do |arg|
    options[:target] = arg
  end

  opts.on("-m", "--master TARGETNODE", "Master node for cookie generation. Defaults to your target.") do |arg|
    options[:master] = arg
  end
end.parse!
options[:target] = environment.master_nodename if options[:target] == :default
options[:master] = options[:target] if options[:master] == :default

line = %{erl -setcookie #{cookie_hash(options[:master])} -remsh #{options[:target]} -name 'fuzed-shell-#{rand(1000)}'}
puts line
exec(line)


