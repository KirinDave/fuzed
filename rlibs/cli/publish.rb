require 'fuzed_auto_config'

environment = FUZEDAutoConfig.new
options = {:master => :default}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed publish [-m master]"
  
  opts.on("-m", "--master TARGETNODE", "Master node for cookie generation. Defaults to your target.") do |arg|
    options[:master] = arg
  end
end.parse!
options[:master] = environment.master_hostname if options[:master] == :default

line = %{curl http://#{options[:master]}:9001/api -d '{"method":"publish","params":{}}'}
puts line
exec(line)
