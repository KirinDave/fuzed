require 'id2_auto_config'

environment = ID2AutoConfig.new
options = {:master => :default}
OptionParser.new do |opts|
  opts.banner = "Usage: id2 publish [-m master]"
  
  opts.on("-m", "--master TARGETNODE", "Master node for cookie generation. Defaults to your target.") do |arg|
    options[:master] = arg
  end
end.parse!
options[:master] = environment.master_hostname if options[:master] == :default

line = %{curl http://#{options[:master]}:9001/api -d '{"method":"unpublish","params":{}}'}
puts line
exec(line)
