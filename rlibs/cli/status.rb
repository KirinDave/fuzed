# ---
# port: 9001
# host: localhost
# specs:
# - foo/2
# - bar/4

require 'net/http'
require 'yaml'
require 'fuzed_auto_config'

options = {:host => :auto, :port => 9001, :spec => []}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed status [options]"

  opts.on("-c CONFIG", "--config CONFIG", "Configuration file") do |x|
    contents = File.read(x)
    data = YAML.load(contents)
    options[:host] = data['host'] if data['host']
    options[:port] = data['port'] if data['port']
    options[:spec] += data['specs'] if data['specs']
  end

  opts.on("-h HOST", "--host HOST", "Hostname. Default: localhost") do |x|
    options[:host] = x
  end

  opts.on("-p PORT", "--port PORT", "Port. Default: 9001") do |x|
    options[:port] = x
  end

  opts.on("-s SPEC", "--spec SPEC", "Specify a kind/number to check on") do |n|
    options[:spec] << n
  end
end.parse!

# Won't work on darwin
if options[:host] == :auto
  env = FUZEDAutoConfig.new
  options[:host] = environment.master_hostname
end

# display hte endpoint
puts "Using endpoint - http://#{options[:host]}:#{options[:port]}/status"

failed = false

begin
  options[:spec].each do |spec|
    res = Net::HTTP.start(options[:host], options[:port]) {|http|
      http.get('/status/' + spec)
    }

    if res.instance_of?(Net::HTTPInternalServerError)
      puts "[ fail ] #{res.body}"
      failed = true
    else
      puts "[  ok  ] #{res.body}"
    end
  end
rescue Exception => e
  puts "[ fail ] #{e.message}"
  failed = true
end

exit(1) if failed
