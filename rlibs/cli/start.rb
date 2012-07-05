require 'fileutils'

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: fuzed start [options]"

  opts.on("-n NAME", "--name NAME", "Node name") do |n|
    options[:name] = n
  end

  opts.on("--server SERVER", "HTTP Server to use. Choices are: mochiweb, yaws") do |server|
    $stderr.puts "Unknown server type. Using yaws." unless %w[mochiweb yaws].include?(server)
    options[:http_server] = server
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
http_server = options[:http_server] || DEFAULT_HTTP_SERVER

if nodename !~ /@/
  abort "Please specify fully qualified node name e.g. -n master@fuzed.tools.powerset.com"
end

# This is obnoxious, but needs to happen for yaws.
dotyaws = ENV["HOME"] + "/.yaws/"
FileUtils.mkdir(dotyaws) unless File.exists?(dotyaws)

cmd = %Q{erl -boot start_sasl \
             #{detached} \
             +Bc \
             +K true \
             -smp enable \
             #{code_paths}
             -name '#{nodename}' \
             -fuzed http_server '#{http_server}' \
             -config '#{FUZED_ROOT}/conf/fuzed_base' \
             -setcookie #{cookie_hash(nodename)} \
             -kernel start_boot_server true \
             -run fuzed start}.squeeze(' ')
puts cmd
exec(cmd)
