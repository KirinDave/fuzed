rails_root = "/Users/tom/dev/git/helloworld"
port = 3000

God.watch do |w|
  w.name = "mongrel-#{port}"
  w.start = "mongrel_rails start -c #{rails_root} \
    -P #{rails_root}/log/mongrel-#{port}.pid -p #{port} -d"
  w.stop = "mongrel_rails stop -P #{rails_root}/log/mongrel-#{port}.pid"
  w.pid_file = "#{rails_root}/log/mongrel-#{port}.pid"
  w.interval = 5.seconds
  w.start_grace = 5.seconds

  w.behavior(:clean_pid_file)

  w.start_if do |on|
    on.condition(:process_running) do |c|
      c.running = false
    end
  end

  w.restart_if do |on|
    on.condition(:cpu_usage) do |c|
      c.above = 20.percent
      c.times = 5
    end
  end
end

