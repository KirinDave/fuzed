rails_root = "/Users/tom/dev/git/helloworld"

%w{3000 3001}.each do |port|
  God.watch do |w|
    w.name = "mongrel-#{port}"
    w.start = "mongrel_rails start -c #{rails_root} \
      -P #{rails_root}/log/mongrel-#{port}.pid -p #{port} -d"
    w.stop = "mongrel_rails stop \
      -P #{rails_root}/log/mongrel-#{port}.pid"
    w.pid_file = "#{rails_root}/log/mongrel-#{port}.pid"
    w.interval = 5.seconds
    w.log = File.join(rails_root, "log/commands.#{port}.log")
  
    # clean pid files before start if necessary
    w.behavior(:clean_pid_file)
  
    # determine the state on startup
    w.transition(:init, { true => :up, false => :start }) do |on|
      on.condition(:process_running) do |c|
        c.running = true
      end
    end
  
    # determine when process has finished starting
    w.transition([:start, :restart], :up) do |on|
      on.condition(:process_running) do |c|
        c.running = true
      end
    
      # failsafe
      on.condition(:tries) do |c|
        c.times = 8
        c.within = 2.minutes
        c.transition = :start
      end
    end

    # start if process is not running
    w.transition(:up, :start) do |on|
      on.condition(:process_exits)
    end
  
    # restart if memory or cpu is too high
    w.transition(:up, :restart) do |on|
      on.condition(:memory_usage) do |c|
        c.interval = 20
        c.above = 50.megabytes
        c.times = [3, 5]
      end
    
      on.condition(:cpu_usage) do |c|
        c.interval = 10
        c.above = 10.percent
        c.times = 5
      end
    
      on.condition(:http_response_code) do |c|
        c.host = 'localhost'
        c.port = port
        c.path = '/'
        c.code_is_not = 200
        c.timeout = 10.seconds
        c.times = [3, 5]
      end
    end
  
    # lifecycle
    w.lifecycle do |on|
      on.condition(:flapping) do |c|
        c.to_state = [:start, :restart]
        c.times = 5
        c.within = 1.minute
        c.transition = :unmonitored
        c.retry_in = 10.minutes
        c.retry_times = 5
        c.retry_within = 2.hours
      end
    end
  end
end