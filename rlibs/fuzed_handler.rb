LOG = true

# core
require 'stringio'
require 'logger'

# gems
require 'rack'

# initialize logging info
$total_avg = [0, 0]
$rails_avg = [0, 0]

# Log the given message
#   +msg+ is the message to log
#
# Returns nothing
def log(msg)
  $logger << msg + "\n"
end

module Rack
  module Handler
    class Fuzed
      def initialize(app, logfile)
        @app = app
        $logger = Logger.new(logfile)
      end
      
      def service(request)
        log('------------RAW IN------------') if LOG
        log(request.inspect) if LOG
        
        method = request['method']
        version = request['http_version'] # => e.g. [1, 1]
        path = request['querypath']
        query = request['querydata'] == :undefined ? '' : request['querydata']
        server = request['servername']
        headers = request['headers']
        cookies = request['cookies']
        postdata = request['postdata'] == :undefined ? '' : request['postdata']
          
        translate = {'content_type' => 'CONTENT_TYPE',
                     'content_length' => 'CONTENT_LENGTH',
                     'authorization' => 'HTTP_AUTHORIZATION',
                     'accept' => 'HTTP_ACCEPT',
                     'Accept-Charset' => 'HTTP_ACCEPT_CHARSET',
                     'Accept-Encoding' => 'HTTP_ACCEPT_ENCODING',
                     'Accept-Language' => 'HTTP_ACCEPT_LANGUAGE',
                     'connection' => 'HTTP_CONNECTION',
                     'keep_alive' => 'HTTP_KEEP_ALIVE',
                     'host' => 'HTTP_HOST',
                     'referer' => 'HTTP_REFERER',
                     'user_agent' => 'HTTP_USER_AGENT',
                     'X-Prototype-Version' => 'HTTP_X_PROTOTYPE_VERSION',
                     'X-Requested-With' => 'HTTP_X_REQUESTED_WITH'}
               
        env = {}
        env['REQUEST_METHOD'] = method.to_s
        env['QUERY_STRING'] = query
        env["PATH_INFO"] = path
        env = headers.inject(env) { |a, x| a[translate[x[0]] || x[0].to_s] = x[1]; a }
        env.delete_if { |k, v| v.nil? }
  
        env.update({"rack.version" => [0, 1],
                    "rack.input" => StringIO.new(postdata),
                    "rack.errors" => STDERR,
              
                    "rack.multithread" => true,
                    "rack.multiprocess" => false,
                    "rack.run_once" => false,
              
                    "rack.url_scheme" => request['https'] == 1 ? "https" : "http"
                  })
             
        env['SERVER_NAME'] = server.split(':')[0]
        env['SERVER_PORT'] = server.split(':')[1]
        env['HTTP_VERSION'] = version.join('.')
        env['HTTPS'] = request['https'] == 1 ? "on" : "off"
  
        env["HTTP_VERSION"] ||= env["SERVER_PROTOCOL"]
        env["QUERY_STRING"] ||= ""
        env["REQUEST_PATH"] ||= "/"
        env.delete "PATH_INFO" if env["PATH_INFO"] == ""
  
        cookies.each do |cookie|
          env["HTTP_COOKIE"] = cookie.to_s
        end
  
        log('------------IN------------') if LOG
        log(env.inspect) if LOG
  
        begin
          t1 = Time.now
    
          status, headers, body = @app.call(env)
    
          rails_delta = Time.now - t1
          $rails_avg[0] += rails_delta
          $rails_avg[1] += 1
          log(">> Rails in #{rails_delta} (#{$rails_avg[0] / $rails_avg[1]} avg) sec") if LOG
  
          html = ''
          body.each do |part|
            html << part
          end
    
          headers['Server'] = 'YAWS + Fuzed 0.0.1'
          headers['Connection'] = 'close'
    
          cookies = headers.delete('cookie')
          headers['Set-Cookie'] = cookies if cookies
    
          status = (headers["Status"].split(" ").first rescue nil) || status
          headers.delete("Status") if headers["Status"]

          res = 
          [:response,
           [[:status, status.to_i],
            [:allheaders, headers.inject([]) { |a, x| a += x[1].map { |y| [:header, x[0], y] } }],
            [:html, html]]]
        rescue => e
          res = 
          [:response, 
            [[:status, 500],
             [:allheaders, [
               [:header, "Content-Type", "text/plain; charset=utf-8"], 
               [:header, "Cache-Control", "no-cache"]]], 
             [:html, "500 Internal Error\n\n#{e}\n\n#{e.backtrace}"]]]
        end
    
        log('-----------OUT------------') if LOG
        log(res.inspect) if LOG
  
        res
      end
    end
  end
end