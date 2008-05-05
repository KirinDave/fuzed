# core
require 'stringio'
require 'logger'

# internal
require 'chassis'
require 'rails_adapter'

# gems
require 'rack'

TESTMODE = false

if TESTMODE
  rails_root = File.join(File.dirname(__FILE__), *%w[.. test app])
else
  rails_root = ARGV[0] || File.join(File.dirname(__FILE__), *%w[.. test app])
end

require File.join(rails_root, 'config/boot')
require RAILS_ROOT + "/config/environment"

LOG = true

$total_avg = [0, 0]
$rails_avg = [0, 0]
$logger = Logger.new(RAILS_ROOT + "/log/fuzed.#{Process.pid}.log")

def log(msg)
  $logger << msg + "\n"
end

$app = Rack::Adapter::Rails.new(:root => RAILS_ROOT)

def service(request)
  method = request[:method]
  version = request[:http_version] # => e.g. [1, 1]
  path = request[:querypath]
  query = request[:querydata] == :undefined ? '' : request[:querydata]
  server = request[:servername]
  headers = request[:headers]
  cookies = request[:cookies]
  postdata = request[:postdata] == :undefined ? '' : request[:postdata]
          
  translate = {:content_type => 'CONTENT_TYPE',
               :content_length => 'CONTENT_LENGTH',
               :accept => 'HTTP_ACCEPT',
               :'Accept-Charset' => 'HTTP_ACCEPT_CHARSET',
               :'Accept-Encoding' => 'HTTP_ACCEPT_ENCODING',
               :'Accept-Language' => 'HTTP_ACCEPT_LANGUAGE',
               :connection => 'HTTP_CONNECTION',
               :keep_alive => 'HTTP_KEEP_ALIVE',
               :host => 'HTTP_HOST',
               :referer => 'HTTP_REFERER',
               :user_agent => 'HTTP_USER_AGENT',
               'X-Prototype-Version' => 'HTTP_X_PROTOTYPE_VERSION',
               'X-Requested-With' => 'HTTP_X_REQUESTED_WITH'}
               
  env = {}
  env['REQUEST_METHOD'] = method.to_s
  env['QUERY_STRING'] = query
  env["PATH_INFO"] = path == '/' ? '' : path
  env = headers.inject(env) { |a, x| a[translate[x[0]] || x[0].to_s] = x[1]; a }
  env.delete_if { |k, v| v.nil? }
  
  env.update({"rack.version" => [0, 1],
              "rack.input" => StringIO.new(postdata),
              "rack.errors" => STDERR,
              
              "rack.multithread" => true,
              "rack.multiprocess" => false,
              "rack.run_once" => false,
              
              "rack.url_scheme" => ["yes", "on", "1"].include?(ENV["HTTPS"]) ? "https" : "http"
            })
             
  env['SERVER_NAME'] = server.split(':')[0]
  env['SERVER_PORT'] = server.split(':')[1]
  env['HTTP_VERSION'] = version.join('.')
  
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
    
    # status = '200'
    # headers = {}
    # body = ['foo']
    
    status, headers, body = $app.call(env)
    
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
    #cookies.map! {|c| c.include?('path=') ? c : c + "; path=/"}
    headers['Set-Cookie'] = cookies if cookies
    
    # p headers
    
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
      
class RailsHandler < Chassis
  kind "rails"

  details("rails" => "default")

  handle(:handle_request, :request) do |args|
    service(args[:request])
  end
end

if TESTMODE
  # [[:method, :POST], [:http_version, [1, 1]], [:querypath, "/main/go"], [:querydata, ""], [:servername, "testing:8002"], [:headers, [[:connection, "keep-alive"], [:accept, "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"], [:host, "localhost:8002"], [:referer, "http://localhost:8002/main/ready"], [:user_agent, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3"], [:keep_alive, "300"], [:content_length, "7"], [:content_type, "application/x-www-form-urlencoded"], [:"Cache-Control", "max-age=0"], [:"Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7"], [:"Accept-Encoding", "gzip,deflate"], [:"Accept-Language", "en-us,en;q=0.5"]]], [:cookies, ["_helloworld_session_id=d3eae987aab3230377abc433b7a8d7c1"]], [:pathinfo, "/Users/tom/dev/fuzed/helloworld/public"], [:postdata, "val=foo"]]
  
  req = 
  {:method => :POST, 
   :http_version => [1, 1], 
   :querypath => "/main/go", 
   :querydata => "", 
   :servername => "testing:8002", 
   :headers => {:connection => "keep-alive",
                :accept => "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5", 
                :host => "localhost:8002",
                :referer => "http://localhost:8002/main/ready",
                :user_agent => "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3",
                :keep_alive => "300",
                :content_length => "7",
                :content_type => "application/x-www-form-urlencoded",
                :"Cache-Control" => "max-age=0",
                :"Accept-Charset" => "ISO-8859-1,utf-8;q=0.7,*;q=0.7",
                :"Accept-Encoding" => "gzip,deflate",
                :"Accept-Language" => "en-us,en;q=0.5"},
   :cookies => ["_helloworld_session_id=d3eae987aab3230377abc433b7a8d7c1"],
   :pathinfo => "/Users/tom/dev/fuzed/helloworld/public",
   :postdata => "val=foo"}
  
  # [[:method, :GET], [:http_version, [1, 1]], [:querypath, "/main/say"], [:querydata, ""], [:servername, "testing:8002"], [:headers, [[:connection, "keep-alive"], [:accept, "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"], [:host, "localhost:8002"], [:user_agent, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3"], [:keep_alive, "300"], [:"Cache-Control", "max-age=0"], [:"Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7"], [:"Accept-Encoding", "gzip,deflate"], [:"Accept-Language", "en-us,en;q=0.5"]]], [:cookies, ["_helloworld_session_id=166098a3c3f702698d0529c6148c6164"]], [:pathinfo, "/Users/tom/dev/fuzed/helloworld/public"], [:postdata, :undefined]]

  p service(req)
  p service(req)
  p service(req)
  exit!
end