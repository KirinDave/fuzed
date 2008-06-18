$rails_json = true

# core
require 'optparse'

# internal
require 'chassis'
require 'rails_adapter'
require 'fuzed_handler'

# read command line options
options = {}
opts = OptionParser.new do |opts|
  opts.on("-r", "--rails-root RAILS_ROOT", String) do |x| 
    options[:rails_root] = x
  end
  opts.on("-t", "--test", "enable test mode") do 
    options[:test] = true
  end
  opts.on("-e", "--rails-env ENV", String) do |x|
    options[:rails_env] = x
  end
end
opts.parse(ARGV)
options[:rails_root] = File.join(File.dirname(__FILE__), *%w[.. test app]) if options[:test]
options[:rails_env] ||= 'development'

# app
app = Rack::Adapter::Rails.new(:root => options[:rails_root], :environment => options[:rails_env])
logfile = options[:rails_root] + "/log/fuzed.#{Process.pid}.log"
$handler = Rack::Handler::Fuzed.new(app, logfile)

# chassis
class RailsHandler < Chassis
  kind "rails"

  handle(:handle_request, :request) do |args|
    $handler.service(args[:request])
  end
end

# test mode
if options[:test]  
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
  
  p $handler.service(req)
  exit!
end