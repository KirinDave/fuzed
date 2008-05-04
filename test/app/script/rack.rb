#!/usr/bin/env ruby

require File.dirname(__FILE__) + '/../config/boot'
require RAILS_ROOT + "/config/environment"

require 'erlectricity'
require 'stringio'
require 'logger'

$logger = Logger.new(RAILS_ROOT + "/log/fuzed.#{Process.pid}.log")

def log(msg)
  $logger.info(msg)
end

module Rack
  module Handler
    class Fuzed
      def self.run(app)
        Fuzed.new(app).listen
      end
      
      def initialize(app)
        @app = app
      end
      
      def listen
        log "Waiting for connections"
        
        me = self
        
        receive(IO.new(3), IO.new(4)) do
          match(:request, list(:request)) do
            $t = Time.now
            log('------------MATCH------------')
            log request.inspect
            res = me.service(request)
            send!(res)
            log(">> Total in " + (Time.now - $t).to_s + " sec\n")
            receive_loop
          end
          
          match(:ping) do
            send!(:pong)
            receive_loop
          end
          
          match(any(:any)) do
            log('------------NO-MATCH------------')
            log any.inspect
            receive_loop
          end
        end
      end
      
      def service(vars)
        request = vars.inject({}) { |a, x| a[x[0]] = x[1]; a }
        
        method = request[:method]
        version = request[:http_version] # => e.g. [1, 1]
        path = request[:querypath]
        query = request[:querydata]
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
        
        env.update({"rack.version" => [0,2],
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
        
        log('------------IN------------')
        log(env.inspect)
        
        begin
          t1 = Time.now
          status, headers, body = @app.call(env)
          log(">> Rails in " + (Time.now - t1).to_s + " sec")
        
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
          
        log('-----------OUT------------')
        log(res.inspect)
        
        res
      end
    end
  end
end

###############################################################################

unless defined? RAILS_ROOT
  raise "Rails' environment has to be loaded before using Rack::Adapter::Rails"
end

require "rack/request"
require "rack/response"
require "dispatcher"

module Rack
  module Adapter 
    class Rails
      def call(env)
        request = Request.new(env)
        response = Response.new
        
        cgi = CGIStub.new(request, response)
        
        Dispatcher.dispatch(cgi, ActionController::CgiRequest::DEFAULT_SESSION_OPTIONS, response)
        
        response.finish
      end
      
      protected
      
      class CGIStub < ::CGI
        
        def initialize(request, response, *args)
          @request = request
          @response = response
          @args = *args
          @input = request.body
          super(*args)
        end
        
        IGNORED_HEADERS = [ "Status" ]
        
        def header(options = "text/html")
          # puts 'header---------------'
          # p options
          # puts '---------------------'
          
          if options.instance_of?(String)
            @response['Content-Type'] = options unless @response['Content-Type']
          else
            @response['Content-Length'] = options.delete('Content-Length').to_s if options['Content-Length']
            
            @response['Content-Type'] = options.delete('type') || "text/html"
            @response['Content-Type'] += "; charset=" + options.delete('charset') if options['charset']
                        
            @response['Status'] = options.delete('status') if options['status']
            @response['Content-Language'] = options.delete('language') if options['language']
            @response['Expires'] = options.delete('expires') if options['expires']
        
            IGNORED_HEADERS.each {|k| options.delete(k) }
        
            options.each{|k,v| @response[k] = v}
            
            # convert 'cookie' header to 'Set-Cookie' headers
            if cookie = @response['cookie']
              case cookie
                when Array
                  cookie.each {|c| @response['Set-Cookie'] = c.to_s }
                when Hash
                  cookie.each_value {|c| @response['Set-Cookie'] = c.to_s}
                else
                  @response['Set-Cookie'] = options['cookie'].to_s
              end
        
              @output_cookies.each { |c| @response['Set-Cookie'] = c.to_s } if @output_cookies
            end
          end
        
          ""
        end
                        
        def params
          @request.params
        end
        
        def cookies
          @request.cookies
        end
        
        def query_string
          @request.query_string
        end
          
        # Used to wrap the normal args variable used inside CGI.
        def args
          @args
        end
    
        # Used to wrap the normal env_table variable used inside CGI.
        def env_table
          @request.env
        end
    
        # Used to wrap the normal stdinput variable used inside CGI.
        def stdinput
          @input
        end
        
        def stdoutput
          STDERR.puts "stdoutput should not be used."
          @response.body
        end  
      end
    end
  end
end

###############################################################################

require 'rack'
require 'rack/cascade'
require 'rack/showexceptions'
# Rack::Handler::Fuzed.run \
#   Rack::ShowExceptions.new(Rack::Lint.new(Rack::Adapter::Rails.new))

if ARGV.first != 'test'
  Rack::Handler::Fuzed.run(Rack::Adapter::Rails.new)
else
  req = 
  [[:method, :POST], [:http_version, [1, 1]], [:querypath, "/main/go"], [:querydata, ""], [:servername, "testing:8002"], [:headers, [[:connection, "keep-alive"], [:accept, "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"], [:host, "localhost:8002"], [:referer, "http://localhost:8002/main/ready"], [:user_agent, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3"], [:keep_alive, "300"], [:content_length, "7"], [:content_type, "application/x-www-form-urlencoded"], [:"Cache-Control", "max-age=0"], [:"Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7"], [:"Accept-Encoding", "gzip,deflate"], [:"Accept-Language", "en-us,en;q=0.5"]]], [:cookies, ["_helloworld_session_id=d3eae987aab3230377abc433b7a8d7c1"]], [:pathinfo, "/Users/tom/dev/fuzed/helloworld/public"], [:postdata, "val=foo"]]
  
  # [[:method, :GET], [:http_version, [1, 1]], [:querypath, "/main/say"], [:querydata, ""], [:servername, "testing:8002"], [:headers, [[:connection, "keep-alive"], [:accept, "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"], [:host, "localhost:8002"], [:user_agent, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3"], [:keep_alive, "300"], [:"Cache-Control", "max-age=0"], [:"Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7"], [:"Accept-Encoding", "gzip,deflate"], [:"Accept-Language", "en-us,en;q=0.5"]]], [:cookies, ["_helloworld_session_id=166098a3c3f702698d0529c6148c6164"]], [:pathinfo, "/Users/tom/dev/fuzed/helloworld/public"], [:postdata, :undefined]]

  p Rack::Handler::Fuzed.new(Rack::Adapter::Rails.new).service(req)
end
