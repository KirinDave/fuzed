# by brendan 5/07 modifications by CVP 6/07
require 'rubygems'
require 'json'

module Powerset
  module QuantumLeap
    class JSONClient
      attr_accessor :uri, :prefix
      def initialize(uri, prefix='')
        @uri = uri.is_a?(String) ? URI.parse(uri) : uri
        raise "need a legitimate URI" unless @uri.host
        @prefix = prefix
      end
    
      # can use symbols for arg names if you want
      def call(methodname, arghash={})
        arghash.is_a?(Hash) or raise "must pass hash args"
    
        envelope = {
          'method' => methodname,
          'params' => arghash
        }
        
        puts '--------------------'
        puts envelope.to_json
        puts '--------------------'
        
        body = Net::HTTP.new(@uri.host, @uri.port).post(@uri.path, envelope.to_json, {'Content-Type' => 'application/json'}).body
        jr = JSON.parse(body) || {} rescue {}
        
        puts '--------------------'
        puts body
        puts '--------------------'
        
        if jr['error']
          # some lazy jsonrpc implementations just have a string here
          raise JSONRPCError, jr['error'].to_s if !jr['error'].is_a?(Hash)
    
          e = JSONRPCError.new(jr['error']['message'])
          e.code = jr['error']['code']
          e.error = jr['error']['error']
          raise e
        end
        
        return jr['result'] || "JSON parse error... (#{body})"
      end
    
      def method_missing(m, *a)
        if a.size == 0 && @prefix.empty?   # then a prefix thingy, only 1 level of chaining
          self.new(@uri, m.to_s)
        else
          if !@prefix.empty?
            m = @prefix +"."+ m.to_s
          end
          self.call(m, *a)
        end
      end
    end
    
    class JSONRPCError < StandardError
      # numeric code
      attr_accessor :code
    
      # arbitrary json object
      attr_accessor :error
    end
  end
end