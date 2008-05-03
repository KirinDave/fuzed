require 'open-uri'

require 'rubygems'

def root_binding
  binding
end

module Pontoon
  EXTENSIONS = ['', '.rb']
  
  class << self
    attr_accessor :paths
  end
  
  self.paths = []
  
  def self.add_path(path)
    self.paths << path
  end
  
  def self.remote_code_load(file)
    self.paths.each do |path|
      EXTENSIONS.each do |ext|
        begin
          location = File.join(path, file + ext)
          eval(open(location).read, root_binding, file)
          return
        rescue OpenURI::HTTPError
          # do nothing
        end
      end
    end
    raise LoadError.new("No local or remote code found for #{file}")
  end
end

alias :require_without_remote_loading :require

def require(file)
  begin
    require_without_remote_loading(file)
    STDERR.puts "\rLoaded [LOCAL] #{file}\n"
  rescue LoadError => e
      Pontoon.remote_code_load(file)
      STDERR.puts "\rLoaded [REMOTE] #{file}\n"
  end
end

if $0 == __FILE__
  file = ARGV.shift
  path = ARGV.shift
  Pontoon.add_path(path)
  require file
end