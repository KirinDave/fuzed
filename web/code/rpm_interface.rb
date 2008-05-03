module Powerset
  class PowerPack
    
    def self.installed?(name)
      is_installed?(rpm_query(name))
    end
    
    def self.check(name)
      self.new(name)
    end
    
    def self.check_packages(names)
      names.map { |x| self.installed?(x) ? self.check(x) : nil }
    end
    
    attr_reader :name
    attr_reader :version
    #attr_reader :arch
    
    # Inherently insecure.
    def initialize(name)
      result = PowerPack.rpm_query(name)
      if PowerPack.is_installed?(result)
        extract_data(result)
      else
        raise "Package is not installed. Cannot Instantiate"
      end
    end
        
    def dependencies
      result = PowerPack.rpm_dep_versions(@name)
      result.inject({}) do |injection, element| 
        e = element.split(' ')
        if e.size > 1
          injection[e[0]] = e[1] + " " + e[2]
        else
          injection[e[0]] = ">= 0.0.0"
        end
        injection
      end
    end
    
    def package_dependencies
      dependencies.keys.uniq
    end
    
    private
    def self.rpm_query(name)
      # Some security features would be nice....
      `rpm -q --qf "%{N} %{V} %{R}\n" #{name}`
    end
    
    def self.rpm_dep_versions(name)
      result = `rpm -qR #{name} | cut -d' ' -f 1 | uniq | xargs rpm -q --whatprovides --qf "%{N} %{V} %{R}\n"`.split("\n")
      result.reject { |x| x.match(/rpmlib/) }
    end
    
    
    def self.is_installed?(result)
      (result =~ /not installed$/) ? false : true # Dear Ruby Gods, Object#to_bool plz.
    end
    
    # New format: server_base 0.1.20 000054I
    def extract_data(result)
      @name, vstring, release = result.split(" ")
      @version = vstring.split(".").map {|x| x.to_i }[0..2] + [release.to_i]      
      tweak_version
    end
    
    def tweak_version
      return unless @version
      class << @version    
        def major ; self[0] ; end
        def minor ; self[1] ; end
        def trivial ; self[2] ; end
        def release ; self[3] ; end
        def to_s
          self[0..-2].join(".") + "-#{self[-1]}"
        end
      end
    end
  end
end
