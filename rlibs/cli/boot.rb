require 'yaml'
require 'id2_auto_config'


def finish_spec(yml_spec, autoconf)
  raise "Your spec must define a kind!" unless yml_spec["kind"]
  raise "Your spec must define a name!" unless yml_spec["name"]
  yml_spec["clone"] ||= 1
  yml_spec["master_host"] ||= autoconf.master_hostname
  yml_spec["master_node"] ||= autoconf.master_nodename
  yml_spec["heartbeat"] = false if yml_spec["heartbeat"].nil?
  
  return yml_spec
end

def client_spec_2_cmdline(yml_spec, autoconf)
  yml_spec = finish_spec(yml_spec, autoconf)
  cmd_set = ["id2 join -d"]
  cmd_set << "-x" if yml_spec["xlew"]
  cmd_set << "-n '" + yml_spec["name"] + "'"
  cmd_set << "-f " + yml_spec["kind"]
  cmd_set << "-t '" + [yml_spec["tags"]].flatten.join(",") + "'" if yml_spec["tags"]
  cmd_set << "-r '" + [yml_spec["roles"]].flatten.join(",") + "'" if yml_spec["roles"]
  cmd_set << "-z " + yml_spec["master_host"]
  cmd_set << "-c "  + yml_spec["clone"].to_s if yml_spec["clone"]
  cmd_set << "-s "  + "'" + yml_spec["spec"] + "'" if yml_spec["spec"]
  cmd_set << "-h" if yml_spec["heartbeat"]
  
  return cmd_set.join(" ")
end

def read_directory_configs(path)
  names = []
  configs = []
  
  Dir.new(path).each do |f|
    if f =~ /\.yml$/i
      YAML::load(File.read(File.join(path, f))).each do |conf|
        name = conf["name"]
        unless name && names.include?(name)
          configs << conf
          names << name
        end
      end
    end
  end
  
  configs
end

def read_configs(path)
  if File.directory?(path)
    read_directory_configs(path)
  else
    YAML::load(File.read(path))
  end
end

id2_bootpath = ARGV[1] || DEFAULT_BOOT_DIR
environment = ID2AutoConfig.new

begin
  commands = []
  if environment.is_master?
    commands << "id2 start -d -n #{environment.master_nodename}"
  end
  configs = read_configs(id2_bootpath)
  commands += configs.map {|x| client_spec_2_cmdline(x, environment)}
  commands.each {|x| puts x ; `#{x}` ; sleep(1) }
rescue 
  puts "Failed to boot ID2 during configuration stage.\nError:#{$!}"
  puts $!.backtrace.join("\n")
  exit(1)
end

