require 'helper'

require 'fuzed_auto_config'

DEFAULT_BOOT_FILE = "/p/conf/fuzed/default.yml"

class TestCliBoot < Test::Unit::TestCase
  def test_default_config
    ARGV.clear
    ARGV << '' << File.join(File.dirname(__FILE__), *%w[fixtures boot default.yml])
    
    FUZEDAutoConfig.expects(:new).returns(stub(:is_master? => false,
                                             :master_hostname => 'aa0-foo',
                                             :master_nodename => 'master'))
    
    Object.any_instance.expects(:`).times(2)
    
    load 'cli/boot.rb'
  end
  
  def test_dir_config
    ARGV.clear
    ARGV << '' << File.join(File.dirname(__FILE__), *%w[fixtures boot configs])
    
    FUZEDAutoConfig.expects(:new).returns(stub(:is_master? => false,
                                             :master_hostname => 'aa0-foo',
                                             :master_nodename => 'master'))
    
    Object.any_instance.expects(:`).times(3)
    
    load 'cli/boot.rb'
  end
end