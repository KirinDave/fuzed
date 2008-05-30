require 'chassis'
require 'rubygems'
require 'erlectricity'
require 'optparse'


# speller handler
class FakeHandler < Chassis
  kind "calculon"

  handle(:add, :arg1, :arg2) do |args|
    arg1 = args[:arg1] or 0
    arg2 = args[:arg2] or 0
    arg1 + arg2
  end
  
  handle(:crash) do |args|
    raise "You asked me to crash, so I did."
  end
end
