require 'chassis'
require 'rubygems'
require 'erlectricity'
require 'optparse'
require 'stringio'


# speller handler
class FakeHandler < Chassis
  kind "calculon"

  handle(:add, :arg1, :arg2) do |args|
    args[:arg1] + args[:arg2]
  end

  handle(:crash) do |args|
    raise "You asked me to crash, so I did."
  end
end
