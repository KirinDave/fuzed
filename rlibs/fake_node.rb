require 'chassis'
require 'rubygems'
require 'erlectricity'
require 'optparse'


# speller handler
class FakeHandler < Chassis
  kind "fake"

  handle(:echo, :textsy, :options) do |args|
    "You said: " + args[:textsy] + ' ' + args[:options].inspect
  end

  handle(:killer) do |args|
    exit
  end

  handle(:woot) do |args|
    "Boots are delightful."
  end

  handle(:mangler) do |args|
    return_and_exit("Let slip the dogs of war")
  end

  handle(:failboat) do |args|
    raise "This boat failed under an endless sky."
  end
end
