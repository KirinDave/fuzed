require 'chassis'
require 'rubygems'
require 'erlectricity'
require 'stringio'

class CalcNode < Chassis
  kind "calc"

  handle(:add, :a, :b) do |args|
    args[:a] + args[:b]
  end
end
