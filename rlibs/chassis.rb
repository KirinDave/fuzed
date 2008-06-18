require 'rubygems'
require 'optparse'
require 'erlectricity'
require 'json' unless $rails_json # prevent from clashing with Rails' built-in json library

class Chassis
  class << self
    attr_accessor :signatures, :node_kind, :pkgs, :tags, :roles, :extra_config, :exception_handler, :exit_after_current_dispatch
  end
  
  self.signatures = {}
  self.node_kind = nil
  self.pkgs = []
  self.tags = []
  self.roles = []
  self.extra_config = nil
  self.exception_handler = nil
  self.exit_after_current_dispatch = false
  
  # Define a handler method
  #   +method+ is the Symbol method name
  #   +names+ is a list of required parameters
  #
  # Returns nothing
  def self.handle(method, *names, &block)
    Chassis.signatures[method] = names.sort { |a, b| a.to_s <=> b.to_s }
    
    define_method("handle_proxy_#{method}", &block)
    
    define_method("handle_#{method}".to_sym) do |iargs|
      args = self.convert_args(iargs)
      self.verify_args(method, args)
      
      self.send("handle_proxy_#{method}", args)
    end
  end
  
  # Specify this handler's kind
  #   +name+ is a String identifier
  #
  # Returns nothing
  def self.kind(name)
    Chassis.node_kind = name
  end
  
  # Specify details that will be used for matching incoming requests
  #   +details+ is a Hash of primitives
  #
  # Returns nothing
  def self.details(details)
    raise("details() has already been called") unless Chassis.pkgs.empty?
    
    Chassis.pkgs = details.to_a
  end
  
  def self.config(&block)
    Chassis.extra_config = block.call
  end
  
  # Specify a block that takes an Exception as its single argument
  # to be called when an exception is not handled inside a handler
  #   +block+ is the block to execute
  #
  # Block should
  #   Raise -or-
  #   Return one of:
  #     [:result, <jsonified result>]
  #     [:error, <error string>]
  #
  # Returns nothing
  def self.handle_exception(&block)
    Chassis.exception_handler = block
  end
  
  # Limit version number to 9999 max
  #   +version+ is the verstion number to limit
  #
  # Returns Integer
  def self.limit_version(version)
    version.map { |x| x > 9999 ? 9999 : x }
  end
  
  # Convert Erlang-style args to Ruby-style args
  #   +iargs+ is the Erlang-style arg structure
  #
  # Returns Hash (converted args)
  def convert_args(iargs)
    args = HashWithIndifferentAccess.new
    iargs.each do |a|
      args[a[0]] = self.convert_args_node(a[1])
    end
    args
  end
  
  # Helper for +convert_args+. Recursively converts a node in
  # Erlang-style to a node in Ruby-style
  #  +node+ is the Erlang-style node
  #
  # Returns String|Symbol|Hash|Array
  # Raises if an invalid node is encountered
  def convert_args_node(node)
    if node.kind_of?(String) || node.kind_of?(Symbol) || node.kind_of?(Numeric) || 
       node.kind_of?(TrueClass) || node.kind_of?(FalseClass) || node.kind_of?(NilClass)
      node
    elsif node.instance_of?(Array)
      if node[0] == :struct
        node[1].inject(HashWithIndifferentAccess.new) do |acc, x|
          acc[x[0]] = convert_args_node(x[1]); acc
        end
      elsif node[0] == :array
        node[1].inject([]) do |acc, x|
          acc << convert_args_node(x)
        end
      else
        raise "Invalid tagged node: #{node.inspect}"
      end
    else
      raise "Invalid node, must be an instance of String, Symbol, Numeric, True, False, Nil, or Array: #{node.inspect} (#{node.class.inspect})"
    end
  end
  
  # Verify that the required args are met for the given method call
  #   +method+ is the Symbol representing the method name
  #   +args+ is the Ruby-style args to check
  #
  # Returns nothing
  # Raises on verification failure
  def verify_args(method, args)
    matches = Chassis.signatures[method].select do |key|
      args[key]
    end
    
    misses = Chassis.signatures[method] - matches
    
    unless misses.empty?
      raise "Required arguments missing for '#{method}': #{misses.join(", ")}"
    end
  end
  
  # Dispatch a method by its name
  #   +method+ is the Symbol representing the method name
  #   +retype+ is the response type Symbol (:json | :pure)
  #   +args+ is the Erlang-stle args for the call
  #
  # Returns one of:
  #   [:result, <jsonified result>]
  #   [:error, <error string>]
  def dispatch(method, retype, args)
    result = self.send("handle_#{method}".to_sym, args)
    result_key = Chassis.exit_after_current_dispatch ? :last_result : :result
    case retype
      when :json
        [result_key, [:raw, result.to_json]]
      when :pure
        [result_key, result]
      else
        raise "Unknown response type: #{retype}"
    end
  rescue Exception => e
    if e.instance_of?(SystemExit)
      exit
    elsif Chassis.exception_handler
      begin
        Chassis.exception_handler.call(e)
      rescue Exception => e2
        [:error, e2.message + "\n\n" + e2.backtrace.join("\n")]
      end
    else
      [:error, e.message + "\n\n" + e.backtrace.join("\n")]
    end
  end
  
  # The API structure of this Chassis, sorted alphabetically by method name
  #
  # Returns Array (Erlang-style nested structure)
  #   e.g.
  #   [[:meth1, []],
  #    [:meth2, [:arg1, :arg2]]]
  def api
    api = Chassis.signatures.to_a.sort { |a, b| a.first.to_s <=> b.first.to_s }
    api
  end
  
  # Construct the config (aka details)
  #
  # Returns Array (config)
  def config
    details = Chassis.pkgs.dup
    details << ["tags"] + Chassis.tags unless Chassis.tags.empty?
    details << ["roles"] + Chassis.roles unless Chassis.roles.empty?
    details << ["kind"] + [Chassis.node_kind] if Chassis.node_kind
    details << Chassis.extra_config if Chassis.extra_config
    details
  end
  
  # Start the Erlectricity recieve/respond loop
  #
  # Never returns
  def start
    receive(IO.new(3), IO.new(4)) do |f|
      f.when(:call, Array) do |args|
        method = args[0]
        retype = args[1]
        args = args[2..-1]
        f.send! self.dispatch(method, retype, args)
        exit if Chassis.exit_after_current_dispatch
        f.receive_loop
      end
      
      f.when(:config) do
        f.send! :result, self.config
        f.receive_loop
      end
      
      f.when(:api) do
        f.send! :result, self.api
        f.receive_loop
      end
      
      f.when(:ping) do
        f.send! :pong
        f.receive_loop
      end
      
      f.when(:quit) do
        exit(0)
      end
    end
  end
  
  def self.pull_cli_args
    Chassis.tags = []
    Chassis.roles = []
    
    # parse the options
    opts = OptionParser.new
    opts.on("--tags x,y,z", Array) do |val|
      Chassis.tags = val
    end
    opts.on("--roles x,y,z", Array) do |val|
      Chassis.roles = val
    end
    opts.parse(ARGV)
    
    # grab the extras
    extras = []
    dashdash = ARGV.index('--')
    if dashdash
      extras = ARGV[(dashdash + 1)..-1]
    end
    
    # ramrod the extras into ARGV
    ARGV.clear
    ARGV.concat(extras)
  end
  
  def self.start
    # get any handlers
    handlers = []
    ObjectSpace.each_object(Class) do |o|
      handlers << o if o < Chassis
    end
    
    if $!
      raise $!
    end
    
    # check that one and only one exist
    if handlers.size != 1
      raise "There must be exactly one class that extends Chassis, but there are #{handlers.size}"
    end
    
    # check that a kind has been set
    unless Chassis.node_kind
      raise "A kind must be specified"
    end
    
    h = handlers.first.new
    h.start
  end
  
  def return_and_exit(data)
    Chassis.exit_after_current_dispatch = true
    data
  end
end

class HashWithIndifferentAccess < Hash
  def initialize(constructor = {})
    if constructor.is_a?(Hash)
      super()
      update(constructor)
    else
      super(constructor)
    end
  end

  def default(key = nil)
    if key.is_a?(Symbol) && include?(key = key.to_s)
      self[key]
    else
      super
    end
  end

  alias_method :regular_writer, :[]= unless method_defined?(:regular_writer)
  alias_method :regular_update, :update unless method_defined?(:regular_update)

  def []=(key, value)
    regular_writer(convert_key(key), convert_value(value))
  end

  def update(other_hash)
    other_hash.each_pair { |key, value| regular_writer(convert_key(key), convert_value(value)) }
    self
  end

  alias_method :merge!, :update

  def key?(key)
    super(convert_key(key))
  end

  alias_method :include?, :key?
  alias_method :has_key?, :key?
  alias_method :member?, :key?

  def fetch(key, *extras)
    super(convert_key(key), *extras)
  end

  def values_at(*indices)
    indices.collect {|key| self[convert_key(key)]}
  end

  def dup
    HashWithIndifferentAccess.new(self)
  end

  def merge(hash)
    self.dup.update(hash)
  end

  def delete(key)
    super(convert_key(key))
  end

  def stringify_keys!; self end
  def symbolize_keys!; self end
  def to_options!; self end

  # Convert to a Hash with String keys.
  def to_hash
    Hash.new(default).merge(self)
  end

  protected
    def convert_key(key)
      key.kind_of?(Symbol) ? key.to_s : key
    end

    def convert_value(value)
      case value
      when Hash
        value.with_indifferent_access
      when Array
        value.collect { |e| e.is_a?(Hash) ? e.with_indifferent_access : e }
      else
        value
      end
    end
end

class Hash
  def with_indifferent_access
    hash = HashWithIndifferentAccess.new(self)
    hash.default = self.default
    hash
  end
end

at_exit do
  Chassis.start
end

Chassis.pull_cli_args

#############################################################################
# Tests

if $0 == __FILE__
  require 'test/unit'
  require 'mocha'
  
  class Chassis
    def self.start
      nil
    end
  end
  
  class Awesome < Chassis
    kind "awesome"
    
    details "failboat" => "sinking"
    
    config do
      ["grammar", "/tmp/awesome.lfg"]
    end
    
    handle(:alpha) do
      'alpha'
    end
    
    handle(:beta, :foo, :bar) do |args|
      helper(args[:foo], args[:bar])
    end
    
    handle(:gamma) do
      raise "once more unto the breach"
    end
    
    handle(:delta) do
      exit
    end
    
    handle(:epsilon) do
      return_and_exit('foo')
    end
    
    handle_exception do |e|
      raise "EBORKD"
    end
    
    def helper(x, y)
      "#{x}=#{y}"
    end
  end
  
  class TestChassis < Test::Unit::TestCase
    def setup
      @a = Awesome.new
    end
    
    # signatures
    
    def test_signatures_should_match_declaration
      sigs = {:gamma=>[], :alpha=>[], :delta=>[], :beta=>[:bar, :foo], :epsilon=>[]}
      assert_equal sigs, Chassis.signatures
    end
    
    # handler meta
    
    def test_handler_methods_should_exist
      assert @a.respond_to?(:handle_alpha)
      assert @a.respond_to?(:handle_beta)
    end
    
    def test_handler_methods_arity_should_always_be_one
      assert 1, @a.method(:handle_alpha).arity
      assert 1, @a.method(:handle_beta).arity
    end
    
    # convert_args
    
    def test_convert_args_should_convert_top_level_array_to_hash
      i = [[:foo, 'bar'], [:qux, 'quux']]
      o = {'foo' => 'bar', 'qux' => 'quux'}
      assert_equal o, @a.convert_args(i)
      
      i = [['foo', 'bar'], ['qux', 'quux']]
      o = {'foo' => 'bar', 'qux' => 'quux'}
      assert_equal o, @a.convert_args(i)
    end
    
    def test_convert_args_should_convert_structs_to_indifferent_hashes
      i = [[:foo, 'bar'], [:baz, [:struct, [[:qux, 'quux']]]]]
      o = {'foo' => 'bar', 'baz' => {'qux' => 'quux'}}
      assert_equal o, @a.convert_args(i)
      
      i = [[:foo, 'bar'], [:baz, [:struct, [[:qux, [:struct, [[:a, 'b']]]]]]]]
      o = {'foo' => 'bar', 'baz' => {'qux' => {'a' => 'b'}}}
      assert_equal o, @a.convert_args(i)
    end
    
    def test_convert_args_should_make_all_hashes_indifferent
      i = [[:foo, 'bar'], [:baz, [:struct, [[:qux, 'quux']]]]]
      # o = {'foo' => 'bar', 'baz' => {'qux' => 'quux'}}
      o = @a.convert_args(i)
      assert_equal 'bar', o[:foo]
      assert_equal 'bar', o['foo']
      
      assert_equal 'quux', o[:baz][:qux]
      assert_equal 'quux', o['baz']['qux']
    end
    
    def test_convert_args_should_convert_arrays_to_arrays
      i = [[:foo, 'bar'], [:baz, [:array, [:qux, 'quux']]]]
      o = {'foo' => 'bar', 'baz' => [:qux, 'quux']}
      assert_equal o, @a.convert_args(i)
      
      i = [[:foo, 'bar'], [:baz, [:array, [:qux, [:array, [:a, 'b']]]]]]
      o = {'foo' => 'bar', 'baz' => [:qux, [:a, 'b']]}
      assert_equal o, @a.convert_args(i)
    end
    
    # convert_args_node
    
    def test_convert_node_should_be_identity_for_strings
      assert_equal 'foo', @a.convert_args_node('foo')
    end
    
    def test_convert_node_should_be_identity_for_symbol
      assert_equal :foo, @a.convert_args_node(:foo)
    end
    
    def test_convert_node_should_be_identity_for_true
      assert_equal true, @a.convert_args_node(true)
    end
    
    def test_convert_node_should_be_identity_for_false
      assert_equal false, @a.convert_args_node(false)
    end
    
    def test_convert_node_should_be_identity_for_nil
      assert_equal nil, @a.convert_args_node(nil)
    end
    
    def test_convert_node_should_be_identity_for_numeric
      assert_equal 1, @a.convert_args_node(1)
      assert_equal 1.0, @a.convert_args_node(1.0)
      assert_equal 9999999999999999999, @a.convert_args_node(9999999999999999999)
    end
    
    def test_convert_node_should_convert_array
      assert_equal [1], @a.convert_args_node([:array, [1]])
      assert_equal [1, 2], @a.convert_args_node([:array, [1, 2]])
      assert_equal [1, 2, 3], @a.convert_args_node([:array, [1, 2, 3]])
    end
    
    def test_convert_node_should_convert_nested_array
      assert_equal [[1]], @a.convert_args_node([:array, [[:array, [1]]]])
    end
    
    def test_convert_node_should_convert_struct_to_hash
      i = [:struct, [[:foo, 'foo']]]
      o = {"foo" => 'foo'}
      assert_equal o, @a.convert_args_node(i)
    end
    
    # handlers
    
    def test_alpha_nominal
      assert_equal 'alpha', @a.handle_alpha([])
    end
    
    def test_beta_nominal
      assert_equal 'foo=bar', @a.handle_beta([[:foo, 'foo'], [:bar, 'bar']])
    end
    
    def test_beta_with_missing_required_arg_should_raise
      assert_raise RuntimeError do
        @a.handle_beta([[:foo, 'foo']])
      end
    end
    
    def test_epsilon_should_set_exit_after_current_dispatch
      assert_equal false, Chassis.exit_after_current_dispatch
      @a.dispatch(:epsilon, :json, [])
      assert_equal true, Chassis.exit_after_current_dispatch
    rescue SystemExit
      # prevent from exiting
    ensure
      Chassis.exit_after_current_dispatch = false
    end
    
    def test_epsilon_should_respond_with_last_result
      res = @a.dispatch(:epsilon, :json, [])
      assert_equal [:last_result, [:raw, "\"foo\""]], res
    rescue SystemExit
      # prevent from exiting
    ensure
      Chassis.exit_after_current_dispatch = false
    end
    
    # api
    
    def test_api
      assert_equal [[:alpha, []], [:beta, [:bar, :foo]], [:delta, []], [:epsilon, []], [:gamma, []]], @a.api
    end
    
    # kind
    
    def test_kind
      assert_equal "awesome", Chassis.node_kind
    end
    
    # packages
    
    def test_packages
      packages = [["failboat", "sinking"]]
      assert_equal packages, Chassis.pkgs
    end
    
    # handle_exception
    
    def test_handle_exception
      @a.dispatch(:gamma, :json, [])
    rescue => e
      assert_equal "EBORKD", e.message
    end
    
    # pull_cli_args
    
    def test_pull_cli_args
      ARGV.clear
      ARGV.concat(["--tags=foo,bar", "--", "--tags=baz,qux"])
      
      Chassis.pull_cli_args
      
      assert_equal ["foo", "bar"], Chassis.tags
      assert_equal ["--tags=baz,qux"], ARGV
    end
    
    # config
    
    def test_config
      ARGV.clear
      ARGV.concat(["--tags=foo,bar", "--roles=production", "--", "--tags=baz,qux"])
      Chassis.pull_cli_args
      
      config = [["failboat", "sinking"],
               ["tags", "foo", "bar"],
               ["roles", "production"],
               ["kind", "awesome"],
               ["grammar", "/tmp/awesome.lfg"]]
                
      assert_equal config, @a.config
    end
    
    # return_and_exit
    
    def test_return_and_exit
      assert_equal false, Chassis.exit_after_current_dispatch
      assert_equal 'foo', @a.return_and_exit('foo')
      Chassis.exit_after_current_dispatch = false
    end
  end
end