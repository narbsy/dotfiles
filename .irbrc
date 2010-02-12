require 'rubygems'
require 'boson'

# To use Boson commands in irb
# NOTE: FOR RAILS 3 YOU MUST HAVE BOSON IN YOUR GEMFILE
Boson.start :autoload_libraries => true

require 'sketches'
Sketches.config :editor => 'gvim'
