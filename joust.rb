begin
  require 'rubygems'
rescue LoadError
end
require 'yaml'

JOUST_PATH = File.dirname(__FILE__)
META_PATH  = File.join(JOUST_PATH, "meta")
SYSTEM_META_PATH = File.join(META_PATH, "system")
USER_META_PATH = File.join(META_PATH, "user")
PACKAGE_PATH = File.join(JOUST_PATH, "packages")

module PackageCommands
  def install(package)
    user_meta = File.join(USER_META_PATH, "#{package}.yml")
    system_meta = File.join(SYSTEM_META_PATH, "#{package}.yml")
    if File.exists?(user_meta)
      meta = YAML.load(File.read(user_meta))
    elsif File.exists?(system_meta)
      meta = YAML.load(File.read(system_meta))
    else
      puts "NOT FOUND!"
      exit 1
    end
    case meta["type"]
    when "git": 
      puts meta["url"]
    default:
      puts "wtf."
    end
  end

  def uninstall(package)
    puts "removing #{package}"
  end
end

class Joust
  extend PackageCommands
end

if __FILE__ == $0
  command = ARGV[0]
  package = ARGV[1]

  Joust.method(command.to_sym).call(package)
end
