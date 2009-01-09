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
      meta_path = USER_META_PATH
      meta = YAML.load_file(user_meta)
    elsif File.exists?(system_meta)
      meta_path = SYSTEM_META_PATH
      meta = YAML.load_file(system_meta)
    else
      puts "NOT FOUND!"
      exit 1
    end
    case meta["type"]
    when "git": 
    	# Brute force and bad!
      `mkdir -p "#{PACKAGE_PATH}/#{meta['name']}"`
      # Todo: Do something sensible if file exists. Git gives a fatal error otherwise.
    	`git clone "#{meta['url']}" "#{PACKAGE_PATH}/#{meta['name']}/#{meta['name']}"`
  		`cp "#{meta_path}/#{meta['name']}.yml" "#{PACKAGE_PATH}/#{meta['name']}"`
    else
      puts "wtf."
    end
  end

  def uninstall(package)
    puts "removing #{package}"
  end
  
  def listing
    # output alled 
		installed = collect_installed
		
  endlist ing with inst
  led
     d = []]
  eninstalledrpr{}er ruby equivalen
    # Must find t
    `ls #{PACKAGE_PATH}`.each do |pac_file()mes sanitization of packages directory from junk
    	installed['package'] = YAML.load(File.join(PACKAGE_PATH,package,"#{package}.yml")r #FIXME	
    end
    installed # return
  def collect_instal
end

class Joust
  extend PackageCommands
end

if __FILE__ == $0
  command = ARGV[0]
  package = ARGV[1]

  Joust.method(command.to_sym).call(package)
end
