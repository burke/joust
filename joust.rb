#!/usr/bin/env ruby

begin
  require 'rubygems'
rescue LoadError
end
require 'yaml'
require 'optparse'

JOUST_PATH = File.dirname(__FILE__)
META_PATH  = File.join(JOUST_PATH, "meta")
SYSTEM_META_PATH = File.join(META_PATH, "system")
USER_META_PATH = File.join(META_PATH, "user")
PACKAGE_PATH = File.join(JOUST_PATH, "packages")

class Joust

  def initialize(args=ARGV)
    load_opts(args)
  end

  def load_opts(args)
    opts = OptionParser.new do |opts|
      opts.banner = "Joust help menu:\n"
      opts.banner += "===============\n"
      opts.banner += "Usage #$0 [options]"

      opts.on('-l', '--listing [type]',
              "Get a listing of packages",
              "  Types: all, installed, outdated") do |type|
        listing(type.to_sym)
      end

      opts.on('-i', '--install [package]',
              "Install a given package and its dependencies") do |package|
        install(package)
      end

      opts.on('-r', '--uninstall [package]',
              "Uninstall a given package") do |package|
        uninstall(package)
      end

      opts.on("-f", "--[no-]force", "Force activities (will overwrite on install)") do |f|
        @force = f
      end

      opts.on_tail('-h', '--help', 'Display this help and exit') do
        puts opts
        exit
      end
    end

    opts.parse!(args)
  end

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
      if @force
        `rm -rf #{PACKAGE_PATH}/#{meta['name']}`
      end
      # Brute force and bad!
      if File.exists? "#{PACKAGE_PATH}/#{meta['name']}"
        puts "#{meta['name']} is already installed. Perhaps you want to update? "
        exit 1
      end
      print "Installing #{meta['name']}..."
      $stdout.flush
      `mkdir -p "#{PACKAGE_PATH}/#{meta['name']}"`
      `git clone "#{meta['url']}" "#{PACKAGE_PATH}/#{meta['name']}/#{meta['name']}"`

      meta.merge!({'version' => Time.now})
      File.open("#{PACKAGE_PATH}/#{meta['name']}/#{meta['name']}.yml","w") { |f| f.puts meta }

      `cp "#{meta_path}/#{meta['name']}.yml" "#{PACKAGE_PATH}/#{meta['name']}"`
      puts " Done."
    else
      puts "wtf."
    end

    # Append date installed to meta YAML
    # Write meta yaml to file in package folder
  end

  def uninstall(package)
    if File.exists?(File.join(USER_META_PATH, "#{package}.yml")) or
      File.exists?(File.join(SYSTEM_META_PATH, "#{package}.yml"))

      if !File.exists?(File.join(PACKAGE_PATH, package))
        puts "#{package} not installed."
        exit 1
      end
      print "Removing #{package}..."
      $stdout.flush
      `rm -rf #{PACKAGE_PATH}/#{package}`
      puts " Done."
    else
      puts "#{package} does not exist."
      exit 1
    end
  end

  def listing(type)
    # output listing with installed
    list = collect(type)
    printf "%-10s| %-10s| %-5s\n","Package","Installed", "Stale"
    puts   "=" * 32
    list.each_value do |package|
      printf "%-10s| %-10s| %-5s\n", package['name'],"NOW","Nope"
    end
  end

  # Collected installed packages by ls'ing the package pack for folders. Each
  def collect_installed
    installed = {}
    # Must find ruby equivalent
    `ls #{PACKAGE_PATH}`.each do |package| # Assumes sanitization of packages directory from junk
      package.strip! # silly \n
      installed[package] = YAML.load_file(File.join(PACKAGE_PATH,package,"#{package}.yml")) #FIXME
    end
    installed # return
  end

  def collect(type)
    if type == :all
      collect(:system).merge(collect(:user))
    else
      list = {}
      if type == :installed
        ls_path = File.join(PACKAGE_PATH)
        path_to_yaml = Proc.new do |package_name|
          File.join(ls_path,package_name,"#{package_name}.yml")
        end
      elsif type == :system
        ls_path = File.join(META_PATH,"system")
        path_to_yaml = Proc.new do |package_name|
          File.join(ls_path,"#{package_name}.yml")
        end
      elsif type == :user
        ls_path = File.join(META_PATH,"user")
        path_to_yaml = Proc.new do |package_name|
          File.join(ls_path,"#{package_name}.yml")
        end
      end

      `ls #{ls_path}`.each do |package| # Assumes sanitization of packages directory
        package.strip!.gsub!(".yml","") # silly \n
        list[package] = YAML.load_file(path_to_yaml[package]) #FIXME
      end
      list
    end
  end
end

if __FILE__ == $0
  j = Joust.new
end
