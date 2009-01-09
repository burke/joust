#!/usr/bin/env ruby

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
      if File.exists? "#{PACKAGE_PATH}/#{meta['name']}"
        puts "#{meta['name']} is already installed. Perhaps you want to update? "
        exit 1
      end
      print "Installing #{meta['name']}..."
      $stdout.flush
      `mkdir -p "#{PACKAGE_PATH}/#{meta['name']}"`
      `git clone "#{meta['url']}" "#{PACKAGE_PATH}/#{meta['name']}/#{meta['name']}"`
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

  def listing(package)
    # output listing with installed
    installed = collect_installed
    printf "%-10s| %-10s| %-5s\n","Package","Installed", "Stale"
    puts   "=" * 32
    installed.each_value do |package|
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
end

class Joust
  extend PackageCommands
end

if __FILE__ == $0
  command = ARGV[0]
  package = ARGV[1] || nil

  Joust.method(command.to_sym).call(package)
end
