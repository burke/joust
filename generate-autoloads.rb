#!/usr/bin/env ruby

# TODO: Rewrite in elisp.

file = File.read(ARGV[0])

autoload_triggers = []
al=false
package=nil
file.each do |line|
  autoload_triggers << (line.match(/\(defun ([^\s]+) \(/)[1]) if al
  al=false
  al=true if line =~ /;;;###autoload/
  package ||= (line.match(/\(provide ('.+)\)/)[1] rescue nil)
end

puts autoload_triggers.map{ |el| "(autoload #{package} \"#{el}\" t nil)" }
