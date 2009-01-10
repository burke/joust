#!/usr/bin/env ruby

# TODO: Rewrite in elisp.

file = File.read(ARGV[0])

autoload_triggers = []
al=false
package=nil
file.each do |line|
  autoload_triggers << line if al
  al=false
  al=true if line =~ /;;;###autoload/
  package ||= (line.match(/\(provide ('.+)\)/)[1] rescue nil)
end

autoload_triggers.map!{ |el| el.split(/\s+/)[1] } # this is fragile; just do it better in elisp.

puts autoload_triggers.map!{ |el| "(autoload #{package} \"#{el}\" t nil)" }
