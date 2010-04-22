require "rubygems"
require "hpricot"

doc		= Hpricot(STDIN.read)
h1			= (doc/"h1")
classes	= {"spec passed"=>"+","spec failed"=>"-","spec not_implemented"=>"#"}

puts "* #{h1.inner_html}"

stats = (doc/"script").select {|script| script.innerHTML =~ /duration|totals/ }.map {|script| script.inner_html.scan(/".*"/).first.gsub(/<\/?strong>/,"") }
stats.each do |stat|
	puts "* #{stat.gsub(/\"/,'')}"
end
puts "* Parsed with Hpricot (http://wiki.github.com/why/hpricot)"
puts " "

(doc/"div[@class='example_group']").each do |example|
	puts "[#{(example/"dl/dt").inner_html}]"
	(example/"dd").each do |dd|
		txt = (dd/"span:first").inner_html
		puts "#{classes[dd[:class]]} #{txt}"
		next unless dd[:class] == "spec failed"
		failure = (dd/"div[@class='failure']")
    # this is typically one line
		msg		  = (failure/"div[@class='message']/pre").inner_html
    # as this is multiple lines, make sure all are available
		back		= (failure/"div[@class='backtrace']/pre").inner_html.lines.map { |line| "  " + line }.join
    # the last line doesn't have a \n at the end, it seems.
    ruby    = (failure/"pre[@class='ruby']/code").text.lines.map { |line| "  " + line.gsub(/^(\d+)\s*(.*?)/, '\1: \2') }.join "\n"
		puts "  #{msg}"
		puts back
    puts ruby
	end
	puts " "
end
