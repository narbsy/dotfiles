require 'rake'

# Inspired by: http://github.com/ryanb/dotfiles/blob/master/Rakefile which
# takes a file or an erb template (which it renders) and links it to the home directory.
# 
# This script links the files recursively.
# 

desc "install the dot files into user's home directory"
task :install do
  replace_all = false
  Dir['**/.*'].each do |file|
    next if %w[Rakefile .git . ..].include? file

    if File.directory? file
      # Make the folder tree, then copy the files
      FileUtils.mkdir_p(file)

      Dir["#{file}/**/*"].each do |sub_file|
        replace_all = place_file replace_all, sub_file
      end
    else # it's probably a file, so just place it.
      replace_all = place_file replace_all, file
    end
  end
end

def place_file(replace_all, file)
  current_dotfile = File.join(ENV['HOME'], file)
  if File.exist? current_dotfile
    if File.identical? file, current_dotfile
      puts "identical ~/#{file}"
    elsif replace_all
      replace_file(file)
    else
      print "overwrite ~/#{file}? [ynaq] "
      case $stdin.gets.chomp
      when 'a'
        replace_all = true
        replace_file(file)
      when 'y'
        replace_file(file)
      when 'q'
        exit
      else
        puts "skipping ~/#{file}"
      end
    end
  else
    link_file(file)
  end
  replace_all
end

def replace_file(file)
  system %Q{rm "$HOME/#{file}"}
  link_file(file)
end

def link_file(file)
  puts "linking ~/#{file}"
  system %Q{ln -s "$PWD/#{file}" "$HOME/#{file}"}
end


