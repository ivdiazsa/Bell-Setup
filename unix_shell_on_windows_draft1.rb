# Little script to use Windows cmd as if it was Bash on Linux.

# Bash's "ls" is mapped to Windows' "dir".
def bash_ls(*args)
  system('dir', *args)
end

# Bash's "cd" is the same as Windows', except it goes to the root folder when
# called without a parameter.
def bash_cd(*args)
  if args.empty? then
    Dir.chdir('\\')
  else
    Dir.chdir(args[0])
  end
end

# Main loop of our shell!
def main
  loop do
    print("\n[ Bash on Windows @ #{Dir.getwd} ]:>> ")
    command, *args = STDIN.gets.chomp.split(' ')

    case command
    when "ls"
      bash_ls(*args)
    when "cd"
      bash_cd(*args)
    when "exit"
      break
    else
      puts("Command '#{command}' not implemented yet...")
    end
  end

  puts("\nExiting...\n");
end

main()

