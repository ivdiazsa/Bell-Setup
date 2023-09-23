using System;
using System.Text;

internal static class WishyConsole
{
    private static ConsoleKeyInfo _input;
    private static StringBuilder _commandSb = new StringBuilder();

    internal static string Prompt = string.Empty;

    public static void Init()
    {
        // This method will be in charge of setting up the environment variables,
        // setting the prompt, and who knows what other neat features that require
        // initializing I will come up with in the future :)

        Prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
        return ;
    }

    // Commands like "Cd" can make the prompt need to be changed, so this function
    // will be in charge of that.

    public static void UpdatePrompt()
    {
        Prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
    }

    // Read the user's input on a char-by-char basis. The reason for this instead
    // of an entire line, is to be able to later on handle certain autocompletion
    // shortcuts, such as Tab and Ctrl+D in Unix for example.

    public static string ReadCommand()
    {
        // Clean the string builder from the previous command.
        _commandSb.Clear();
        Console.Write(Prompt);

        do
        {
            // Main loop where we read the user's entered keys, and process them
            // accordingly, before returning the full command to the terminal.
            _input = Console.ReadKey();

            switch (_input.Key)
            {
                // We don't want the Enter key to add its character to the command
                // so, we have to explicitly tell it to exit the switch case.
                case ConsoleKey.Enter:
                    break;

                // Any "normal" character, we assume is part of the command
                // currently being typed.
                default:
                    _commandSb.Append(_input.KeyChar);
                    break;
            }
        }
        while (_input.Key != ConsoleKey.Enter);

        Console.Write("\n");
        return _commandSb.ToString();
    }
}
