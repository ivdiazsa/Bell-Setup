using System;
using System.Text;

using WishyExtensions;

internal static class WishyConsole
{
    private static ConsoleKeyInfo _input;
    private static StringBuilder _commandSb = new StringBuilder();

    // GENERAL TODO: Handle prompts with line breaks and/or multiple lines properly,
    // especially when handling the Backspace, and deleting characters in general.
    internal static string Prompt = string.Empty;

    // Void Init():
    //
    // This method will be in charge of setting up the environment variables,
    // setting the prompt, and who knows what other neat features that require
    // initializing I will come up with in the future :)

    public static void Init()
    {
        Prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
        return ;
    }

    // Void UpdatePrompt():
    //
    // Commands like "Cd" can make the prompt need to be changed, so this function
    // will be in charge of that.

    public static void UpdatePrompt()
    {
        Prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
    }

    // String ReadCommand():
    //
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

                // The Backspace key visibly only deletes the last input character.
                // However, I'm learning the hard way it takes much more processing
                // than I initially thought.
                case ConsoleKey.Backspace:
                    HandleBackspace(_commandSb);
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

    // Void HandleBackspace():
    //
    // TODO: Add method doc.
    // TODO: Add check to avoid going into a negative index in the command
    // string builder.

    private static void HandleBackspace(StringBuilder cmdSb)
    {
        // Delete the last input character from the command's StringBuilder instance.
        cmdSb.DeleteLast();

        // Now, here is the complicated stuff. We have to redraw the last line
        // of the console, but without the removed character this time.

        // Clear the line entirely. Use the carriage return and fill it with blanks
        // up to the number of characters previously written.
        Console.Write("\r{0}", new string(' ', Prompt.TrimStart().Length + cmdSb.Length + 1));

        // Use the carriage return to write over the newly blanked line. Write the
        // prompt, and the text with the last character removed.
        Console.Write("\r{0}{1}", Prompt.TrimStart(), cmdSb.ToString());
    }

    // Void AlternateHandleBackspace():
    //
    // Here's another implementation with Console.SetCursorPosition().

    private static void AlternateHandleBackspace(StringBuilder cmdSb)
    {
        cmdSb.DeleteLast();

        Console.SetCursorPosition(0, Console.CursorTop);
        Console.Write(new string(' ', Prompt.Length + cmdSb.Length + 1));
        Console.SetCursorPosition(0, Console.CursorTop);
        Console.Write("{0}{1}", Prompt.TrimStart(), cmdSb.ToString());
    }
}
