using System;
using System.Text;

using WishyExtensions;

internal static partial class WishyConsole
{
    private static ConsoleKeyInfo _input;
    private static StringBuilder _commandSb = new StringBuilder();

    // Cursor position relative to the typing area (i.e. not counting the prompt text).
    private static int _cursorPosition = 0;

    // GENERAL TODO: Handle prompts with line breaks and/or multiple lines properly,
    // especially when handling the Backspace, and deleting characters in general.
    private static string _prompt = string.Empty;

    // Void Init():
    //
    // This method will be in charge of setting up the environment variables,
    // setting the prompt, and who knows what other neat features that require
    // initializing I will come up with in the future :)

    public static void Init()
    {
        _prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
        return ;
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
        _cursorPosition = 0;

        Console.Write(_prompt);

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

                case ConsoleKey.Backspace:
                    HandleBackspace();
                    break;

                case ConsoleKey.LeftArrow:
                case ConsoleKey.RightArrow:
                    HandleSideArrows(_input.Key);
                    break;

                // Any "normal" character, we assume is part of the command
                // currently being typed. We use Insert() instead of Append(),
                // since the cursor can be at different places, not just at
                // the end.
                default:
                    HandleCharacter(_input.KeyChar);
                    break;
            }
        }
        while (_input.Key != ConsoleKey.Enter);

        Console.Write("\n");
        return _commandSb.ToString();
    }

    // Void HandleCharacter():
    //
    // Add the given character to our currently received text at the current position
    // of the cursor.
    //
    // FIXME: If the cursor is anywhere but at the end of the line,
    //        the text will not be rendered correctly. It is captured
    //        correctly, however.

    private static void HandleCharacter(ConsoleKey charKey)
    {
        _commandSb.Insert(_cursorPosition, charKey);
        _cursorPosition++;
    }

    // Void HandleBackspace():
    //
    // TODO: Add method doc.
    //
    // FIXME: Backspace currently will always delete the last character in the
    //        command StringBuilder, rather than the one where the cursor is.

    private static void HandleBackspace()
    {
        // Delete the last input character from the command's StringBuilder instance,
        // if any. Otherwise, just return and await next command.
        if (_commandSb.Length <= 0)
            return ;

        _commandSb.DeleteLast();
        _cursorPosition--;

        RenderPrompt();
    }

    // Void HandleSideArrows():
    //
    // Handler for the left and right arrows. We have to move the cursor accordingly,
    // while making sure to stay within the bounds of the typing area.
    //
    // FIXME: Add check to ensure we're actually receiving a side arrow. Technically,
    //        it's impossible for that to not happen, but we can never be too safe.

    private static void HandleSideArrows(ConsoleKey arrowKey)
    {
        // Left Arrow means go back one character, and Right Arrow means go forward
        // one character.
        int motion = arrowKey == ConsoleKey.RightArrow ? 1 : -1;
        int targetPosition = _cursorPosition + motion;

        // If we would end up outside the typing area, then we simply don't
        // do anything.
        if (targetPosition >= 0 && targetPosition <= _commandSb.Length)
        {
            Console.SetCursorPosition(Console.CursorLeft + motion,
                                      Console.CursorTop);
            _cursorPosition = targetPosition;
        }
    }
}
