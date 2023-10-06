// ******************************************************************* //
//                             Wishy Console!                          //
// ******************************************************************* //

using System;
using System.Text;

internal static partial class WishyConsole
{
    private static ConsoleKeyInfo _input;
    private static StringBuilder _commandSb = new StringBuilder();

    // Cursor position relative to the typing area (i.e. not counting the prompt text).
    private static int _cursorPosition = 0;

    // FEATURE FOR LATER: Handle complex prompts appropriately when redrawing the
    // current line. Currently, if a prompt will have newlines, they must be at the
    // very beginning of the prompt. Otherwise, the terminal will glitch out when
    // using Backspace and/or typing characters anywhere but the current end of the
    // text area.

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
                // currently being typed.
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

    private static void HandleCharacter(char charKeyVal)
    {
        _commandSb.Insert(_cursorPosition, charKeyVal);
        _cursorPosition++;

        // If we typed somewhere in the middle of the already existing text, we
        // have to render the line again, or it will be missing characters.
        if (_cursorPosition < _commandSb.Length)
        {
            int currentConsoleCursorPos = Console.CursorLeft;
            RenderPrompt();
            Console.SetCursorPosition(currentConsoleCursorPos, Console.CursorTop);
        }
    }

    // Void HandleBackspace():
    //
    // Delete the character behind the cursor. Also, redraw and adjust the cursor's
    // position as needed.

    private static void HandleBackspace()
    {
        // If we haven't typed anything, or are at the very beginning of the typing
        // area, then there's nothing to delete, so we just return.
        if (_cursorPosition <= 0)
            return ;

        // Backspace deletes the character behind the cursor, hence we are calling
        // here Remove() with a -1 value.
        _commandSb.Remove(_cursorPosition - 1, 1);
        _cursorPosition--;

        // If we deleted the last character, then the cursor is already where it
        // ought to be. If we deleted in the middle, we have to make further
        // adjustments, like in HandleCharacter().
        if (_cursorPosition == _commandSb.Length)
        {
            RenderPrompt();
            return ;
        }
        
        int currentConsoleCursorPos = Console.CursorLeft;
        RenderPrompt();
        Console.SetCursorPosition(currentConsoleCursorPos - 1, Console.CursorTop);
    }

    // Void HandleSideArrows():
    //
    // Handler for the left and right arrows. We have to move the cursor accordingly,
    // while making sure to stay within the bounds of the typing area.
    //
    // ENHANCEME: Add check to ensure we're actually receiving a side arrow.
    //            Technically, it's impossible for that to not happen, but we
    //            can never be too safe.

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
