// Utilities for the WishyConsole!

internal static partial class WishyConsole
{
    // Void UpdatePrompt():
    //
    // Commands like "Cd" can make the prompt need to be changed, so this function
    // will be in charge of that.

    public static void UpdatePrompt()
    {
        _prompt = $"\n[ CWD: {System.IO.Directory.GetCurrentDirectory()} ]::> ";
    }

    // Void RenderPrompt():
    //
    // Sometimes we need to redraw the entire prompt with whatever had been typed
    // there, like for example when the Backspace key is pressed, or we are writing
    // in the middle.

    public static void RenderPrompt()
    {
        int lineLength = _prompt.TrimStart().Length + _commandSb.Length + 1;

        // Clear the line entirely. Use the carriage return and fill it with blanks
        // up to the number of characters previously written.
        Console.Write("\r{0}", new string(' ', lineLength));

        // Use the carriage return to write over the newly blanked line. Write the
        // prompt, and the text with the last character removed.
        Console.Write("\r{0}{1}", _prompt.TrimStart(), _commandSb.ToString());
    }

    // Void AlternateHandleBackspace():
    //
    // Here's another implementation with Console.SetCursorPosition().

    private static void AlternateHandleBackspace(StringBuilder cmdSb)
    {
        cmdSb.DeleteLast();

        Console.SetCursorPosition(0, Console.CursorTop);
        Console.Write(new string(' ', _prompt.Length + cmdSb.Length + 1));
        Console.SetCursorPosition(0, Console.CursorTop);
        Console.Write("{0}{1}", _prompt.TrimStart(), cmdSb.ToString());
    }
}
