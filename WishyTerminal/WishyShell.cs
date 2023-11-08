// ******************************************************************* //
//                              Wishy Shell!                           //
// ******************************************************************* //

using System;
using System.Collections.Generic;
using System.Text;

internal static class WishyShell
{
    // Int ExecuteCommand():
    //
    // Main method that will process the command read and sent by the console,
    // and do the necessary operations to run and execute it.

    public static int ExecuteCommand(string input)
    {
        // Here we parse the input and call the command that was called with it.
        return 0;
    }

    // (String, String[]) ParseCommandLineArgs():
    //
    // Method that parses the received input into the command and its arguments,
    // if any. The reason a simple string.Split() is not enough, is that arguments
    // can come with quotes, which makes spaces part of the argument, so we can't
    // split it. A bit more handling is required to process them right :)

    private static (string, string[]) ParseCommandLineArgs(string input)
    {
        // Clean up any accidental leading or trailing whitespace. We have to do this
        // step because spaces can have a special meaning in certain commands, so we
        // don't want to ignore them altogether.

        input = input.Trim();

        // The parameter list starts at the first space.
        int argsStart = input.IndexOf(' ');

        // If the string we receive has no spaces, then we can safely assume the
        // command was called with no arguments, so we just return it.

        if (argsStart < 0)
            return (input, Array.Empty<string>());

        string command = input.Substring(0, argsStart);
        List<string> parsedArgs = new List<string>();

        char quote = '\"';
        bool inQuotes = false;

        for (int i = argsStart; i < input.Length; i++)
        {
        }

        return (command, parsedArgs.ToArray());
    }
}
