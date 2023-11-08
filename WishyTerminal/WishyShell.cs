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
        (string command, string[] args) = ParseCommandLineArgs(input);

        Console.WriteLine("\nCOMMAND: {0}", command);
        Console.WriteLine("ARGS: {0}", string.Join('-', args));
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
        StringBuilder nextArgSb = new StringBuilder();

        // We start here with argsStart + 1 because that space is not part of the
        // parameters list, just the separator from the command.
        for (int i = argsStart+1; i < input.Length; i++)
        {
            char c = input[i];

            // Handling quoted parameters: If we find the closing quote, then we have
            // finished parsing this specific parameter. Add the contents of the
            // arg StringBuilder to the list of parsed args and clear it.
            if (inQuotes)
            {
                nextArgSb.Append(c);

                if (c == quote)
                {
                    parsedArgs.Add(nextArgSb.ToString());
                    nextArgSb.Clear();
                    inQuotes = false;
                }
                continue;
            }

            if (c == ' ')
            {
                // We found an extra space.
                if (nextArgSb.Length == 0)
                    continue;

                // A space in a non-quoted parameter is the delimiter. So, we add
                // the argument to the list and clear the StringBuilder.
                parsedArgs.Add(nextArgSb.ToString());
                nextArgSb.Clear();
            }
            else if (c == '\'' || c == '\"')
            {
                // Found an opening quote.
                quote = c;
                inQuotes = true;
                nextArgSb.Append(c);
            }
            else
            {
                // The most normal scenario we can encounter: Normal character in
                // an unquoted string.
                nextArgSb.Append(c);
            }
        }
        return (command, parsedArgs.ToArray());
    }
}
