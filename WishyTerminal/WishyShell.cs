// ******************************************************************* //
//                              Wishy Shell!                           //
// ******************************************************************* //

using System;
using System.Collections.Generic;
using System.Text;

using WishyExtensions;

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
        return (string.Empty, Array.Empty<string>());
    }
}
