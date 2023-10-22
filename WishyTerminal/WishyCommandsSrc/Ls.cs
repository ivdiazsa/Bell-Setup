// ******************************************************************* //
//                      Ls Command Implementation!                     //
// ******************************************************************* //

using System;
using System.Collections.Generic;

internal sealed class Ls
{
    private static Ls _instance = null;

    public static Ls Instance
    {
        get
        {
            if (_instance is null)
                _instance = new Ls();

            return _instance;
        }
    }

    private CmdFlagCollection _lsFlags = new CmdFlagCollection(
        new[]
        {
            new CmdFlagInfo("-a", "--all", "Include files starting with '.'"),
            new CmdFlagInfo("-h", "--help", "Display the command's usage and exit."),
            new CmdFlagInfo("-l", "--long", "Display items in long format."),
            new CmdFlagInfo("-r", "--recursive", "List contents from all subdirectories"
                                               + " as well.")
        }
    );

    internal int ExecuteCommand(string[] args)
    {
        // TODO:
        // - Main 'ls' Functionality :)
        // - Handle current directory case.
        // - Handle nonexisting directories.
        // - Handle flags:
        //   * Let's begin with the '-l' flag.

        // The 'ls' command can receive one or more target files or directories
        // to display, hence we're using a list to store them.

        List<string> targets = new List<string>();
        List<string> flags = new List<string>();

        foreach (string arg in args)
        {
            // Not starting with '-' or '--' means it's a potential target.
            if (!arg.StartsWith('-'))
            {
                targets.Add(arg);
                continue;
            }

            if (!CmdFlagInfo.IsValidFlag(arg) || !_lsFlags.IsFlagDefined(arg))
            {
                Console.WriteLine($"Invalid ls flag '{arg}'");
                return WishyShell.SHELL_COMMAND_FAILURE;
            }

            flags.Add(arg);
        }

        Console.WriteLine($"You passed the following targets: {string.Join(',', targets)}");
        Console.WriteLine($"You passed the following flags: {string.Join(',', flags)}");
        return WishyShell.SHELL_COMMAND_SUCCESS;
    }
}
