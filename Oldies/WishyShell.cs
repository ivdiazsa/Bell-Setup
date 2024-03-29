// ******************************************************************* //
//                              Wishy Shell!                           //
// ******************************************************************* //

using System;
using System.Collections.Generic;
using System.IO;

internal static class WishyShell
{
    internal const int SHELL_COMMAND_SUCCESS = 0;
    internal const int SHELL_COMMAND_FAILURE = -1;

    // Int ExecuteCommand():
    //
    // Main method that will process the command read and sent by the console,
    // and do the necessary operations to run and execute it.

    public static int ExecuteCommand(string command)
    {
        int exitCode = SHELL_COMMAND_SUCCESS;

        // The item at [0] is the command and the items at [1...n] are the arguments
        // for said command. If more stuff is passed to a certain command that it
        // doesn't need, then all that extra stuff is simply not used.

        string[] commandAndArgs = command.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        string cmd = commandAndArgs[0];
        string[] args = commandAndArgs[1..];

        // The deal! Call the functionality of the received command!
        switch (cmd)
        {
            case "pwd":
            case "cwd":
                exitCode = _Pwd();
                break;

            case "cd":
                string target = args.Length > 0 ? args[0] : string.Empty;
                exitCode = _Cd(target);
                break;

            case "echo":
                exitCode = _Echo(args);
                break;

            case "rm":
                exitCode = _Rm(args);
                break;

            // If the given command does not match any of the Wishy Shell's ones,
            // or any in the PATH, then we'll assume it's attempting to call an
            // external program and try to run it.
            default:
                Console.WriteLine($"DEV-ONLY: {cmd}");
                Console.WriteLine("DEV-ONLY: Here goes running an external program :)");
                break;
        }

        return exitCode;
    }

    // Command     : pwd
    // Parameters  : None
    // Description : Print the current working directory.

    private static int _Pwd()
    {
        Console.WriteLine(Directory.GetCurrentDirectory());
        return SHELL_COMMAND_SUCCESS;
    }

    // Command     : cd
    // Parameters  : Target directory to switch to.
    // Description : Change the cwd to the specified one.

    private static int _Cd(string targetDir)
    {
        // The empty 'cd' command will behave like Unix: Change to the user's
        // home directory.
        if (string.IsNullOrWhiteSpace(targetDir))
            targetDir = Environment.GetEnvironmentVariable("HOME");

        if (!Directory.Exists(targetDir))
        {
            Console.WriteLine($"The specified directory '{targetDir}' does not exist.");
            return SHELL_COMMAND_FAILURE;
        }

        Directory.SetCurrentDirectory(targetDir);
        WishyConsole.UpdatePrompt();

        return SHELL_COMMAND_SUCCESS;
    }

    // Command     : echo
    // Parameters  : Text, variables, etc to display and flags
    // Description : Print the specified text to the console.

    private static int _Echo(string[] echoArgs)
    {
        // Under Construction!
        return SHELL_COMMAND_SUCCESS;
    }

    // Command     : rm
    // Parameters  : Files and/or directories to delete and flags.
    // Description : Delete the specified files and/or directories.

    private static int _Rm(string[] rmArgs)
    {
        return new Rm(rmArgs).ExecuteCommand();
    }
}

