using System;
using System.IO;

internal static class WishyShell
{
    private const int SHELL_COMMAND_SUCCESS = 0;

    // Main method that will process the command read and sent by the console,
    // and do the necessary operations to run and execute it.

    public static int ExecuteCommand(string command)
    {
        int exitCode = SHELL_COMMAND_SUCCESS;

        // The item at [0] is the command and the items at [1...n] are the arguments
        // for said command.
        string[] commandAndArgs = command.Split();
        string cmd = commandAndArgs[0];
        string[] args = commandAndArgs[1..];

        switch (cmd)
        {
            case "pwd":
                exitCode = Pwd();
                break;
        }

        return exitCode;
    }

    private static int Pwd()
    {
        Console.WriteLine(Directory.GetCurrentDirectory());
        return SHELL_COMMAND_SUCCESS;
    }
}
