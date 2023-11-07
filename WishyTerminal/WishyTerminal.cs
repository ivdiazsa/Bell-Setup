// ******************************************************************* //
//                            Wishy Terminal!                          //
// ******************************************************************* //

using System;

public class WishyTerminal
{
    static int Main(string[] args)
    {
        Console.WriteLine("\nWelcome to Wishy Terminal!\n");
        WishyConsole.Init();

        string nextCommand = string.Empty;
        int lastCmdExitCode = 0;

        // Main interactive loop!
        while (true)
        {
            // Command processing goes here!
            nextCommand = WishyConsole.ReadCommand();
            Console.WriteLine("DEV-NOTE: You typed the command '{0}'!", nextCommand);

            if (nextCommand == "exit")
                break;

            WishyShell.ExecuteCommand(nextCommand);
            Console.WriteLine("DEV-NOTE: Exit Code: " + lastCmdExitCode);
        }

        Console.WriteLine("\nWishy Terminal wishes you a wonderful day!"
                          + " Thanks for your patronage!");
        return lastCmdExitCode;
    }
}
