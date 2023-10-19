using System;

public class WishyTerminal
{
    static int Main(string[] args)
    {
        Console.WriteLine("\nWelcome to Wishy Terminal!\n");
        WishyConsole.Init();

        string command = string.Empty;
        int cmdExitCode = 0;

        // Main Interactive Loop!
        while (true)
        {
            // Command processing goes here!
            command = WishyConsole.ReadCommand();
            Console.WriteLine("DEV-NOTE: You typed the command {0}!", command);

            if (command == "exit")
                break;

            cmdExitCode = WishyShell.ExecuteCommand(command);
            Console.WriteLine("DEV-NOTE: " + cmdExitCode); // DEBUG-ONLY NOTE!
        }

        Console.WriteLine("\nWishy Terminal wishes you a wonderful day!"
                          + " Thanks for your patronage!");
        return 0;
    }
}
