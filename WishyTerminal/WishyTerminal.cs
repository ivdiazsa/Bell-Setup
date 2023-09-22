using System;
using System.Text;

public class WishyTerminal
{
    static int Main(string[] args)
    {
        Console.WriteLine("\nWelcome to Wishy Terminal!\n");

        ConsoleKeyInfo input;
        string command = string.Empty;
        StringBuilder commandSb = new StringBuilder();

        // Main interactive loop of the shell!
        while (true)
        {
            commandSb.Clear();
            Console.Write("\nPrompt goes here::> ");

            do
            {
                input = Console.ReadKey();

                // IMPORTANT: Add the Backspace handler. Right now, the Backspace
                // Key prints a weird character.

                switch (input.Key)
                {
                    case ConsoleKey.Enter:
                        break;

                    default:
                        commandSb.Append(input.KeyChar);
                        break;
                }
            }
            while (input.Key != ConsoleKey.Enter);

            command = commandSb.ToString();
            Console.Write("\n");

            // Command processing goes here!
            Console.WriteLine("You typed the command {0}!", command);

            if (command == "exit")
                break;
        }

        Console.WriteLine("\nWishy Terminal wishes you a wonderful day!"
                          + " Thanks for your patronage!");
        return 0;
    }
}
