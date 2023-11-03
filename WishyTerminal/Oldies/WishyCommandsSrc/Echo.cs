// ******************************************************************* //
//                          Echo Source Code!                          //
// ******************************************************************* //

using System;
using System.Collections.Generic;

internal sealed class Echo : CommandTemplate
{
    // ****************************************************************************
    // Class EchoConfiguration:
    // Helper class that contains all constant data related to the 'echo' command,
    // as well as managing the configuration of each specific call made.
    // ****************************************************************************

    private static class EchoConfiguration
    {
        [Flags]
        private enum EchoSettings : short
        {
            NONE = 0,
            NO_NEWLINE = 1,
            NO_ESCAPING_SEQUENCES = 2
        };

        private static EchoSettings s_config = EchoSettings.NONE;

        public static readonly CmdOptionCollection EchoOptions = new CmdOptionCollection(
            new[]
            {
                new CmdOptionInfo("-n", "--no-newline", "Print the given stuff without
                                                      + " a newline character at the end."),

                new CmdOptionInfo("-e", "--no-escaping", "Print escaping sequences"
                                                      + " characters literally."),

                new CmdOptionInfo("-h", "--help", "Display the help and usage and exit.")
            }
        );

        // Method that translates the given flags and options into the configuration
        // enum format that the class can easily use to run.
        public static void SetConfiguration(List<string> options)
        {
            foreach (string flag in options)
            {
                switch (flag)
                {
                    case "-n":
                    case "--no-newline":
                        s_config |= EchoSettings.NO_NEWLINE;
                        break;

                    case "-e":
                    case "--no-escaping":
                        s_config |= EchoSettings.NO_ESCAPING_SEQUENCES;
                        break;

                    // We shouldn't get to this point since the command's class
                    // ParseCommandArgs() method has already validated all the
                    // given flags are acceptable by this command.
                    default:
                        break;
                }
            }
        }

        public static void Reset()
        {
            s_config = EchoSettings.NONE;
        }

        public static bool NoNewline() => (s_config & EchoSettings.NO_NEWLINE) != 0;

        public static bool NoEscaping() =>
            (s_config & EchoSettings.NO_ESCAPING_SEQUENCES) != 0;
    }

    // *******************************
    // Echo Main Class Implementation
    // *******************************

    public Echo(string[] echoArgs) : base(echoArgs, "echo") {}

    public override int ExecuteCommand()
    {
        List<string> targetsToPrint = new List<string>();
        List<string> options = new List<string>();

        if (!ParseCommandArgs(options, targetsToPrint))
            return WishyShell.SHELL_COMMAND_FAILURE;

        EchoConfiguration.SetConfiguration(options);

        foreach (string item in targetsToPrint)
        {
        }

        return WishyShell.SHELL_COMMAND_SUCCESS;
    }

    // FIXME: Help flag '-h/--help' is not implemented yet.
    private bool ParseCommandArgs(List<string> options, List<string> targets)
    {
        foreach (string item in _commandArgs)
        {
            // 'Echo' doesn't have any flags that receive values, so any argument
            // received that does not start with a '-', can be safely assumed to
            // be a potential target to print to the console.

            if (!item.StartsWith('-'))
            {
                targets.Add(item);
                continue;
            }

            // If there is an invalid option, then there is no point in continuing
            // to process the rest, since the command won't be able to run anyways.

            if (!CmdOptionInfo.IsValidOption(item)
                || !EchoConfiguration.EchoOptions.IsOptionDefined(item))
            {
                Console.WriteLine($"Invalid '{_commandName}' option '{item}'.");
                return false;
            }

            options.Add(item);
        }

        return true;
    }
}

