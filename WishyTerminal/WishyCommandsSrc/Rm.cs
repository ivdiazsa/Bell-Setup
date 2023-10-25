// ******************************************************************* //
//                           Rm Source Code!                           //
// ******************************************************************* //

using System;
using System.Collections.Generic;

internal sealed class Rm
{
    [Flags]
    private enum RmSettings : short
    {
        NONE = 0,
        FORCE = 1,
        INTERACTIVE = 2,
        RECURSIVE = 4,
        VERBOSE = 8
    };

    private static Rm s_instance = null;
    public static Rm Instance
    {
        get
        {
            if (s_instance is null)
                s_instance = new Rm();
            return s_instance;
        }
    }

    private RmSettings _config = RmSettings.None;

    private readonly CmdOptionCollection _rmOptions = new CmdOptionCollection(
        new[]
        {
            new CmdOptionInfo("-f", "--force", "Don't fail with nonexistent targets"
                                             + " and bypass deletion restrictions."),

            new CmdOptionInfo("-i", "--interactive", "Prompt the user before deleting"
                                                   + " each target."),

            new CmdOptionInfo("-r", "--recursive", "Delete directories and all of"
                                                 + " their contents recursively."),

            new CmdOptionInfo("-v", "--verbose", "Print a message for each item that"
                                               + " is deleted."

            new CmdOptionInfo("-h", "--help", "Display the help and usage and exit."
        }
    );

    // FIXME: As of now, the '--' flag is accepted but results in undefined behavior.
    public int ExecuteCommand(string[] rmArgs)
    {
        List<string> targetsToDelete = new List<string>();
        List<string> options = new List<string>();

        CmdUtils.ParseCommandArgs(rmArgs, _rmOptions, options, targetsToDelete);
        SetConfiguration(options);

        return WishyShell.SHELL_COMMAND_SUCCESS;
    }

    // Method that translates the flags and options into a configuration enum format,
    // that the class can use to run its functionality and do its job.
    private void SetConfiguration(List<string> options)
    {
        foreach (string flag in options)
        {
            switch (flag)
            {
                case "-f":
                case "--force":
                    _config |= RmSettings.FORCE;
                    break;

                case "-i":
                case "--interactive":
                    _config |= RmSettings.INTERACTIVE;
                    break;

                case "-r":
                case "--recursive":
                    _config |= RmSettings.RECURSIVE;
                    break;

                case "-v":
                case "--verbose":
                    _config |= RmSettings.VERBOSE;
                    break;

                default:
                    break;
            }
        }
    }
}
