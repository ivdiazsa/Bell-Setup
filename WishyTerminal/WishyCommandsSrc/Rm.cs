// ******************************************************************* //
//                           Rm Source Code!                           //
// ******************************************************************* //

using System;
using System.Collections.Generic;

internal sealed class Rm
{
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

    private CmdOptionCollection _rmOptions = new CmdOptionCollection(
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

    public int ExecuteCommand(string[] rmArgs)
    {
        List<string> targetsToDelete = new List<string>();
        List<string> options = new List<string>();

        foreach (string arg in rmArgs)
        {
        }

        return WishyShell.SHELL_COMMAND_SUCCESS;
    }
}
