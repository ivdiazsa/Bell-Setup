// ******************************************************************* //
//                           Rm Source Code!                           //
// ******************************************************************* //

using System;
using System.IO;
using System.Collections.Generic;

internal sealed class Rm
{
    // Helper class that contains all constant data related to the 'rm' command,
    // as well as managing the configuration of each specific call made.

    private static class RmConfiguration
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

        private RmSettings _config = RmSettings.None;

        public readonly CmdOptionCollection RmOptions = new CmdOptionCollection(
            new[]
            {
                new CmdOptionInfo("-f", "--force", "Don't fail with nonexistent targets"
                                                 + " and bypass deletion restrictions."),
    
                new CmdOptionInfo("-i", "--interactive", "Prompt the user before deleting"
                                                       + " each target."),
    
                new CmdOptionInfo("-r", "--recursive", "Delete directories and all of"
                                                     + " their contents recursively."),
    
                new CmdOptionInfo("-v", "--verbose", "Print a message for each item that"
                                                   + " is deleted."),
    
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

        public bool IsForce()       => _config & RmSettings.FORCE;
        public bool IsInteractive() => _config & RmSettings.INTERACTIVE;
        public bool IsRecursive()   => _config & RmSettings.RECURSIVE;
        public bool IsVerbose()     => _config & RmSettings.VERBOSE;
    }

    private string[] _rmArgs;

    public Rm(string[] args) { _rmArgs = args; }

    // FIXME: As of now, the '--' flag is accepted but results in undefined behavior.
    public int ExecuteCommand()
    {
        List<string> targetsToDelete = new List<string>();
        List<string> options = new List<string>();

        CmdUtils.ParseCommandArgs(_rmArgs, RmConfiguration.RmOptions, options, targetsToDelete);

        if (targetsToDelete.Length == 0)
        {
            Console.WriteLine("rm: Missing item(s) to delete.");
            return WishyShell.SHELL_COMMAND_FAILURE;
        }

        RmConfiguration.SetConfiguration(options);

        foreach (string item in targetsToDelete)
        {
            // If no wildcard and file exists, then delete it normally.
            // If no wildcard and directory exists, check for the presence of the '-r'
            // or '--recursive' flag. If not, then fail. If yes, delete it.
            // If wildcard, get all matching files and directories, and then delete them.

            if (item.ContainsAny('*', '?'))
            {
                // Handle wildcards here.
            }
            else if (Directory.Exists(item))
            {
                // We can't remove directories non-recursively, so we need to have
                // that flag set.

                if (!RmConfiguration.IsRecursive())
                {
                    Console.WriteLine("rm: Can't remove directory '{test}' without"
                                      + " the '-r/--recursive' flag.");
                    continue;
                }

                DeleteDirectory(item);
            }
            else if (File.Exists(item))
            {
                DeleteFile(item);
            }
            else
            {
                // We won't return an error because failing to delete one item
                // doesn't mean we can't try removing the others. Instead we let
                //the user know, and then we continue with the next item.

                Console.WriteLine($"rm: Could not remove '{item}' because it was"
                                  + " not found.");
            }
        }

        return WishyShell.SHELL_COMMAND_SUCCESS;
    }

    private void DeleteDirectory(string dirName)
    {
        return ;
    }

    private void DeleteFile(string fileName)
    {
        // We need to get the nod from the user if the interactive flag is set.
        // If we get another answer, then we tell the user to try again answering
        // with only "yes" or "no".

        if (RmConfiguration.IsInteractive())
        {
            string confirm = string.Empty;

            while (true)
            {
                Console.Write($"Do you wish to delete '{filename}' (yes/no)? ");
                confirm = Console.ReadLine();

                if (confirm == "n" || confirm == "no")
                    return ;

                if (confirm == "y" || confirm == "yes")
                    break;

                Console.WriteLine("Please answer with y(es) or n(o)!");
            }
        }

        // Delete the file. We could just catch the generic Exception, but we wanted
        // to add custom error messages for some potential errors.

        try { File.Delete(filename) }
        catch (IOException ioex)
        {
            Console.WriteLine($"rm: The OS decided to keep using the file '{filename}',"
                              + " so it could not be removed.");
            return ;
        }
        catch (UnauthorizedAccessException uaex)
        {
            Console.WriteLine("rm: Could not remove the file '{filename}' because"
                              + "of a lack of permissions.");
            return ;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"rm: Cold not remove the file '{filename}' because"
                              + $" an error occurred. Error details: {ex.Message}");
        }
        finally
        {
            if (RmConfiguration.IsVerbose())
                Console.WriteLine($"rm: Deleted file '{filename}' successfully!");
        }
    }
}
