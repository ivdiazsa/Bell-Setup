// ******************************************************************* //
//                           Rm Source Code!                           //
// ******************************************************************* //

using System;
using System.IO;
using System.Collections.Generic;

using WishyExtensions;

internal sealed class Rm : CommandTemplate
{
    // ***************************************************************************
    // Class RmConfiguration:
    // Helper class that contains all constant data related to the 'rm' command,
    // as well as managing the configuration of each specific call made.
    // ***************************************************************************

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

        private static RmSettings s_config = RmSettings.NONE;

        public static readonly CmdOptionCollection RmOptions = new CmdOptionCollection(
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
                        s_config |= RmSettings.FORCE;
                        break;

                    case "-i":
                    case "--interactive":
                        s_config |= RmSettings.INTERACTIVE;
                        break;

                    case "-r":
                    case "--recursive":
                        s_config |= RmSettings.RECURSIVE;
                        break;

                    case "-v":
                    case "--verbose":
                        s_config |= RmSettings.VERBOSE;
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
            s_config = RmSettings.NONE;
        }

        public static bool IsForce()       => (s_config & RmSettings.FORCE) != 0;
        public static bool IsInteractive() => (s_config & RmSettings.INTERACTIVE) != 0;
        public static bool IsRecursive()   => (s_config & RmSettings.RECURSIVE) != 0;
        public static bool IsVerbose()     => (s_config & RmSettings.VERBOSE) != 0;
    }

    // *****************************
    // Rm Main Class Implementation
    // *****************************

    public Rm(string[] rmArgs) : base(rmArgs, "rm") {}

    // FIXME: As of now, the '--' flag is accepted but results in undefined behavior.
    public override int ExecuteCommand()
    {
        List<string> targetsToDelete = new List<string>();
        List<string> options = new List<string>();

        if (!ParseCommandArgs(options, targetsToDelete))
            return WishyShell.SHELL_COMMAND_FAILURE;

        if (targetsToDelete.Count == 0)
        {
            Console.WriteLine("rm: Missing item(s) to delete.");
            return WishyShell.SHELL_COMMAND_FAILURE;
        }

        RmConfiguration.SetConfiguration(options);

        foreach (string item in targetsToDelete)
        {
            // If no wildcard and file exists, then delete it normally.
            // If no wildcard and directory exists, check for the presence of
            // the '-r' or '--recursive' flag. If not, then fail. If yes, delete
            // it. If wildcard, get all matching files and directories, and then
            // delete them.

            // FIXME: Wildcards are not implemented yet, so using them will result
            // in no effect at best.
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

        RmConfiguration.Reset();
        return WishyShell.SHELL_COMMAND_SUCCESS;
    }

    // FIXME: Help flag '-h/--help' is not implemented yet.
    private bool ParseCommandArgs(List<string> options, List<string> targets)
    {
        foreach (string item in _commandArgs)
        {
            // 'Rm' doesn't have any flags that receive values, so any argument
            // received that does not start with a '-', can be safely assumed to
            // be a potential target to delete.

            if (!item.StartsWith('-'))
            {
                targets.Add(item);
                continue;
            }

            // If there is an invalid option, then there is no point in continuing
            // to process the rest, since the command won't be able to run anyways.

            if (!CmdOptionInfo.IsValidOption(item)
                || !RmConfiguration.RmOptions.IsOptionDefined(item))
            {
                Console.WriteLine($"Invalid '{_commandName}' option '{item}'.");
                return false;
            }

            options.Add(item);
        }

        return true;
    }

    private bool RequestItemDeletion(string itemName)
    {
        // We need to get the nod from the user if the interactive flag is set.
        // If we get another answer, then we tell the user to try again answering
        // with only "yes" or "no".

        string answer = string.Empty;

        while (true)
        {
            Console.Write($"Do you wish to delete '{itemName}' (yes/no)? ");
            answer = Console.ReadLine();

            if (answer != "n" && answer != "no" && answer != "y" && answer != "yes")
                Console.WriteLine("Please answer with y(es) or n(o)!");
            else
                break;
        }

        // We already validated in the loop that answer is "yes" or "no" only.
        return (answer == "y" || answer == "yes") ? true : false;
    }

    private void DeleteDirectory(string dirName)
    {
        if (RmConfiguration.IsInteractive() && !RequestItemDeletion(dirName))
            return ;

        // Main algorithm:
        // * Get all the directory's contents.
        // * Delete all the files.
        // * Recursively call itself for subdirectories.
        // * Delete the starting directory.

        IEnumerable<string> subDirs = Directory.EnumerateDirectories(dirName);
        IEnumerable<string> files = Directory.EnumerateFiles(dirName);

        foreach (string file in files)
        {
            DeleteFile(file);
        }

        foreach (string subDir in subDirs)
        {
            DeleteDirectory(subDir);
        }

        try { Directory.Delete(dirName); }
        catch (UnauthorizedAccessException)
        {
            Console.WriteLine($"rm: Could not remove the directory '{dirName}' because"
                              + "of a lack of permissions.");
            return ;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"rm: Cold not remove the directory '{dirName}' because"
                              + $" an error occurred. Error details: {ex.Message}");
            return ;
        }
        finally
        {
            if (RmConfiguration.IsVerbose())
                Console.WriteLine($"rm: Deleted directory '{dirName}' and its contents"
                                  + " and subdirectories successfully!");
        }
    }

    private void DeleteFile(string fileName)
    {
        if (RmConfiguration.IsInteractive() && !RequestItemDeletion(fileName))
            return ;

        // Delete the file. We could just catch the generic Exception, but we wanted
        // to add custom error messages for some potential errors.

        try { File.Delete(fileName); }
        catch (IOException)
        {
            Console.WriteLine($"rm: The OS decided to keep using the file '{fileName}',"
                              + " so it could not be removed.");
            return ;
        }
        catch (UnauthorizedAccessException)
        {
            Console.WriteLine($"rm: Could not remove the file '{fileName}' because"
                              + "of a lack of permissions.");
            return ;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"rm: Cold not remove the file '{fileName}' because"
                              + $" an error occurred. Error details: {ex.Message}");
            return ;
        }
        finally
        {
            if (RmConfiguration.IsVerbose())
                Console.WriteLine($"rm: Deleted file '{fileName}' successfully!");
        }
    }
}
