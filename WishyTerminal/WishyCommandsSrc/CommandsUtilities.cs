// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

using System;
using System.Collections.Generic;

// *******************************************************************
// This little structure stores each flag's general information:
// * Short version: One dash and usually one letter
// * Long version: Two dashes and a full word or words sometimes.
// * Description: Sentence concisely explaining that the flag does.
// *******************************************************************

internal class CmdOptionInfo
{
    public string ShortVersion { get; init; }
    public string LongVersion { get; init; }
    public string Description { get; init; }

    public CmdOptionInfo(string oneDash, string twoDashes, string desc)
    {
        ShortVersion = oneDash;
        LongVersion = twoDashes;
        Description = desc;
    }

    // Check if the given string is part of this flag/option instance.
    public bool MatchesOption(string opt)
    {
        return (opt == ShortVersion || opt == LongVersion);
    }

    // There are no commands that accept a single '-' alone or flags starting
    // with more than 2 dashes.
    public static bool IsValidOption(string opt)
    {
        return opt.Length <= 1 || (opt.Length > 2 && opt[2] == '-')
               ? false
               : true;
    }
}

// *****************************************************************************
// Small wrapper class to be able to do operations on the entire set of flags
// and options any given command might have.
// *****************************************************************************

internal class CmdOptionCollection
{
    private CmdOptionInfo[] _optsList { get; }

    public CmdOptionCollection(CmdOptionInfo[] options)
    {
        _optsList = options;
    }

    // Check if the given string is any of the defined supported flags/options.
    public bool IsOptionDefined(string opt)
    {
        foreach (CmdOptionInfo oi in _optsList)
        {
            if (oi.MatchesOption(opt))
                return true;
        }
        return false;
    }

    // public override string ToString()
    // {
    //     Console.WriteLine("DescribeFlags() is under construction!");
    //     return ;
    // }
}

// *****************************************************************************
// Additional utilities class for commands, flags, and options processing :)
// *****************************************************************************

internal static class CmdUtils
{
    // Helper method to parse the arguments any command might receive.
    public static void ParseCommandArgs(string[] args,
                                        CmdOptionCollection supportedOpts,
                                        List<string> options,
                                        List<string> targets = null)
    {
        foreach (string item in args)
        {
            // In theory, we should never receive targets in commands that don't
            // use them, but we have to be safe from misinputs and wrong command lines.

            if (targets is not null && !item.StartsWith('-'))
            {
                targets.Add(item);
                continue;
            }

            // If there is an invalid option, then there is no point in continuing
            // to process the rest, since the command won't be able to run anyways.

            if (!CmdOptionInfo.IsValidOption(item) || !supportedOpts.IsOptionDefined(item))
            {
                Console.WriteLine($"Invalid rm option '{item}'.");
                return WishyShell.SHELL_COMMAND_FAILURE;
            }

            options.Add(item);
        }
    }
}

