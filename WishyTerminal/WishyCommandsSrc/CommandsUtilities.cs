// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

// This little structure stores each flag's general information:
// * Short version: One dash and usually one letter
// * Long version: Two dashes and a full word or words sometimes.
// * Description: Sentence concisely explaining that the flag does.

// TODO: Implement DescribeFlags() method.

internal class CmdFlagInfo
{
    public string ShortVersion { get; init; }
    public string LongVersion { get; init; }
    public string Description { get; init; }

    public CmdFlagInfo(string oneDash, string twoDashes, string desc)
    {
        ShortVersion = oneDash;
        LongVersion = twoDashes;
        Description = desc;
    }

    public bool MatchesFlag(string flag)
    {
        return (flag == ShortVersion || flag == LongVersion);
    }

    // There are no commands that accept a single '-' alone or flags starting
    // with more than 2 dashes.
    public static bool IsValidFlag(string flag)
    {
        return flag.Length <= 1 || (flag.Length > 2 && flag[2] == '-')
               ? false
               : true;
    }
}

internal class CmdFlagCollection
{
    private CmdFlagInfo[] _flagsList { get; }

    public CmdFlagCollection(CmdFlagInfo[] flagsList)
    {
        _flagsList = flagsList;
    }

    public bool IsFlagDefined(string flag)
    {
        foreach (CmdFlagInfo fi in _flagsList)
        {
            if (fi.MatchesFlag(flag))
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

