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
    public readonly string ShortVersion { get; init; }
    public readonly string LongVersion { get; init; }
    public readonly string Description { get; init; }

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
    private readonly CmdFlagInfo[] _flagsList { get; }

    public CmdFlagCollection(CmdFlagInfo[] flagsList)
    {
        _flagsList = flagsList;
    }

    internal void DescribeFlags()
    {
        Console.WriteLine("DescribeFlags() is under construction!");
        return ;
    }
}

