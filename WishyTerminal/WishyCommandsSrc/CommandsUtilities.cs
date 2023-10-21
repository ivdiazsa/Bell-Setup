// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

// This little structure stores each flag's general information:
// * Short version: One dash and usually one letter
// * Long version: Two dashes and a full word or words sometimes.
// * Description: Sentence concisely explaining that the flag does.

// TODO: Implement DescribeFlags() method.

internal readonly struct CmdFlagInfo
{
    public CmdFlagInfo(string oneDash, string twoDashes, string desc)
    {
        ShortVersion = oneDash;
        LongVersion = twoDashes;
        Description = desc;
    }

    string ShortVersion { get; init; }
    string LongVersion { get; init; }
    string Description { get; init; }
}

internal class CmdFlagCollection
{
    private readonly CmdFlagInfo[] _flagsList { get; init; }

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
