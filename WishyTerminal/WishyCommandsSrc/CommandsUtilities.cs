// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

// This little structure stores each flag's general information:
// * Short version: One dash and usually one letter
// * Long version: Two dashes and a full word or words sometimes.
// * Description: Sentence concisely explaining that the flag does.

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

    public bool MatchesOption(string opt)
    {
        return (opt == ShortVersion || opt == LongVersion);
    }

    // There are no commands that accept a single '-' alone or flags starting
    // with more than 2 dashes.
    public static bool IsValidOption(string opt)
    {
        return opt.Length <= 1 || (opt.Length > 2 && flag[2] == '-')
               ? false
               : true;
    }
}

internal class CmdOptionCollection
{
    private CmdOptionInfo[] _optsList { get; }

    public CmdOptionCollection(CmdOptionInfo[] options)
    {
        _optsList = options;
    }

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

