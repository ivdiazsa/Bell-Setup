// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

internal abstract class CommandTemplate
{
    private string[] _commandArgs;
    private string _commandName;

    protected CommandTemplate(string[] args, string name)
    {
        _commandArgs = args;
        _commandName = name;
    }

    public abstract int ExecuteCommand();
}

