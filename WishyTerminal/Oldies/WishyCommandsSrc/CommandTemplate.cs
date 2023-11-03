// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

internal abstract class CommandTemplate
{
    protected string[] _commandArgs;
    protected string _commandName;

    protected CommandTemplate(string[] args, string name)
    {
        _commandArgs = args;
        _commandName = name;
    }

    public abstract int ExecuteCommand();
}

