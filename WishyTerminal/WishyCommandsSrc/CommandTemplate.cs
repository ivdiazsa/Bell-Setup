// ******************************************************************* //
//                       Other Command Utilities!                      //
// ******************************************************************* //

internal abstract class CommandTemplate
{
    private string[] _args;
    protected CommandTemplate(string[] args) { _args = args; }
    public abstract int ExecuteCommand();
}
