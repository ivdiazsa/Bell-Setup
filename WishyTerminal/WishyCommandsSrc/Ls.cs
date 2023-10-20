// ******************************************************************* //
//                         Ls Command Utilities!                       //
// ******************************************************************* //

internal sealed class Ls
{
    private static Ls _instance = null;

    public static Ls Instance
    {
        get
        {
            if (_instance is null)
                _instance = new Ls();

            return _instance;
        }
    }

    internal int ExecuteCommand()
    {
        return WishyShell.SHELL_COMMAND_SUCCESS;
    }
}
