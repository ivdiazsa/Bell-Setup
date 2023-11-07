/* ********************************************************************** */
/*                             Pwd Command!                               */
/* ********************************************************************** */

using System;
using System.IO;

internal class Pwd
{
    // Void Execute():
    //
    // This method contains the main functionality of the "pwd" command.

    public void Execute()
    {
        // Simple and straight to the point. Print the current working directory.
        Console.WriteLine(Directory.GetCurrentDirectory());
    }
}
