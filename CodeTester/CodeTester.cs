// Little app to easily run and test any C# methods without having to create
// multiple apps or constantly editing code :)

using System;
using System.Linq;
using System.Reflection;

// IDEAS:
// - Create a class to store all methods that can be chosen to be tested.
// - Create a helper class for all the console menu management.
// - Plan ideas for when we implement the source file adder.

public static class CodeTester
{
    static void Main(string[] args)
    {
        MethodInfo[] methods = GetTestableMethods();

        foreach (MethodInfo mi in methods)
            Console.WriteLine(mi.ToString());

        return ;
    }

    private static MethodInfo[] GetTestableMethods()
    {
        return typeof(TestablesCollection).GetMethods();
    }
}

// public static class CodeTester
// {
//     static int Main(string[] args)
//     {
//         BindingFlags methodSearchFlags = BindingFlags.Public
//                                        | BindingFlags.NonPublic
//                                        | BindingFlags.Static;

//         MethodInfo[] methods = typeof(CodeTester).GetMethods(methodSearchFlags)
//                                                  .Where(IsTestableMethod)
//                                                  .ToArray();

//         foreach (MethodInfo mi in methods)
//         {
//             Console.WriteLine("* {0}", mi.ToString());
//         }
//         Console.Write("\n");

//         foreach (MethodInfo mi in methods)
//         {
//             mi.Invoke(null, null);
//         }

//         return 0;
//     }

//     static int Main(string[] args)
//     {
//         string[] options = { "Option1", "Option2", "Exit" };
//         ConsoleKeyInfo input;
//         string optionPrefix = "  ";

//         int optionIndex = 0;
//         int selectedOption = 0;
//         bool exitSelected = false;

//         Console.CursorVisible = false;

//         // Console and menu management.
//         do
//         {
//             // Exit if the "Exit" option was selected.
//             if (exitSelected)
//                 break;

//             // Clear the console for the next render.
//             Console.Clear();

//             // Print all the options with the currently selected one colored.
//             for (optionIndex = 0; optionIndex < 3; optionIndex++)
//             {
//                 if (selectedOption == optionIndex)
//                 {
//                     Console.ForegroundColor = ConsoleColor.Red;
//                     optionPrefix = "* ";
//                 }
//                 else
//                 {
//                     optionPrefix = "  ";
//                 }

//                 Console.WriteLine("{0}{1}", optionPrefix, options[optionIndex]);
//                 Console.ResetColor();
//             }

//             input = Console.ReadKey(false);
//             switch (input.Key)
//             {
//                 case ConsoleKey.DownArrow:
//                     selectedOption = ++selectedOption % 3;
//                     break;

//                 case ConsoleKey.UpArrow:
//                     selectedOption = selectedOption > 0 ? --selectedOption
//                                                         : selectedOption = 3 - 1;
//                     break;

//                 case ConsoleKey.Enter:
//                     // Handle the selected option and act accordingly :)
//                     if (selectedOption == 0)
//                     {
//                         Console.WriteLine("\nYou selected Option 1!\n");
//                         Thread.Sleep(2000);
//                     }
//                     else if (selectedOption == 1)
//                     {
//                         Console.WriteLine("\nYou selected Option 2!\n");
//                         Thread.Sleep(2000);
//                     }
//                     else
//                     {
//                         exitSelected = true;
//                         Console.WriteLine("\nThanks for participating!\n");
//                         Thread.Sleep(2000);
//                     }
//                     break;

//                 default:
//                     Console.Beep();
//                     break;
//             }
//         }
//         while (input.Key != ConsoleKey.Escape);

//         Console.WriteLine("Thanks for stopping by!");
//         return 0;
//     }

//     private static bool IsTestableMethod(MethodInfo mi)
//     {
//         return mi.GetCustomAttributes(typeof(TestableAttribute), false).Length > 0;
//     }

//     [Testable]
//     static void Test1()
//     {
//         Console.WriteLine("Hello! I'm Test #1");
//         return ;
//     }

//     [Testable]
//     static void Test2()
//     {
//         Console.WriteLine("Hello! I'm Test #2");
//         return ;
//     }

//     [Testable]
//     static void Test3()
//     {
//         Console.WriteLine("Hello! I'm Test #3");
//         return ;
//     }
// }
