// Little app to easily run and test any C# methods without having to create
// multiple apps or constantly editing code :)

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

[AttributeUsage(AttributeTargets.Method)]
internal class TestableAttribute : Attribute { }

public static class CodeTester
{
    static int Main(string[] args)
    {
        BindingFlags methodSearchFlags = BindingFlags.Public
                                       | BindingFlags.NonPublic
                                       | BindingFlags.Static;

        MethodInfo[] methods = typeof(CodeTester).GetMethods(methodSearchFlags)
                                                 .Where(IsTestableMethod)
                                                 .ToArray();

        foreach (MethodInfo mi in methods)
        {
            Console.WriteLine("* {0}", mi.ToString());
        }
        Console.Write("\n");

        foreach (MethodInfo mi in methods)
        {
            mi.Invoke(null, null);
        }

        return 0;
    }

    private static bool IsTestableMethod(MethodInfo mi)
    {
        return mi.GetCustomAttributes(typeof(TestableAttribute), false).Length > 0;
    }

    [Testable]
    static void Test1()
    {
        Console.WriteLine("Hello! I'm Test #1");
        return ;
    }

    [Testable]
    static void Test2()
    {
        Console.WriteLine("Hello! I'm Test #2");
        return ;
    }

    [Testable]
    static void Test3()
    {
        Console.WriteLine("Hello! I'm Test #3");
        return ;
    }
}
