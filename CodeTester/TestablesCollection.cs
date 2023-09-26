// File: TestablesCollection.cs

using System;

[AttributeUsage(AttributeTargets.Method)]
internal class TestableAttribute : Attribute { }

public sealed class TestablesCollection
{
    [Testable]
    public void Test1()
    {
        Console.WriteLine("Hello! I'm Test #1");
        return ;
    }

    [Testable]
    public void Test2()
    {
        Console.WriteLine("Hello! I'm Test #2");
        return ;
    }

    [Testable]
    public void Test3()
    {
        Console.WriteLine("Hello! I'm Test #3");
        return ;
    }
}
