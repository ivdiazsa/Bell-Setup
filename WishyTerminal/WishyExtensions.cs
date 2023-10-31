// ******************************************************************* //
//                         Extension Methods!                          //
// ******************************************************************* //

namespace WishyExtensions;

public static class MethodExtensions
{
    public static bool ContainsAny(this string str, params char[] chars)
    {
        foreach (char c in str)
        {
            if (Array.IndexOf(chars, c) >= 0)
                return true;
        }
        return false;
    }
}
