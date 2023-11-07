// ******************************************************************* //
//                           Wishy Extensions!                         //
// ******************************************************************* //

using System;

namespace WishyExtensions;

public static class WishyExtensions
{
    public static bool StartsQuoted(this string str)
    {
        return str.StartsWith('\'') || str.StartsWith('\"');
    }
}
