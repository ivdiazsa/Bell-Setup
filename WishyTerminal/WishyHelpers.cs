using System.Text;

namespace WishyExtensions;

public static class Extensions
{
    // StringBuilder DeleteLast():
    //
    // Nice method to help with the handling of Backspaces.

    public static StringBuilder DeleteLast(this StringBuilder sb)
    {
        return sb.Remove(sb.Length - 1, 1);
    }
}
