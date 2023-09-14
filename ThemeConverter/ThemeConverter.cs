// Little Tool to convert between Emacs, Vim, and VS Code themes.
using System;
using System.Text;

public class ThemeConverter
{
    private const string EMACS_THEME_COMMENT_HEADER = ";;; Theme Code:";

    static int Main(string[] args)
    {
        // Validate we actually receive a theme source file to convert.
        string srcThemeFile = args[0];

        // Validate we actually receive which editor to convert the theme file to.
        string destEditor = args[1];

        var outputThemeCode = new StringBuilder();

        // Use the corresponding header to the target editor provided.
        outputThemeCode.AppendLine(EMACS_THEME_COMMENT_HEADER);

        return 0;
    }
}
