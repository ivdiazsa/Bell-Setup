#!/usr/bin/ruby

# Little application to help me port color themes from VSCode's JSON Format
# to Emacs' Elisp format.

require 'json'

# Little application's helper functions and objects!
# This class generates the initial (defcustom) calls of the Elisp file.

module DefcustomMethod

  def self.get_defcustom_header(theme_name, custom_name)
    return "defcustom #{theme_name}-#{custom_name} t"
  end

  def self.get_defcustom_doc(custom_name)
    return "Set box around TODO items in org-mode." if custom_name == 'box-org-todo'
    return "Scale headlines and other org faces." if custom_name == 'scale-org-faces'
    return "Set (:invert-video t) on hl-todo face." if custom_name == 'invert-hl-todo'
    raise "get_defcustom_doc: Value #{custom_name} not yet implemented."
  end

  def self.get_defcustom_type(type)
    return ":type #{type}"
  end

  def self.get_defcustom_group(group)
    return ":group #{group}"
  end

end

# Class that serves as the main component of our little app :)

class ThemeConverter

  private

  # Files to work with and the name of the theme.

  attr_reader :elisp_file, :theme, :vscode_file

  # This little variable will be the holder of all the Elisp code we'll be generating
  # throughout this app. This is the little one that will get recorded into the final
  # resulting file (named in the variable "elisp_file" above).

  attr_accessor :elisp_translated_code

  # And this other little variable keeps track of how many spaces the current
  # indentation level is at.

  attr_accessor :indentation

  public

  def initialize(source, theme)
    @vscode_file = source
    @theme_name = theme

    @elisp_file = "#{@theme_name}-theme.el"
    @elisp_translated_code = ""
    @indentation = 0
  end

  # Main method where all the magic takes place :)

  def translate
    @elisp_translated_code << gen_deftheme()

    custom_header_names = ['box-org-todo', 'scale-org-faces', 'invert-hl-todo']

    vscode_theme_json = JSON.load(File.read(@vscode_file))
    theme_colors = vscode_theme_json['colors']
    token_colors = vscode_theme_json['tokenColors']

    # Generate the top (defcustom <code>) statements of the elisp file.
    custom_header_names.each do |name|
      @elisp_translated_code << gen_defcustom(name)
    end

    # Write down all our code to the new theme file!
    File.open(@elisp_file, 'w') do |ef|
      ef.write(@elisp_translated_code)
    end
  end

  private

  def gen_deftheme
    return "\n(deftheme #{@theme_name})\n"
  end

  def gen_defcustom(custom_name)
    code = "\n(#{DefcustomMethod.get_defcustom_header(@theme_name, custom_name)}\n"
    code <<  "  \"#{DefcustomMethod.get_defcustom_doc(custom_name)}\"\n"
    code <<  "  #{DefcustomMethod.get_defcustom_type("'boolean")}\n"

    # ENHANCEME: Generate a good group name. We're currently using "'dark-plus"
    #            as a placeholder only.
    code <<  "  #{DefcustomMethod.get_defcustom_group("'dark-plus")})\n"
    return code
  end
end

# Little application script starting point :)

# ENHANCEME: Validate we actually received at least one parameter value.
vscode_file = ARGV[0]
theme_name = ARGV.length > 1 ? ARGV[1] : "ported-from-vscode"

app = ThemeConverter.new(vscode_file, theme_name)
app.translate()
