#!/usr/bin/ruby

require 'json'

# Little application's helper functions and objects!

class DefcustomGenerator

  public
  attr_reader :theme

  def initialize(theme_name)
    @theme = theme_name
  end

  private

  def get_defcustom_header(custom_name)
    return "defcustom #{@theme}-#{custom_name} t"
  end

  def get_defcustom_doc(custom_name)
    return "Set box around TODO items in org-mode." if custom_name == 'box-org-todo'
    return "Scale headlines and other org faces." if custom_name == 'scale-org-faces'
    return "Set (:invert-video t) on hl-todo face." if custom_name == 'invert-hl-todo'
    raise "get_defcustom_doc: Value #{custom_name} not yet implemented."
  end

end

# Little application to help me port color themes from VSCode's JSON Format
# to Emacs' Elisp format.

vscode_file = ARGV[0]
theme_name = ARGV.length > 1 ? ARGV[1] : "ported-from-vscode"
elisp_file = "#{theme_name}-theme.el"

vscode_theme_json = JSON.load(vscode_file)
theme_colors = vscode_theme['colors']
token_colors = vscode_theme['tokenColors']
