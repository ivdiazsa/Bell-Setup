#!/usr/bin/ruby

# Little application to help me port color themes from VSCode's JSON Format
# to Emacs' Elisp format.

require 'json'

require './defcustommethod'

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

    # Generate the big list of colors at the beginning of the (let) statement.
    @elisp_translated_code << gen_colors_letlist(theme_colors, token_colors)

    # Write down all our code to the new theme file!
    File.open(@elisp_file, 'w') do |ef|
      ef.write(@elisp_translated_code)
    end
  end

  def debugstuff
    vscode_theme_json = JSON.load(File.read(@vscode_file))
    theme_colors = vscode_theme_json['colors']
    token_colors = vscode_theme_json['tokenColors']

    puts "\nCOLORS:\n#{theme_colors}\n"
    # puts "TOKENS:\n#{token_colors}"
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

  def gen_colors_letlist(theme_c, tokens_c)
    code = "\n(let ((class '((class color) (min-colors 89)))\n"

    # The 'color-properties.txt' file contains a table with 4 columns:
    #
    #    * Name of color variable in Emacs.
    #    * JSON Key where the VSCode property is found in the theme.json file.
    #    * Name of the equivalent property in the VSCode theme file.
    #    * List of other VSCode properties we could consider equivalent and use,
    #      in the case the main one is not found.
    #
    # So, the first value will be used to name the 'let' variable containing the
    # color hex value in our Emacs theme 'el' file.
    #
    # Now, the VSCode theme json file can have the values we're looking for in
    # under one of two keys: 'Colors' and 'Tokens'. This is the meaning of the
    # second value in the table row described above.
    #
    # The third and fourth values are the VSCode properties equivalent to the
    # one in Emacs we want to translate to. Ideally, the third value should be
    # enough, but VSCode themes can be written in a myriad of ways. So, the
    # property we need might not be there. For these cases, we have a list of
    # fallback properties we can use. In theory, we're guaranteed to find at
    # least one of those :)

    File.foreach('color-properties.txt') do |row|
      values = row.split(' ')
      color = "#010101"

      emacs_colorsem = values[0]
      vscode_key = values[1]
      vscode_colorprop = values[2]
      vscode_fallbacks = values[3]

      puts "Emacs Property: #{emacs_colorsem}"

      if (vscode_key == 'colors') then
        color = get_hex_from_colors_hash(theme_c, vscode_colorprop, vscode_fallbacks)

      elsif (vscode_key == 'tokens') then
        color = get_hex_from_tokens_hash(tokens_c, vscode_colorprop, vscode_fallbacks)
      end

      code << "      (#{emacs_colorsem} ###Color goes here###)"
    end
  end

  def get_hex_from_colors_hash(colors, target_prop, fallback_props)
    # If the ideal key is present in the VSCode theme file, then our task here
    # is complete.
    if (colors.has_key?(target_prop)) then
      puts "Found property key #{target_prop} with value #{colors[target_prop]}"
      return colors[target_prop] if colors.has_key?(target_prop)
    end

    fallbacks = []

    puts "Property key #{target_prop} was not found. Checking the fallbacks..."
    fallback_props.split(',').each { |prop| fallbacks << [prop, colors[prop]] }

    puts "The following fallback properties were found:"
    fallbacks.each_with_index { |prop, i| puts "  #{i}) #{prop[0]} => '#{prop[1]}'" }

    # Prompt the user for which fallback property they want to use.
    # Return the hex code of said property.
  end

  def get_hex_from_tokens_hash(tokens, target_prop, fallback_props)
    "#fefefe"
  end
end

# ******************************************* #
# Little application script starting point :)
# ******************************************* #

# ENHANCEME: Validate we actually received at least one parameter value.
vscode_file = ARGV[0]
theme_name = ARGV.length > 1 ? ARGV[1] : "ported-from-vscode"

app = ThemeConverter.new(vscode_file, theme_name)
# app.translate
app.debugstuff
