# Properties_Table.rb

# Here, we have a set of data storing structures to easily keep and retrieve the
# properties used in the VSCode and Emacs theme definitions.

class PropertyData

  attr_reader :emacs, :vsc_key, :vsc_property, :vsc_fallbacks

  def initialize(emacs_prop, vscode_prop_type, vscode_prop, vscode_others)
    @emacs = emacs_prop
    @vsc_key = vscode_prop_type
    @vsc_property = vscode_prop
    @vsc_fallbacks = vscode_others
  end

end

# Class where we will store all the mapped properties.

class PropertiesTable

  @@props = {}

  def self.init
    @@props[:default_fg] = PropertyData.new('default-fg',
                                            'colors',
                                            'editor.foreground',
                                            ['foreground'])

    @@props[:default_bg] = PropertyData.new('default-bg',
                                            'colors',
                                            'editor.background',
                                            ['background'])

    @@props[:builtin] = PropertyData.new('builtin',
                                         'tokens',
                                         'constant.language',
                                         [])
  end

end
