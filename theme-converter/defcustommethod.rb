# Defcustommethod.rb

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
