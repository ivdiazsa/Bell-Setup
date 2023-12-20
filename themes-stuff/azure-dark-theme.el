;;; azure-dark-theme.el - Azure Dark Color Theme ported from VSCode

;;; Code:

(deftheme azure-dark)

(defcustom azure-dark-box-org-todo t
  "Set box around TODO items in org-mode"
  :type 'boolean
  :group 'dark-plus)

(defcustom azure-dark-scale-org-faces t
  "Scale headlines and other org faces"
  :type 'boolean
  :group 'dark-plus)

(defcustom azure-dark-invert-hl-todo t
  "Set (:invert-video t) on hl-todo face"
  :type 'boolean
  :group 'dark-plus)

(let ((class '((class color) (min-colors 89)))
      (black-eerie      "#161920")
      (black-rich       "#0a0e14")
      (blue-bolt        "#00aeff")
      (blue-picton      "#39bae6")
      (cyan-pale        "#88ddff")
      (gray-charleston  "#24272e")
      (gray-dim         "#626A73")
      (gray-metal       "#292d36")
      (gray-philippine  "#8b888f")
      (green-sheen      "#72c598")
      (magnolia         "#f7f1ff")
      (orange-royal     "#fd9353")
      (pink-hot         "#ff66bb")
      (pistachio        "#91dd64")
      (red-deep-carmine "#ff3333")
      (silver-foil      "#b3b1ad")
      (violet-pastel    "#c594c5")
      (yellow           "#ffd14a"))

  (let ((builtin                blue-bolt)
        (comment                gray-dim) ;; Italic
        (constant               blue-bolt)
        (cursor-bg              black-rich)
        (cursor-fg              magnolia)
        (default-bg             black-rich)
        (default-fg             magnolia)
        (doc                    pistachio)
        (errorc                 red-deep-carmine)
        (func                   blue-picton)
        (highlight              blue-bolt)
        (header-line-bg         black-eerie)
        (header-line-fg         violet-pastel)
        (hl-line-bg             black-eerie)
        (hl-line-fg             gray-philippine)
        (isearch-bg             gray-charleston)
        (isearch-fg             blue-picton)
        (line-number-bg         black-eerie)
        (line-number-fg         gray-philippine)
        (line-number-current-bg black-rich)
        (line-number-current-fg violet-pastel)
        (link-bg                black-rich)
        (link-fg                blue-picton)
        (keyword                yellow)
        (numeric                pink-hot)
        (negation-char          blue-bolt)
        (paren-match-bg         magnolia)
        (paren-match-fg         gray-metal)
        (paren-mismatch-bg      red-deep-carmine)
        (paren-mismatch-fg      magnolia)
        (reference              blue-bolt)
        (regexp                 blue-bolt)
        (region-bg              gray-charleston)
        (region-fg              magnolia)
        (string                 pistachio)
        (success                green-sheen)
        (type                   cyan-pale)
        (variable               silver-foil) ;; Italic
        (warning                orange-royal))

    (custom-theme-set-faces
     'ventura-xcode-default
     `(default                                  ((,class (:background ,default-bg :foreground ,default-fg))))

     `(font-lock-builtin-face                   ((,class (:foreground ,builtin))))
     `(font-lock-comment-face                   ((,class (:foreground ,comment :italic t))))
     `(font-lock-negation-char-face             ((,class (:foreground ,negation-char))))
     `(font-lock-reference-face                 ((,class (:foreground ,reference))))
     `(font-lock-constant-face                  ((,class (:foreground ,constant))))
     `(font-lock-doc-face                       ((,class (:foreground ,doc))))
     `(font-lock-function-name-face             ((,class (:foreground ,func))))
     `(font-lock-keyword-face                   ((,class (:foreground ,keyword))))
     `(font-lock-string-face                    ((,class (:foreground ,string))))
     `(font-lock-type-face                      ((,class (:foreground ,type))))
     `(font-lock-variable-name-face             ((,class (:foreground ,variable :italic t))))
     `(font-lock-warning-face                   ((,class (:foreground ,warning))))
     `(font-lock-regexp-grouping-backslash      ((,class (:foreground ,regexp))))
     `(font-lock-regexp-grouping-construct      ((,class (:foreground ,regexp))))

     `(region                                   ((,class (:background ,region-bg :distant-foreground ,region-fg))))
     `(secondary-selection                      ((,class (:inherit region))))
     `(highlight                                ((,class (:foreground ,highlight :underline t)))) ; link hover
     `(hl-line                                  ((,class (:background ,hl-line-bg :foreground ,hl-line-fg))))
     `(fringe                                   ((,class (:background nil :foreground "#fefefe"))))
     `(cursor                                   ((,class (:background ,cursor-bg :foreground ,cursor-fg))))
     `(show-paren-match-face                    ((,class (:background ,warning))))
     `(show-paren-match                         ((t (:foreground ,paren-match-fg :background ,paren-match-bg :bold t))))
     `(show-paren-mismatch                      ((t (:background ,errorc))))
     `(isearch                                  ((,class (:background ,isearch-bg :foreground ,isearch-fg))))
     `(vertical-border                          ((,class (:foreground "#010101"))))
     `(minibuffer-prompt                        ((,class (:foreground ,yellow :weight normal))))
     `(default-italic                           ((,class (:italic t))))
     `(link                                     ((,class (:foreground ,link-fg :background ,link-bg))))
     `(error                                    ((,class (:foreground ,errorc))))
     `(warning                                  ((,class (:foreground ,warning))))
     `(success                                  ((,class (:foreground ,success))))
     `(dired-directory                          ((t (:inherit (font-lock-keyword-face)))))
     `(line-number                              ((,class (:foreground ,line-number-fg :background ,line-number-bg))))
     `(line-number-current-line                 ((,class (:foreground ,line-number-current-fg :background ,line-number-current-bg))))
     `(header-line                              ((,class (:inherit nil :foreground ,header-line-fg :background ,header-line-bg :box (:line-width -1)))))

     `(mode-line                                ((,class (:foreground ,default-fg :background ,blue-picton))))
     `(mode-line-inactive                       ((,class (:foreground ,line-number-fg :background ,line-number-bg))))
     `(mode-line-buffer-id                      ((,class (:foreground ,pink-hot))))
     `(mode-line-highlight                      ((,class (:foreground ,keyword :box nil :weight normal))))
     `(mode-line-emphasis                       ((,class (:foreground ,default-fg))))

     `(company-preview-common                   ((t (:foreground unspecified :background ,default-bg))))
     `(company-scrollbar-bg                     ((t (:background ,black-eerie))))
     `(company-scrollbar-fg                     ((t (:background ,black-rich))))
     `(company-tooltip                          ((t (:inherit default))))
     `(company-tooltip-common                   ((t (:foreground ,link-fg :bold t))))
     `(company-tooltip-selection                ((t (:background ,region-bg))))
     `(company-tooltip-annotation               ((t (:foreground ,doc)))) ; parameter hints etc.
     `(company-template-field                   ((t (:inherit region))))

