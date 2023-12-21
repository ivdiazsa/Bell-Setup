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
      (green-midnight   "#174a5c")
      (magnolia         "#f7f1ff")
      (orange-royal     "#fd9353")
      (pink-hot         "#ff66bb")
      (pistachio        "#91dd64")
      (red-deep-carmine "#ff3333")
      (silver-foil      "#b3b1ad")
      (violet-pastel    "#c594c5")
      (yellow           "#ffd14a")

      ;; Will remove these ones at some point.
      (ms-red-bg         "#551b1e")
      (ms-green-bg       "#39422a")
      (ms-blue-bg        "#040e3f")
      (ms-red-bghl       "#74140f")
      (ms-green-bghl     "#4b5332")
      (ms-blue-bghl      "#141e4f"))

  (let ((builtin                blue-bolt)
        (comment                gray-dim) ;; Italic
        (constant               blue-bolt)
        (cursor-bg              magnolia)
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
        (warning                orange-royal)

        (vc-r                   pink-hot)
        (vc-g                   green-sheen)
        (vc-b                   blue-picton))

    (custom-theme-set-faces
     'azure-dark
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

     `(mode-line                                ((,class (:foreground ,default-fg :background ,green-midnight))))
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

     `(org-level-1                              ((,class (:bold nil :foreground ,constant
                                                                ,@(when azure-dark-scale-org-faces (list :height 1.1))))))
     `(org-level-2                              ((,class (:bold nil :foreground ,func))))
     `(org-level-3                              ((,class (:bold nil :foreground ,keyword))))
     `(org-level-4                              ((,class (:bold nil :foreground ,type))))
     `(org-code                                 ((,class (:foreground ,string))))
     `(org-hide                                 ((,class (:foreground ,"#fefefe"))))
     `(org-date                                 ((,class (:underline t :foreground ,variable) )))
     `(org-footnote                             ((,class (:underline t :foreground ,doc))))
     `(org-link                                 ((,class (:underline t :foreground ,type))))
     `(org-special-keyword                      ((,class (:foreground ,success))))
     `(org-block                                ((,class (:foreground ,highlight :background ,cursor-bg :extend t))))
     `(org-quote                                ((,class (:inherit org-block :slant italic))))
     `(org-verse                                ((,class (:inherit org-block :slant italic))))
     `(org-todo                                 ((,class (,@(when azure-dark-box-org-todo (list :box '(:line-width 1 :color ,"#ff1111")))
                                                          :foreground "#ff1111" :bold nil))))
     `(org-done                                 ((,class (:box (:line-width 1 :color "#11ff11") :foreground "#11ff11" :bold nil ))))
     `(org-warning                              ((,class (:underline t :foreground ,warning))))
     `(org-agenda-structure                     ((,class (:weight normal :foreground ,header-line-fg :box (:color ,isearch-bg) :background ,header-line-bg))))
     `(org-agenda-date                          ((,class (:foreground ,variable ,@(when azure-dark-scale-org-faces (list :height 1.1))))))
     `(org-agenda-date-weekend                  ((,class (:weight normal :foreground ,isearch-fg))))
     `(org-agenda-date-today                    ((,class (:weight normal :foreground ,keyword
                                                                  ,@(when azure-dark-scale-org-faces (list :height 1.2))))))
     `(org-agenda-done                          ((,class (:foreground ,numeric))))
     `(org-scheduled                            ((,class (:foreground ,type))))
     `(org-scheduled-today                      ((,class (:foreground ,func :weight normal
                                                                      ,@(when azure-dark-scale-org-faces (list :height 1.2))))))
     `(org-ellipsis                             ((,class (:foreground ,builtin))))
     `(org-verbatim                             ((,class (:foreground ,variable))))
     `(org-document-title                       ((,class (:foreground ,type :bold t
                                                                      ,@(when azure-dark-scale-org-faces (list :height 1.2)))))) ; title
     `(org-document-info                        ((,class (:foreground ,doc)))) ; author, date etc.
     `(org-document-info-keyword                ((,class (:foreground ,builtin))))  ; "#+Title", "#+Date" etc.
     `(org-sexp-date                            ((,class (:foreground ,regexp))))
     `(org-table                                ((,class (:foreground ,hl-line-fg :background ,hl-line-bg))))

     `(font-latex-bold-face                     ((,class (:foreground ,type))))
     `(font-latex-italic-face                   ((,class (:foreground ,numeric :italic t))))
     `(font-latex-string-face                   ((,class (:foreground ,string))))
     `(font-latex-match-reference-keywords      ((,class (:foreground ,constant))))
     `(font-latex-match-variable-keywords       ((,class (:foreground ,variable))))

     `(ido-only-match                           ((,class (:foreground ,keyword))))
     `(ido-subdir                               ((,class (:weight normal :foreground ,isearch-fg))))
     `(ido-first-match                          ((,class (:foreground ,keyword :bold nil))))

     `(gnus-header-content                      ((,class (:foreground ,keyword))))
     `(gnus-header-from                         ((,class (:foreground ,variable))))
     `(gnus-header-name                         ((,class (:foreground ,type))))
     `(gnus-header-subject                      ((,class (:foreground ,func :bold nil))))

     ;; `(mu4e-view-url-number-face                ((,class (:foreground ,type))))
     ;; `(mu4e-cited-1-face                        ((,class (:foreground ,line-number-fg))))
     ;; `(mu4e-cited-7-face                        ((,class (:foreground ,hl-line-fg))))
     ;; `(mu4e-header-marks-face                   ((,class (:foreground ,type))))

     `(js2-private-function-call                ((,class (:foreground ,constant))))
     `(js2-jsdoc-html-tag-delimiter             ((,class (:foreground ,string))))
     `(js2-jsdoc-html-tag-name                  ((,class (:foreground ,warning))))
     `(js2-external-variable                    ((,class (:foreground ,type))))
     `(js2-function-param                       ((,class (:foreground ,constant))))
     `(js2-jsdoc-value                          ((,class (:foreground ,string))))
     `(js2-private-member                       ((,class (:foreground ,comment))))
     `(js2-warning                              ((t (:underline ,warning))))
     `(js2-error                                ((t (:foreground ,warning :weight normal))))
     `(js2-jsdoc-tag                            ((t (:foreground ,variable))))
     `(js2-jsdoc-type                           ((t (:foreground ,variable))))
     `(js2-instance-member                      ((t (:foreground ,variable))))
     `(js2-object-property                      ((t (:foreground ,func))))
     `(js2-magic-paren                          ((t (:foreground ,constant))))
     `(js2-function-call                        ((t (:foreground ,constant))))
     `(js2-keywords                             ((t (:foreground ,keyword))))
     `(js3-warning-face                         ((,class (:underline ,keyword))))
     `(js3-error-face                           ((,class (:underline ,warning))))
     `(js3-external-variable-face               ((,class (:foreground ,variable))))
     `(js3-function-param-face                  ((,class (:foreground ,reference))))
     `(js3-jsdoc-tag-face                       ((,class (:foreground ,keyword))))
     `(js3-instance-member-face                 ((,class (:foreground ,constant))))

     `(ac-completion-face                       ((,class (:underline t :foreground ,keyword))))
     `(info-quoted-name                         ((,class (:foreground ,builtin))))
     `(info-string                              ((,class (:foreground ,string))))
     `(icompletep-determined                    ((,class :foreground ,builtin)))

     `(slime-repl-inputed-output-face           ((,class (:foreground ,type))))
     `(trailing-whitespace                      ((,class :foreground nil :background ,warning)))
     `(lazy-highlight                           ((,class (:background "#613214"))))

     `(undo-tree-visualizer-current-face        ((,class :foreground ,builtin)))
     `(undo-tree-visualizer-default-face        ((,class :foreground ,default-fg)))
     `(undo-tree-visualizer-unmodified-face     ((,class :foreground ,variable)))
     `(undo-tree-visualizer-register-face       ((,class :foreground ,type)))

     `(rainbow-delimiters-depth-1-face          ((,class :foreground "gold")))
     `(rainbow-delimiters-depth-2-face          ((,class :foreground "orchid")))
     `(rainbow-delimiters-depth-3-face          ((,class :foreground "LightSkyBlue")))
     `(rainbow-delimiters-depth-4-face          ((,class :foreground "gold")))
     `(rainbow-delimiters-depth-5-face          ((,class :foreground "orchid")))
     `(rainbow-delimiters-depth-6-face          ((,class :foreground "LightSkyBlue")))
     `(rainbow-delimiters-depth-7-face          ((,class :foreground "gold")))
     `(rainbow-delimiters-depth-8-face          ((,class :foreground "orchid")))
     `(rainbow-delimiters-depth-9-face          ((,class :foreground "LightSkyBlue")))
     `(rainbow-delimiters-unmatched-face        ((,class :foreground ,errorc)))

     `(magit-item-highlight                     ((,class :background ,header-line-bg)))
     `(magit-hunk-heading                       ((,class (:background ,header-line-bg))))
     `(magit-hunk-heading-highlight             ((,class (:background ,header-line-bg))))
     `(magit-bisect-bad                         ((t (:foreground ,errorc))))
     `(magit-bisect-good                        ((t (:foreground ,success))))
     `(magit-bisect-skip                        ((t (:foreground ,warning))))
     `(magit-blame-date                         ((t (:foreground ,paren-mismatch-bg))))
     `(magit-blame-heading                      ((t (:foreground ,header-line-fg :background ,header-line-bg :extend t))))
     `(magit-branch                             ((,class (:foreground ,link-fg, :weight normal))))
     `(magit-branch-current                     ((t (:foreground ,isearch-fg))))
     `(magit-branch-local                       ((t (:foreground ,line-number-current-fg))))
     `(magit-branch-remote                      ((t (:foreground ,line-number-fg))))
     `(magit-cherry-equivalent                  ((t (:foreground ,numeric))))
     `(magit-cherry-unmatched                   ((t (:foreground ,negation-char))))
     `(magit-diff-added                         ((t (:foreground ,hl-line-fg :background ,ms-green-bg :extend t))))
     `(magit-diff-added-highlight               ((t (:foreground ,isearch-fg :background ,ms-green-bghl :extend t))))
     `(magit-diff-removed                       ((t (:foreground ,hl-line-fg :background ,ms-red-bg :extend t))))
     `(magit-diff-removed-highlight             ((t (:foreground ,isearch-fg :background ,ms-red-bghl :extend t))))
     `(magit-diff-base                          ((t (:foreground ,default-bg :background ,vc-r :extend t))))
     `(magit-diff-base-highlight                ((t (:foreground ,highlight :background ,header-line-bg :extend t))))
     `(magit-diff-context                       ((t (:foreground ,default-bg :extend t))))
     `(magit-diff-context-highlight             ((,class (:foreground ,header-line-fg :background ,header-line-bg))))
     `(magit-diff-file-header                   ((,class (:foreground ,hl-line-fg :background ,hl-line-bg))))
     `(magit-diff-file-heading                  ((t (:foreground ,default-fg :extend t))))
     `(magit-diff-file-heading-highlight        ((t (:background ,hl-line-bg :extend t))))
     `(magit-diff-file-heading-selection        ((t (:foreground ,isearch-fg :background ,isearch-bg :extend t))))
     `(magit-diff-hunk-heading                  ((t (:foreground ,default-fg :background ,default-bg :extend t))))
     `(magit-diff-hunk-heading-highlight        ((t (:background ,header-line-bg :extend t))))
     `(magit-diff-lines-heading                 ((t (:foreground ,builtin :background ,errorc :extend t))))
     `(magit-diffstat-added                     ((t (:foreground ,vc-g))))
     `(magit-diffstat-removed                   ((t (:foreground ,vc-r))))
     `(magit-dimmed                             ((t (:foreground ,comment))))
     `(magit-filename                           ((t (:foreground ,string))))
     `(magit-hash                               ((t (:foreground ,comment))))
     `(magit-header-line                        ((t (:inherit nil))))
     `(magit-log-author                         ((t (:foreground ,vc-r))))
     `(magit-log-date                           ((t (:foreground ,vc-b))))
     `(magit-log-graph                          ((t (:foreground ,comment))))
     `(magit-mode-line-process                  ((t (:foreground ,vc-r))))
     `(magit-mode-line-process-error            ((t (:foreground ,errorc))))
     `(magit-process-ok                         ((t (:inherit success))))
     `(magit-process-ng                         ((t (:inherit error))))
     `(magit-reflog-amend                       ((t (:foreground ,warning))))
     `(magit-reflog-checkout                    ((t (:foreground ,vc-b))))
     `(magit-reflog-cherry-pick                 ((t (:foreground ,vc-g))))
     `(magit-reflog-commit                      ((t (:foreground ,vc-g))))
     `(magit-reflog-merge                       ((t (:foreground ,vc-g))))
     `(magit-reflog-other                       ((t (:foreground ,reference))))
     `(magit-reflog-rebase                      ((t (:foreground ,numeric))))
     `(magit-reflog-remote                      ((t (:foreground ,vc-b))))
     `(magit-reflog-reset                       ((t (:inherit error))))
     `(magit-refname                            ((t (:foreground ,comment))))
     `(magit-section-heading                    ((t (:foreground ,header-line-fg))))
     `(magit-section-heading-selection          ((t (:foreground ,vc-r :extend t))))
     `(magit-section-highlight                  ((t (:background ,header-line-bg :extend t))))
     `(magit-sequence-drop                      ((t (:foreground ,errorc))))
     `(magit-sequence-head                      ((t (:foreground ,vc-b))))
     `(magit-sequence-part                      ((t (:foreground ,vc-r))))
     `(magit-sequence-stop                      ((t (:foreground ,vc-g))))
     `(magit-signature-bad                      ((t (:inherit error))))
     `(magit-signature-error                    ((t (:inherit error))))
     `(magit-signature-expired-key              ((t (:foreground ,vc-r))))
     `(magit-signature-good                     ((t (:inherit success))))
     `(magit-signature-revoked                  ((t (:foreground ,errorc))))
     `(magit-signature-untrusted                ((t (:foreground ,warning))))
     `(magit-tag                                ((t (:foreground ,success))))

     `(git-commit-summary                       ((t (:inherit default)))) ; magit commit message face

     `(diredfl-autofile-name                    ((t (:foreground ,header-line-bg))))
     `(diredfl-compressed-file-name             ((t (:foreground ,warning))))
     `(diredfl-compressed-file-suffix           ((t (:foreground ,doc))))
     `(diredfl-date-time                        ((t (:foreground ,line-number-current-fg))))
     `(diredfl-deletion                         ((t (:foreground ,errorc :bold t))))
     `(diredfl-deletion-file-name               ((t (:foreground ,errorc ))))
     `(diredfl-dir-heading                      ((t (:foreground ,constant :bold t))))
     `(diredfl-dir-name                         ((t (:foreground ,func))))
     `(diredfl-dir-priv                         ((t (:foreground ,negation-char))))
     `(diredfl-exec-priv                        ((t (:foreground ,success))))
     `(diredfl-executable-tag                   ((t (:foreground ,success))))
     `(diredfl-file-name                        ((t (:foreground ,default-fg))))
     `(diredfl-file-suffix                      ((t (:foreground ,doc))))
     `(diredfl-flag-mark                        ((t (:foreground ,keyword :bold t))))
     `(diredfl-ignored-file-name                ((t (:foreground ,doc))))
     `(diredfl-link-priv                        ((t (:foreground ,link-fg))))
     `(diredfl-no-priv                          ((t (:foreground ,default-fg))))
     `(diredfl-number                           ((t (:foreground ,numeric))))
     `(diredfl-other-priv                       ((t (:foreground ,variable))))
     `(diredfl-rare-priv                        ((t (:foreground ,default-fg))))
     `(diredfl-read-priv                        ((t (:foreground ,string))))
     `(diredfl-symlink                          ((t (:foreground ,builtin))))
     `(diredfl-tagged-autofile-name             ((t (:foreground ,cursor-bg))))
     `(diredfl-write-priv                       ((t (:foreground ,errorc))))

     `(helm-header                              ((,class (:foreground ,header-line-fg :background ,header-line-bg :underline nil :box nil))))
     `(helm-source-header                       ((,class (:foreground ,keyword :background ,header-line-fg :underline nil :weight normal))))
     `(helm-selection                           ((,class (:background ,hl-line-fg :underline nil :extend t))))
     `(helm-selection-line                      ((,class (:background ,hl-line-fg :extend t))))
     `(helm-visible-mark                        ((,class (:foreground ,default-bg :background ,region-bg))))
     `(helm-candidate-number                    ((,class (:foreground ,line-number-current-bg :background ,region-fg))))
     `(helm-separator                           ((,class (:foreground ,type :background ,default-bg))))
     `(helm-time-zone-current                   ((,class (:foreground ,builtin :background ,default-bg))))
     `(helm-time-zone-home                      ((,class (:foreground ,type :background ,default-bg))))
     `(helm-buffer-not-saved                    ((,class (:foreground ,type :background ,default-bg))))
     `(helm-buffer-process                      ((,class (:foreground ,builtin :background ,default-bg))))
     `(helm-buffer-saved-out                    ((,class (:foreground ,default-fg :background ,default-bg))))
     `(helm-buffer-size                         ((,class (:foreground ,numeric :background ,default-bg))))
     `(helm-ff-directory                        ((,class (:foreground ,func :background ,default-bg :weight normal))))
     `(helm-ff-file                             ((,class (:foreground ,default-fg :background ,default-bg :weight normal))))
     `(helm-ff-executable                       ((,class (:foreground ,warning :background ,default-bg :weight normal))))
     `(helm-ff-invalid-symlink                  ((,class (:foreground ,errorc :background ,default-bg :weight normal))))
     `(helm-ff-symlink                          ((,class (:foreground ,keyword :background ,default-bg :weight normal))))
     `(helm-ff-prefix                           ((,class (:foreground ,default-bg :background ,keyword :weight normal))))
     `(helm-grep-cmd-line                       ((,class (:foreground ,highlight :background ,default-bg))))
     `(helm-grep-file                           ((,class (:foreground ,default-fg :background ,default-bg))))
     `(helm-grep-finish                         ((,class (:foreground ,region-fg :background ,default-bg))))
     `(helm-grep-lineno                         ((,class (:foreground ,line-number-fg :background ,line-number-bg))))
     `(helm-grep-match                          ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-grep-running                        ((,class (:foreground ,func :background ,default-bg))))
     `(helm-moccur-buffer                       ((,class (:foreground ,func :background ,default-bg))))
     `(helm-source-go-package-godoc-description ((,class (:foreground ,string))))
     `(helm-bookmark-w3m                        ((,class (:foreground ,type))))

     `(web-mode-html-tag-bracket-face           ((,class (:foreground "#808080"))))
     `(web-mode-html-tag-face                   ((,class (:foreground ,keyword))))
     `(web-mode-html-attr-name-face             ((,class (:foreground ,variable))))
     `(web-mode-html-attr-value-face            ((,class (:foreground ,string))))
     `(web-mode-html-attr-equal-face            ((,class (:foreground ,highlight))))
     `(web-mode-builtin-face                    ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face                    ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face                   ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-keyword-face                    ((,class (:foreground ,keyword))))
     `(web-mode-doctype-face                    ((,class (:inherit ,font-lock-doc-face))))
     `(web-mode-function-name-face              ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-string-face                     ((,class (:foreground ,string))))
     `(web-mode-type-face                       ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face                    ((,class (:inherit ,font-lock-warning-face))))
     `(web-mode-json-key-face                   ((,class (:foreground ,string))))
     `(web-mode-json-context-face               ((,class (:foreground ,string))))

     `(diff-header                              ((t (:foreground ,header-line-fg :background nil))))
     `(diff-file-header                         ((t (:foreground ,highlight :background nil))))
     `(diff-hunk-header                         ((t (:foreground ,doc :background header-line-bg))))
     `(diff-added                               ((t (:foreground ,hl-line-fg :background ,ms-green-bg))))
     `(diff-removed                             ((t (:foreground ,hl-line-fg :background ,ms-red-bg))))
     `(diff-changed                             ((t (:foreground ,hl-line-fg :background ,ms-blue-bg))))
     `(diff-refine-added                        ((t (:foreground ,default-fg :background ,ms-green-bghl))))
     `(diff-refine-removed                      ((t (:foreground ,default-fg :background ,ms-red-bghl))))
     `(diff-refine-changed                      ((t (:foreground ,default-fg :background ,ms-blue-bghl))))

     `(ediff-fine-diff-Ancestor                 ((t (:background ,ms-red-bghl))))
     `(ediff-fine-diff-A                        ((t (:background ,ms-red-bghl))))
     `(ediff-fine-diff-B                        ((t (:background ,ms-green-bghl))))
     `(ediff-fine-diff-C                        ((t (:background ,ms-blue-bghl))))
     `(ediff-current-diff-Ancestor              ((t (:background ,ms-red-bg))))
     `(ediff-current-diff-A                     ((t (:background ,ms-red-bg))))
     `(ediff-current-diff-B                     ((t (:background ,ms-green-bg))))
     `(ediff-current-diff-C                     ((t (:background ,ms-blue-bg))))
     `(ediff-even-diff-Ancestor                 ((t (:background ,cursor-bg))))
     `(ediff-even-diff-A                        ((t (:background ,cursor-bg))))
     `(ediff-even-diff-B                        ((t (:background ,cursor-bg))))
     `(ediff-even-diff-C                        ((t (:background ,cursor-bg))))
     `(ediff-odd-diff-Ancestor                  ((t (:background ,cursor-bg))))
     `(ediff-odd-diff-A                         ((t (:background ,cursor-bg))))
     `(ediff-odd-diff-B                         ((t (:background ,cursor-bg))))
     `(ediff-odd-diff-C                         ((t (:background ,cursor-bg))))

     `(jde-java-font-lock-package-face          ((t (:foreground ,variable))))
     `(jde-java-font-lock-public-face           ((t (:foreground ,keyword))))
     `(jde-java-font-lock-private-face          ((t (:foreground ,keyword))))
     `(jde-java-font-lock-constant-face         ((t (:foreground ,constant))))
     `(jde-java-font-lock-modifier-face         ((t (:foreground ,type))))
     `(jde-jave-font-lock-protected-face        ((t (:foreground ,keyword))))
     `(jde-java-font-lock-number-face           ((t (:foreground ,numeric))))

     `(git-gutter:added                         ((t (:background ,vc-g :foreground ,vc-g :weight normal))))
     `(git-gutter:deleted                       ((t (:background ,vc-r :foreground ,vc-r :weight normal))))
     `(git-gutter:modified                      ((t (:background ,vc-b :foreground ,vc-b :weight normal))))
     `(git-gutter-fr:added                      ((t (:background ,vc-g :foreground ,vc-g :weight normal))))
     `(git-gutter-fr:deleted                    ((t (:background ,vc-r :foreground ,vc-r :weight normal))))
     `(git-gutter-fr:modified                   ((t (:background ,vc-b :foreground ,vc-b :weight normal))))

     `(diff-hl-insert                           ((t (:background ,vc-g :foreground ,vc-g))))
     `(diff-hl-delete                           ((t (:background ,vc-r :foreground ,vc-r))))
     `(diff-hl-change                           ((t (:background ,vc-b :foreground ,vc-b))))

     `(evil-ex-substitute-matches               ((t (:foreground ,warning :weight normal :strike-through t))))
     `(evil-ex-substitute-replacement           ((t (:foreground ,isearch-fg :weight normal))))

     `(hl-todo                                  ((t ,@(when azure-dark-invert-hl-todo (list :inverse-video t)))))
     `(highlight-numbers-number                 ((t (:foreground ,numeric))))
     `(highlight-operators-face                 ((t (:inherit default))))
     `(highlight-symbol-face                    ((t (:background "#343a40"))))

     `(window-divider                           ((t (:foreground "gray40"))))
     `(window-divider-last-pixel                ((t (:foreground "gray20"))))
     `(window-divider-first-pixel               ((t (:foreground "gray60"))))

     `(tree-sitter-hl-face:method.call          ((t (:inherit font-lock-function-name-face))))
     `(tree-sitter-hl-face:function.call        ((t (:inherit font-lock-function-name-face))))
     `(tree-sitter-hl-face:operator             ((t (:inherit default))))
     `(tree-sitter-hl-face:type.builtin         ((t (:inherit font-lock-keyword-face))))
     `(tree-sitter-hl-face:number               ((t (:inherit highlight-numbers-number))))

     `(tab-bar-tab-inactive                     ((t (:background ,header-line-bg))))
     `(tab-bar-tab                              ((t (:background ,default-bg :foreground ,default-fg))))
     `(tab-bar                                  ((t (:background ,default-bg)))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'azure-dark)
(provide 'azure-dark-theme)

;;; azure-dark.el ends here
