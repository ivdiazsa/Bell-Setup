;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(require 'ibuf-ext)
(require 'multiple-cursors)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load my Org-Mode configuration.

(load-file "~/.emacs.d/org-mode-config/my-org-mode-config.el")

;; Some file extensions use the same syntax as XML. Let Emacs to enable xml-mode
;; by default when editing these types of files.

(add-to-list 'auto-mode-alist '("\\.csproj\\'"     . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ilproj\\'"     . xml-mode))
(add-to-list 'auto-mode-alist '("\\.proj\\'"       . xml-mode))
(add-to-list 'auto-mode-alist '("\\.depproj\\'"    . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sfxproj\\'"    . xml-mode))
(add-to-list 'auto-mode-alist '("\\.bundleproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props\\'"      . xml-mode))
(add-to-list 'auto-mode-alist '("\\.targets\\'"    . xml-mode))
(add-to-list 'auto-mode-alist '("\\.rpy\\'"        . python-mode))

;; Some nice general configuration :)

(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default scroll-conservatively most-positive-fixnum)
(setq-default frame-title-format
              '("" "%b%* (%f) %p/%P - GNU Emacs " emacs-version " at " system-name))
(setq-default auth-sources '("~/.authinfo.gpg"))
(setq-default ediff-forward-word-function 'forward-char)
(setq-default delete-selection-mode t)

;; List containing the checkpoint characters for the *-word-conservative functions
;; defined later on.
;;
;; KEY:
;;  9 = Tab
;; 10 = Newline/Line Feed
;; 11 = Vertical Tabulation
;; 12 = Form Feed
;; 32 = Space
;; 40 = Open Parenthesis
;; 41 = Close Parenthesis
;; 45 = Dash/Hyphen/Minus
;; 46 = Period/Dot/Full Stop
;; 47 = Slash/Divide
;; 58 = Colon
;; 59 = Semicolon
;; 60 = Open Angled Bracket/Less Than
;; 62 = Close Angled Bracket/Greater Than
;; 91 = Open Square Bracket
;; 92 = Backslash
;; 93 = Close Square Bracket
;; 95 = Underscore

;; (defvar boundary-symbols '(45 46 47 58 59 92 95))
;; (defvar opening-symbols '(40 60))
;; (defvar closing-symbols '(41 62))
;; (defvar whitespaces '(9 10 11 12 32))

;; Getter function to know whether a character is a word boundary defined by us.

;; (defun is-conservative-boundary (character)
;;   "Return 't' if CHARACTER is in one of the conservative boundary symbols lists,
;; or 'nil' otherwise."
;;   (or (member character boundary-symbols)
;;       (member character opening-symbols)
;;       (member character closing-symbols)
;;       (member character whitespaces)))

;; ;; Getter function to know whether a character is an opening symbol defined by us.

;; (defun is-opening-symbol (character)
;;   "Return 't' if CHARACTER is in the 'opening-symbols' list, or 'nil' otherwise."
;;   (if (member character opening-symbols) t nil))

;; ;; Getter function to know whether a character is a closing symbol defined by us.

;; (defun is-closing-symbol (character)
;;   "Return 't' if CHARACTER is in the 'closing-symbols' list, or 'nil' otherwise."
;;   (if (member character closing-symbols) t nil))

;; ;; Getter function to know whether a character is an encapsulating bracket one
;; ;; defined by us.

;; (defun is-bracket-symbol (character)
;;   "Return 't' if CHARACTER is in either the 'opening-symbols' or 'closing-symbols'
;; lists, or 'nil' otherwise."
;;   (if (or (is-opening-symbol character)
;;           (is-closing-symbol character))
;;       t nil))

;; ;; Getter function to know whether a character is a whitespace one defined by us.

;; (defun is-whitespace (character)
;;   "Return 't' if CHARACTER is in the 'whitespaces' list, or 'nil' otherwise."
;;   (if (member character whitespaces) t nil))

;; ;; Getter function to know whether a character is a spacing delimiter defined
;; ;; by us (i.e. space or tab).

;; (defun is-spacing-delimiter (character)
;;   "Return 't' if CHARACTER is a space or a tab, or 'nil' otherwise."
;;   (or (equal 9 character)
;;       (equal 32 character)))

;; Function to easily toggle between absolute/normal and relative line numbering.

(defun toggle-line-numbers-type ()
  "Toggle between absolute and relative line numbering."
  (interactive)
  (if (not (equal display-line-numbers-type 'relative))
      (setq-default display-line-numbers-type 'relative)
    (setq-default display-line-numbers-type 'absolute))
  (display-line-numbers-mode 1))

;; ;; Function to jump forward between words without ignoring symbols-only words.

;; (defun forward-word-conservative (arg)
;;   "Move forward to the next word conservatively. In this context, conservatively
;; means to stop at the next boundary or word start character (i.e. 'words' made up
;; of just symbols also count as words). With argument ARG, do this that many times."
;;   (interactive "p")

;;   ;; A negative arg means we want to move backward, so we call the conservative
;;   ;; backwards word friend function.
;;   (if (< arg 0)
;;       (backward-word-conservative (* arg -1))
;;     (dotimes (number arg)

;;       (let ((start (char-after)))
;;         ;; If we begin on a normal text character, then first, we need to find its
;;         ;; boundary to define where we should go next.
;;         (while (not (is-conservative-boundary (char-after)))
;;           (forward-char))

;;         (cond (;; If we began at a section breaking character (e.g. newline), then
;;                ;; our next word starts at the first non-whitespace character.
;;                (and (is-whitespace (char-after))
;;                     (not (is-spacing-delimiter start)))
;;                (while (is-whitespace (char-after))
;;                  (forward-char)))

;;               ;; If we encounter a bracket symbol, then our next stop is the end of
;;               ;; that chain of brackets.
;;               ((is-bracket-symbol (char-after))
;;                (while (is-bracket-symbol (char-after))
;;                  (forward-char)))

;;               ;; If we encounter any other boundary symbol (e.g. hyphens, underscores),
;;               ;; then our next stop is what comes after them.
;;               (t (while (and (is-conservative-boundary (char-after))
;;                              (not (is-whitespace (char-after))))
;;                    (forward-char))))

;;         ;; If we began on any character other than section boundaries, and we ended
;;         ;; up on a space after all the movements in the previous 'cond' statement,
;;         ;; then we continue until we find our next word or the end of the current line.
;;         (unless (and (is-whitespace start)
;;                      (not (is-spacing-delimiter start)))
;;           (while (is-spacing-delimiter (char-after))
;;             (forward-char)))))))

;; ;; Function to jump backward between words without ignoring symbols-only words.

;; (defun backward-word-conservative (arg)
;;   "Move backward to the previous word conservatively. In this context, conservatively
;; means to stop at the previous boundary or word start character (i.e. 'words' made up
;; of just symbols also count as words). With argument ARG, do this that many times."
;;   (interactive "p")

;;   ;; A negative arg means we want to move forward, so we call the conservative
;;   ;; forwards word friend function.
;;   (if (< arg 0)
;;       (forward-word-conservative (* arg -1))
;;     (dotimes (number arg)

;;       (cond (;; If we begin on a character whose previous neighbor is a whitespace
;;              ;; one, then the first step is to get to the previous non-whitespace
;;              ;; character. The next step is handled after this 'cond' statement.
;;              (is-whitespace (char-before))
;;              (backward-char)
;;              (while (is-whitespace (char-after))
;;                (backward-char)))

;;             ;; If we begin on a bracket symbol, then similarly to whitespaces, we
;;             ;; need to find the previous non-bracket symbol character.
;;             ((is-bracket-symbol (char-after))
;;              (while (is-bracket-symbol (char-after))
;;                (backward-char)))

;;             ;; If we begin on any boundary symbol other than brackets and whitespaces,
;;             ;; then the first step is to find the previous character that is not
;;             ;; a boundary character or whitespace. Whitespace is also considered as a
;;             ;; stopping point because being next to a whitespace character means we're
;;             ;; already at the beginning of the word, by definition.
;;             ((is-conservative-boundary (char-before))
;;              (while (and (is-conservative-boundary (char-before))
;;                          (not (is-whitespace (char-before))))
;;                (backward-char))))

;;       ;; After all the potentially done work in the previous 'cond' statement, we
;;       ;; are very most likely to be on a normal text character. In this case, then
;;       ;; our goal is the first previous text character that follows a boundary one.
;;       (while (not (is-conservative-boundary (char-before)))
;;         (backward-char))

;;       ;; There is a specific case where after all the work done previously in this
;;       ;; function, we will still end up on a non-whitespace boundary character.
;;       ;; This specific case happens when the previous word is composed of only
;;       ;; boundary characters. In such case, then our previous word's beginning is
;;       ;; after the previous non-boundary character.
;;       (when (is-conservative-boundary (char-after))
;;         (while (and (is-conservative-boundary (char-before))
;;                     (not (is-whitespace (char-before))))
;;           (backward-char))

;;         ;; This additional loop might look strange but it's needed for this specific
;;         ;; case. Let's take the following example:
;;         ;;
;;         ;;   test-before(((( ___boundarycase))))
;;         ;;
;;         ;; If we do 'backward-word-conservative' without this additional loop,
;;         ;; starting the cursor at the following position, with an asterisk (1):
;;         ;;
;;         ;;   1. test-before(((( *__boundarycase))))
;;         ;;   2. test-before*((( ___boundarycase))))
;;         ;;   3. test-*efore(((( ___boundarycase))))
;;         ;;
;;         ;; Then we end up at the first opening parenthesis character (2). This would
;;         ;; be correct if there was whitespace before said parenthesis, but there is
;;         ;; text in this case. So, the fully correct version should move the cursor
;;         ;; to the beginning of that text (3).
;;         (while (and (not (is-conservative-boundary (char-before)))
;;                     (not (is-whitespace (char-before))))
;;           (backward-char))))))

(defun find-char-forward ()
  "Find the next occurrence of the given char and move the cursor to it."
  (interactive)
  (let ((c (read-char "Character: ")))
    (forward-char)
    (while (not (equal c (char-after)))
      (forward-char))))

(defun find-char-backward ()
  "Find the previous occurrence of the given char and move the cursor to it."
  (interactive)
  (let ((c (read-char "Character: ")))
    (while (not (equal c (char-before)))
      (backward-char))
    (backward-char)))

(defun forward-word-whitespace (arg)
  "Move cursor to the next word. In this context, word is that set of characters
that is bordered by whitespace on both sides.
  Desirable features:
    * Stop at the end of the line regardless of type of character."
  (interactive "p")
  (while (not (equal 32 (char-after)))
    (forward-char))
  (while (equal 32 (char-after))
    (forward-char)))

(windmove-default-keybindings)

;; Useful and Productive Keybindings!

(global-set-key (kbd "C-x x w") 'toggle-word-wrap)
(global-set-key (kbd "C-x x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x x f") 'menu-set-font)
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (toggle-line-numbers-type)))
(global-set-key (kbd "M-n") 'display-line-numbers-mode)
(global-set-key (kbd "C-x C-M-b") 'ibuffer)

;; Text modifying and navigation keyboard shortcuts

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x E") 'erase-buffer)
(global-set-key (kbd "C-x t h") 'sgml-tag)
(global-set-key (kbd "M-s M-s") 'wrap-region-global-mode)
(global-set-key (kbd "M-s M-r") 'narrow-to-region)
(global-set-key (kbd "M-s M-e") 'widen)

(global-set-key (kbd "C-x M-f") 'find-char-forward)
(global-set-key (kbd "C-x M-F") 'find-char-backward)
(global-set-key (kbd "M-F") 'forward-word-whitespace)
;; (global-set-key (kbd "M-B") 'backward-word-conservative)

;; Frame manipulation keyboard shortcuts

(global-set-key (kbd "M-g t") 'transpose-frame)
(global-set-key (kbd "M-g r") 'rotate-frame-clockwise)
(global-set-key (kbd "M-g i") 'flip-frame)
(global-set-key (kbd "M-g o") 'flop-frame)

;; Magit keyboard shortcuts.

(global-set-key (kbd "C-x C-k k") 'magit-kill-this-buffer)
(global-set-key (kbd "C-x M-a") 'magit-blame-addition)
(global-set-key (kbd "C-x M-r") 'magit-blame-removal)

;; Keybindings for easy split windows resizing.

(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)

;; Configured keybindings for multiple cursors mode.

(define-key mc/keymap (kbd "<return>") nil)

(global-set-key (kbd "C-S-c C-S-n") 'mc/edit-lines)
(global-set-key (kbd "C-S-j") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-k") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Keywords not highlighted by default but I believe should be.

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<add-hook\\>"             . font-lock-keyword-face)
                          ("\\<add-to-list\\>"          . font-lock-keyword-face)
                          ("\\<custom-set-faces\\>"     . font-lock-keyword-face)
                          ("\\<custom-set-variables\\>" . font-lock-keyword-face)
                          ("\\<define-key\\>"           . font-lock-keyword-face)
                          ("\\<eql\\>"                  . font-lock-keyword-face)
                          ("\\<equal\\>"                . font-lock-keyword-face)
                          ("\\<format\\>"               . font-lock-keyword-face)
                          ("\\<global-set-key\\>"       . font-lock-keyword-face)
                          ("\\<global-unset-key\\>"     . font-lock-keyword-face)
                          ("\\<kbd\\>"                  . font-lock-keyword-face)
                          ("\\<load-file\\>"            . font-lock-keyword-face)
                          ("\\<member\\>"               . font-lock-keyword-face)
                          ("\\<nil\\>"                  . font-lock-keyword-face)
                          ("\\<not\\>"                  . font-lock-keyword-face)))

(font-lock-add-keywords 'csharp-mode
                        '(("\\<init\\>" . font-lock-keyword-face)))

;; Add Ren'py-specific keywords to Python mode, but only when working with
;; Ren'py files (i.e. have the ".rpy" extension).

(add-hook 'python-mode-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.rpy\\'" buffer-file-name))
              (font-lock-add-keywords 'python-mode
                                      '(("\\(^\\|\s+\\)alpha\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)at\\>"         . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)default\\>"    . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)define\\>"     . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)hide\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)image\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)init\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)jump\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)label\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)play\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)properties\\>" . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)rotate\\>"     . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)rotate_pad\\>" . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)scene\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)screen\\>"     . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)show\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)style\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)text\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)transform\\>"  . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)use\\>"        . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)with\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xalign\\>"     . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xanchor\\>"    . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xpan\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xpos\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xtile\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)xzoom\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)yalign\\>"     . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)yanchor\\>"    . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)ypan\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)ypos\\>"       . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)ytile\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)yzoom\\>"      . font-lock-keyword-face)
                                        ("\\(^\\|\s+\\)zoom\\>"       . font-lock-keyword-face)

                                        ("\s+crop\\>"             . font-lock-builtin-face)
                                        ("\s+dissolve\\>"         . font-lock-builtin-face)
                                        ("\s+fade\\>"             . font-lock-builtin-face)
                                        ("\s+wipedown\\>"         . font-lock-builtin-face)
                                        ("\s+wipeleft\\>"         . font-lock-builtin-face)
                                        ("\s+wiperight\\>"        . font-lock-builtin-face)
                                        ("\s+wipeup\\>"           . font-lock-builtin-face)
                                        ("\\(^\\|\s+\\)linear\\>" . font-lock-builtin-face)
                                        ("\\(^\\|\s+\\)size\\>"   . font-lock-builtin-face)

                                        ("\\(^\\|\s+\\)hide\s+\\(.*\\)"   2 font-lock-variable-name-face)
                                        ("\\(^\\|\s+\\)jump\s+\\(.*\\)"   2 font-lock-variable-name-face)
                                        ("\\(^\\|\s+\\)label\s+\\(.*\\)"  2 font-lock-variable-name-face)
                                        ("\\(^\\|\s+\\)play\s+\\(\\w+\\)" 2 font-lock-variable-name-face)
                                        ("\\(^\\|\s+\\)scene\s+\\(.*\\)"  2 font-lock-variable-name-face)
                                        ("\\(^\\|\s+\\)show\s+\\(.*\\)"   2 font-lock-variable-name-face))))))

;; It's really annoying to have Emacs GUI minimized with an accidental typo :(

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; I seldom use the screen scroll shortcuts, and they're constantly getting on
;; my nerves due to accidental typos :(

(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

;; Reuse Dired buffer also when moving back up to parent folder.

(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map (kbd "^")
                       (lambda () (interactive) (find-alternate-file "..")))))

;; Grouping buffers by category in IBuffer makes my life so much easier
;; and productive :)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Agenda"    (mode . org-mode))
               ("Bash"      (mode . sh-mode))
               ("C"         (mode . c-mode))
               ("C++"       (mode . c++-mode))
               ("C#"        (mode . csharp-mode))
               ("CMake"     (mode . cmake-mode))
               ("Docker"    (or
                             (mode . dockerfile-mode)
                             (name . "^\\*?docker")))
               ("Elisp"     (mode . emacs-lisp-mode))
               ("JSON"      (or
                             (mode . json-mode)
                             (name . "\\.json")))
               ("MSBuild"   (or
                             (name . "\\.??proj")
                             (name . "\\.props")
                             (name . "\\.targets")))
               ("Python"    (mode . python-mode))
               ("Ruby"      (or
                             (mode . ruby-mode)
                             (mode . inf-ruby-mode)))
               ("Text"      (mode . text-mode))
               ("XML"       (name . "\\.xml"))
               ("YAML"      (mode . yaml-mode))
               ("Terminals" (mode . term-mode))
               ("Dired"     (mode . dired-mode))
               ("Magit"     (name . "magit*"))
               ("TAGS"      (or
                             (mode . tags-table-mode)
                             (name . "^\\*Tags List\\*$")))
               ("Emacs"     (or
                             (mode . Buffer-menu-mode)
                             (mode . grep-mode)
                             (mode . help-mode)
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Backtrace\\*$")
                             (name . "^\\*Completions\\*$")
                             (name . "^\\*GNU Emacs\\*$")
                             (name . "^\\*Messages\\*$")
                             (name . "^\\*Packages\\*$")
                             (name . "^\\*Warnings\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; However, I prefer to only see the categories that have buffers open.

(setq-default ibuffer-show-empty-filter-groups nil)

;; I also prefer to have the human-readable notations for buffer sizes, rather than
;; all bytes-only.

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.2f MB" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000)    (format "%7.2f KB" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only
              " "
              (name 20 20 :left :elide)
              " "
              (size-h 11 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#282b33" "#e1c1ee" "#5b94ab" "#cfcf9c" "#819cd6" "#a6c1e0" "#7289bc" "#c6c6c6"])
 '(column-number-mode t)
 '(cursor-type 'box)
 '(custom-enabled-themes '(deeper-blue))
 '(display-time-mode t)
 '(frame-background-mode 'dark)
 '(menu-bar-mode t)
 '(package-selected-packages
   '(docker json-mode inf-ruby org-bullets evil tool-bar+ dot-mode multiple-cursors csharp-mode julia-mode doom-themes wrap-region vimrc-mode transpose-frame vscode-dark-plus-theme vs-light-theme vs-dark-theme github-dark-vscode-theme lua-mode magit yaml-mode cmake-mode dockerfile-mode twilight-anti-bright-theme badwolf-theme clues-theme soothe-theme flatui-dark-theme subatomic-theme tangotango-theme afternoon-theme kaolin-themes gruber-darker-theme alect-themes apropospriate-theme ample-theme cyberpunk-theme moe-theme material-theme dracula-theme gruvbox-theme monokai-theme spacemacs-theme color-theme-modern color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized zenburn-theme treemacs))
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode t)
 '(warning-suppress-types '((comp) (comp) (comp)))
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:foreground "#555753"))))
 '(term-color-blue ((t (:foreground "#729FCF"))))
 '(term-color-cyan ((t (:foreground "#34E2E2"))))
 '(term-color-green ((t (:foreground "#8AE234"))))
 '(term-color-magenta ((t (:foreground "#AD7FA8"))))
 '(term-color-red ((t (:foreground "#EF2929"))))
 '(term-color-white ((t (:foreground "#EEEEEC"))))
 '(term-color-yellow ((t (:foreground "#FCE94F"))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 '(term-default-fg-color ((t (:inherit term-color-white)))))

;; Advanced functionality I need.

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
