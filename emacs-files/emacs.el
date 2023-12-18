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

(load-file "~/.emacs.d/my-org-mode-config.el")

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
(add-to-list 'auto-mode-alist '("\\.rpy\\'" . python-mode))

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

(defvar boundary-symbols '(45 46 47 58 59 92 95))
(defvar opening-symbols '(40 60))
(defvar closing-symbols '(41 62))
(defvar whitespaces '(9 10 11 12 32))

;; Getter function to know whether a character is a word boundary defined by us.

(defun is-conservative-boundary (character)
  "Return 't' if CHARACTER is in one of the conservative boundary symbols lists,
or 'nil' otherwise."
  (or (member character boundary-symbols)
      (member character opening-symbols)
      (member character closing-symbols)
      (member character whitespaces)))

;; Getter function to know whether a character is an opening symbol defined by us.

(defun is-opening-symbol (character)
  "Return 't' if CHARACTER is in the 'opening-symbols' list, or 'nil' otherwise."
  (if (member character opening-symbols) t nil))

;; Getter function to know whether a character is a closing symbol defined by us.

(defun is-closing-symbol (character)
  "Return 't' if CHARACTER is in the 'closing-symbols' list, or 'nil' otherwise."
  (if (member character closing-symbols) t nil))

;; Getter function to know whether a character is an encapsulating bracket one
;; defined by us.

(defun is-bracket-symbol (character)
  "Return 't' if CHARACTER is in either the 'opening-symbols' or 'closing-symbols'
lists, or 'nil' otherwise."
  (if (or (is-opening-symbol character)
          (is-closing-symbol character))
      t nil))

;; Getter function to know whether a character is a whitespace one defined by us.

(defun is-whitespace (character)
  "Return 't' if CHARACTER is in the 'whitespaces' list, or 'nil' otherwise."
  (if (member character whitespaces) t nil))

;; Getter function to know whether a character is a spacing delimiter defined
;; by us (i.e. space or tab).

(defun is-spacing-delimiter (character)
  "Return 't' if CHARACTER is a space or a tab, or 'nil' otherwise."
  (or (equal 9 character)
      (equal 32 character)))

;; Function to easily toggle between absolute/normal and relative line numbering.

(defun toggle-line-numbers-type ()
  "Toggle between absolute and relative line numbering."
  (if (not (equal display-line-numbers-type 'relative))
      (setq-default display-line-numbers-type 'relative)
    (setq-default display-line-numbers-type 'absolute))
  (display-line-numbers-mode 1))

;; Function to jump forward between words without ignoring symbols-only words.

(defun forward-word-conservative (arg)
  "Move forward to the next word conservatively. In this context, conservatively
means to stop at the next boundary or word start character (i.e. 'words' made up
of just symbols also count as words). With argument ARG, do this that many times."
  (interactive "p")

  ;; A negative arg means we want to move backward, so we call the conservative
  ;; backwards word friend function.
  (if (< arg 0)
      (backward-word-conservative (* arg -1))
    (dotimes (number arg)

      (let ((start (char-after)))
        ;; If we begin on a normal text character, then first, we need to find its
        ;; boundary to define where we should go next.
        (while (not (is-conservative-boundary (char-after)))
          (forward-char))

        (cond (;; If we began at a section breaking character (e.g. newline), then
               ;; our next word starts at the first non-whitespace character.
               (and (is-whitespace (char-after))
                    (not (is-spacing-delimiter start)))
               (while (is-whitespace (char-after))
                 (forward-char)))

              ;; If we encounter a bracket symbol, then our next stop is the end of
              ;; that chain of brackets.
              ((is-bracket-symbol (char-after))
               (while (is-bracket-symbol (char-after))
                 (forward-char)))

              ;; If we encounter any other boundary symbol (e.g. hyphens, underscores),
              ;; then our next stop is what comes after them.
              (t (while (and (is-conservative-boundary (char-after))
                             (not (is-whitespace (char-after))))
                   (forward-char))))

        ;; If we began on any character other than section boundaries, and we ended
        ;; up on a space after all the movements in the previous 'cond' statement,
        ;; then we continue until we find our next word or the end of the current line.
        (unless (and (is-whitespace start)
                     (not (is-spacing-delimiter start)))
          (while (is-spacing-delimiter (char-after))
            (forward-char)))))))

;; Function to jump backward between words without ignoring symbols-only words.

(defun backward-word-conservative (arg)
  "Move backward to the previous word conservatively. In this context, conservatively
means to stop at the previous boundary or word start character (i.e. 'words' made up
of just symbols also count as words). With argument ARG, do this that many times."
  (interactive "p")
  (if (< arg 0)
      (forward-word-conservative (* arg -1))
    (dotimes (number arg)

      (cond ((is-whitespace (char-before))
             (backward-char)
             (while (is-whitespace (char-after))
               (backward-char)))

            ((is-bracket-symbol (char-after))
             (while (is-bracket-symbol (char-after))
               (backward-char)))

            ((is-conservative-boundary (char-before))
             (while (and (is-conservative-boundary (char-before))
                         (not (is-whitespace (char-before))))
               (backward-char))))

      (while (not (is-conservative-boundary (char-before)))
        (backward-char)))))

      ;; Normal Case:
      ;; forw*ard-char
      ;; forward char
      ;; forward(char)
      ;; forw*ard(((((((char))))
      ;; forw*ard (char)

      ;; Starting Word With Boundary Character Case:
      ;; other s*tuff  _testing
      ;; other-s*tuff ___lolpol

      ;; Other Cases
      ;; thing*s()(((___ test-lol))))
      ;;;;; thing*s()(((  ___ test-lol))))
      ;; text goes he*re (((((more text )))))continuing-(without-spaces

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
(global-set-key (kbd "M-f") 'forward-word-conservative)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word-conservative)
(global-set-key (kbd "M-B") 'backward-word)

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
                        '(("add-hook"             . font-lock-keyword-face)
                          ("add-to-list"          . font-lock-keyword-face)
                          ("custom-set-faces"     . font-lock-keyword-face)
                          ("custom-set-variables" . font-lock-keyword-face)
                          ("define-key"           . font-lock-keyword-face)
                          ("eql"                  . font-lock-keyword-face)
                          ("equal"                . font-lock-keyword-face)
                          ("global-set-key"       . font-lock-keyword-face)
                          ("global-unset-key"     . font-lock-keyword-face)
                          ("kbd"                  . font-lock-keyword-face)
                          ("load-file"            . font-lock-keyword-face)
                          ("nil"                  . font-lock-keyword-face)
                          ("not"                  . font-lock-keyword-face)))

(font-lock-add-keywords 'csharp-mode
                        '(("init" . font-lock-keyword-face)))

;; Add Ren'py-specific keywords to Python mode, but only when working with
;; Ren'py files (i.e. have the ".rpy" extension).

(add-hook 'python-mode-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.rpy\\'" buffer-file-name))
              (font-lock-add-keywords 'python-mode
                                      '(("default"    . font-lock-keyword-face)
                                        ("define"     . font-lock-keyword-face)
                                        ("hide"       . font-lock-keyword-face)
                                        ("image"      . font-lock-keyword-face)
                                        ("init"       . font-lock-keyword-face)
                                        ("jump"       . font-lock-keyword-face)
                                        ("label"      . font-lock-keyword-face)
                                        ("play"       . font-lock-keyword-face)
                                        ("properties" . font-lock-keyword-face)
                                        ("scene"      . font-lock-keyword-face)
                                        ("screen"     . font-lock-keyword-face)
                                        ("show"       . font-lock-keyword-face)
                                        ("style"      . font-lock-keyword-face)
                                        ("text"       . font-lock-keyword-face)
                                        ("use"        . font-lock-keyword-face))))))

;; It's really annoying to have Emacs GUI minimized with an accidental typo :(

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

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
 '(beacon-color "#d54e53")
 '(column-number-mode t)
 '(cursor-type 'box)
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("49cd634a5d2e294c281348ce933d2f17c19531998a262cbdbe763ef2fb41846b" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "922f930fc5aeec220517dbf74af9cd2601d08f8250e4a15c385d509e22629cac" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "47d5324dac28a85c1bb84b4c1dc3a8dc407cc7369db6e30d3244b16232b1eec4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "ff8be9ed2696bf7bc999423d909a603cb23a9525bb43135c0d256b0b9377c958" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "7de92d9e450585f9f435f2d9b265f34218cb235541c3d0d42c154bbbfe44d4dd" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "c2bce71b37ffd6e95fbd3b98d6eaadd113ec308f85149cfc8f50dee716764fed" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "19b1140ebd62eb216a71b3e7784a260fb4dd893359a172e86558b3328f281400" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(display-time-mode t)
 '(emms-mode-line-icon-color "#1ba1a1")
 '(exwm-floating-border-color "#413a3a")
 '(fci-rule-color "#615959")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-tail-colors ((("#38372d") . 0) (("#343735") . 20)))
 '(hl-sexp-background-color "#1c1f26")
 '(ispell-dictionary nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#131313" "#f9cc6c"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131313" "#adda78"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131313" "#5b5353"))
 '(menu-bar-mode t)
 '(objed-cursor-color "#fd6883")
 '(package-selected-packages
   '(docker json-mode inf-ruby org-bullets evil tool-bar+ dot-mode multiple-cursors csharp-mode julia-mode doom-themes wrap-region vimrc-mode transpose-frame vscode-dark-plus-theme vs-light-theme vs-dark-theme github-dark-vscode-theme lua-mode magit yaml-mode cmake-mode dockerfile-mode twilight-anti-bright-theme badwolf-theme clues-theme soothe-theme flatui-dark-theme subatomic-theme tangotango-theme afternoon-theme kaolin-themes gruber-darker-theme alect-themes apropospriate-theme ample-theme cyberpunk-theme moe-theme material-theme dracula-theme gruvbox-theme monokai-theme spacemacs-theme color-theme-modern color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized zenburn-theme treemacs))
 '(pdf-view-midnight-colors (cons "#fff1f3" "#2c2525"))
 '(pos-tip-background-color "#DEDAD5")
 '(pos-tip-foreground-color "#4b5254")
 '(rustic-ansi-faces
   ["#2c2525" "#fd6883" "#adda78" "#f9cc6c" "#85dacc" "#85dacc" "#85dacc" "#fff1f3"])
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode t)
 '(vc-annotate-background "#2c2525")
 '(vc-annotate-color-map
   (list
    (cons 20 "#adda78")
    (cons 40 "#c6d574")
    (cons 60 "#dfd070")
    (cons 80 "#f9cc6c")
    (cons 100 "#f7b76d")
    (cons 120 "#f4a26e")
    (cons 140 "#f38d70")
    (cons 160 "#cea68e")
    (cons 180 "#a9c0ad")
    (cons 200 "#85dacc")
    (cons 220 "#adb4b3")
    (cons 240 "#d58e9b")
    (cons 260 "#fd6883")
    (cons 280 "#d46276")
    (cons 300 "#ac5d6a")
    (cons 320 "#83585f")
    (cons 340 "#615959")
    (cons 360 "#615959")))
 '(vc-annotate-very-old-color nil)
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
