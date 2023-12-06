;; *************************
;;  Org-Mode Configuration!
;; *************************

;; Let's save our org filenames in variables for easier access and better readability.

(defvar dotnet-work-todos-file "~/Documents/Emacs/dotnet-todos.org")

;; Setting priorities to be with numbers.

(setq org-highest-priority 0)
(setq org-lowest-priority 3)
(setq org-default-priority 2)

;; *********************
;;  New Item Templates!
;; *********************

;; Since org-mode is for everyone, let's add the japanese dictionary capture
;; templates to emacs' "org-capture-templates", rather than setting it from
;; scratch. We don't want to delete the templates from other org-mode components
;; we might have initialized prior to this one :)

(add-to-list 'org-capture-templates
             '("i" "New Work Item"
               entry (file+headline dotnet-work-todos-file "Work Items")
               "** NEW ITEM [#2] %^{Job Item Title}\n\
%^g\n\
:Created: %<%Y/%m/%e %l:%M %P %Z>\n\n\
%^{Job Item Description}\n\n\
*- Repository: %^{Repository Name (what comes after github.com/)}*\n\
*- Issue Number: %^{Issue Number}*\n\
*- Link: [[https://github.com/%\\3/issues/%\\4]]*\n\n"
               :empty-lines 1 :immediate-finish :jump-to-captured)
             t)

;; **************
;;  Item States!
;; **************

(setq org-todo-keywords
      '((sequence "New Item"
                  "In Progress(!)"
                  "Researching(@/!)"
                  "Backlogged(@/!)"
                  "In Review(!)"
                  "Blocked(@/!)"
                  "Warning(@/!)"
                  "Complete(!)"
                  "Discarded(@/!)")))

;; ****************
;;  States Colors!
;; ****************

(setq org-todo-keyword-faces
      '(("New Item"    . (:foreground "#005EB8" :weight bold))
        ("In Progress" . (:foreground "#3BD1B7" :weight bold))
        ("Researching" . (:foreground "#FB7306" :weight bold))
        ("Backlogged"  . (:foreground "#A348C8" :weight bold))
        ("In Review"   . (:foreground "#F1C232" :weight bold))
        ("Blocked"     . (:foreground "#D13B55" :weight bold))
        ("Warning"     . (:foreground "#FF1100" :weight bold))
        ("Complete"    . (:foreground "#19D22A" :weight bold))
        ("Discarded"   . (:foreground "#A7A7A7" :weight bold))))

;; ************
;;  Item Tags!
;; ************

(setq org-tag-list
      '(("Infrastructure" . nil)))

;; **************
;;  Tags Colors!
;; **************

(setq org-tag-faces
      '(("Infrastructure" . (:foreground "#00AF99" :weight bold))))
