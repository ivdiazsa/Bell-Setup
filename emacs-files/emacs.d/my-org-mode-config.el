;; *************************
;;  Org-Mode Configuration!
;; *************************

;; It looks nicer when we just have the bolds, italics, etc, without the special
;; symbols that tell Emacs to use them :)

(setq org-hide-emphasis-markers t)

;; Indented items look nicer and are easier to read :)

(setq org-indent-mode t)
(add-hook 'org-mode-hook 'org-indent-mode)

;; Let's save our org filenames in variables for easier access and better readability.

(defvar work-todos-file "~/Documents/my-todos.org")

;; Setting priorities to be with numbers.

(setq org-highest-priority 0)
(setq org-lowest-priority 3)
(setq org-default-priority 2)

;; *********************
;;  New Item Templates!
;; *********************

;; We will be classifying our work items per area/lane of work, so we will have
;; to specify it when filing the individual work items/tasks. This function asks
;; and retrieves that information.

(defun get-work-item-category ()
  "Prompt the user for the type of work item to be filed (category), and then
look for it in the org file. If found, the work item filled template will be
inserted there."
  (let ((category (read-string "Enter Work Item Category: ")))
    (find-file work-todos-file)
    (goto-char 0)
    (search-forward (format "** %s" category) nil t 1)))

;;

(define-skeleton pr-template
  "Prompt the user for the PR's number and repository to add it to the current
work item."
  > "*- Pull Request Number: " (setq pr (skeleton-read "Enter the PR's Number: ")) "*" \n
  > "*- Link: [[https://github.com/" (skeleton-read "Enter the Repository's Name: ")
  "/pull/" pr "]]*")

;; List containing all the templates for the work items and general org layouts.

(setq org-capture-templates
      '(
        ;; Work Items are classified by categories, depending on what lane of work
        ;; they cover.

        ("c" "New Item Category"
         entry (file+headline work-todos-file "Work Items")
         "** %^{Items Category}"
         :empty-lines-before 1 :immediate-finish :jump-to-captured)

        ;; The main template for Work Items. It requires the category of the
        ;; item, a title to identify it, the Github repository, Issue number,
        ;; and a brief description explaining what happened, what is the
        ;; work item's motivation, and so on.

        ("t" "New Work Item"
         entry (file+function work-todos-file get-work-item-category)
         "*** Not Started [#2] %^{Job Item Title}\n\n\
*- Repository: %^{Repository Name (what goes after github.com/)}*\n\
*- Issue Number: %^{Issue Number}*\n\
*- Link: [[https://github.com/%\\2/issues/%\\3]]*\n\n\
:Created: %<%Y/%m/%e %l:%M %P %Z>\n\n\
%^{Description}\n"
         :empty-lines-before 3 :immediate-finish :jump-to-captured)))

;; **************
;;  Item States!
;; **************

(setq org-todo-keywords
      '((sequence "Not Started(t)"
                  "Active(a!)"
                  "Inactive(i@/!)"
                  "Researching(r@/!)"
                  "Blocked(b@/!)"
                  "Waiting(w@/!)"
                  "Warning(e@/!)"
                  "|"
                  "Complete(c!)"
                  "Backlogged(l@/!)"
                  "Discarded(d@/!)"
       ))
)

;; ****************
;;  States Colors!
;; ****************

(setq org-todo-keyword-faces
      '(
        ("Not Started" . (:foreground "#00CDCD" :weight bold))
        ("Active"      . (:foreground "#3BD1B7" :weight bold))
        ("Inactive"    . (:foreground "#005EB8" :weight bold))
        ("Researching" . (:foreground "#FB7306" :weight bold))
        ("Blocked"     . (:foreground "#A348C8" :weight bold))
        ("Waiting"     . (:foreground "#C8A347" :weight bold))
        ("Warning"     . (:foreground "#FF1100" :weight bold))
        ("Complete"    . (:foreground "#19D22A" :weight bold))
        ("Backlogged"  . (:foreground "#919191" :weight bold))
        ("Discarded"   . (:foreground "#D65073" :weight bold))
       )
)
