;; *************************
;;  Org-Mode Configuration!
;; *************************

;; Let's save our org filenames in variables for easier access and better readability.

(defvar dotnet-work-todos-file "~/Documents/Emacs/dotnet-todos.org")

;; Setting priorities to be with numbers.

(setq org-highest-priority 0)
(setq org-lowest-priority 3)
(setq org-default-priority 2)

;; **********************************
;;  Work Item Information Functions!
;; **********************************

(defun get-work-item-checklist ()
  "Prompt for the initial planned subtasks for the current work item."
  (setq checklist "")
  (let ((continue (read-answer "Do you wish to add checklist items? "
                               '(("yes" nil "add the list of tasks")
                                 ("no"  nil "finish capturing this work item")))))

    (while (equal continue "yes")
      (let* ((task-name (read-string "Enter the next task: "))
             (has-subtasks (read-answer "Does this task have further subtasks? "
                                        '(("yes" nil "add the list of subtasks")
                                          ("no"  nil "continue adding tasks"))))
             (task-item (format "\n- [ ] %s" task-name)))

        (if (equal has-subtasks "yes")
            (setq checklist (concat checklist (format "%s [%%]\n" task-item)))
          (setq checklist (concat checklist (format "%s\n" task-item))))

        (while (equal has-subtasks "yes")
          (let* ((subtask-name (read-string "Enter the subtask: "))
                 (subtask-item (format "  - [ ] %s\n" subtask-name)))

            (setq checklist (concat checklist subtask-item)))

          (setq has-subtasks
                (read-answer "Add another subtask? "
                             '(("yes" nil "prompt for the next subtask")
                               ("no"  nil "end and move to the next task"))))))

        (setq continue (read-answer "Add another task? "
                                    '(("yes" nil "prompt for the next task")
                                      ("no"  nil "finish capturing this work item"))))))

  (if (string-equal checklist "")
      (format "- [ ] Do work item\n")
    (format "%s" checklist)))

;; *********************
;;  New Item Templates!
;; *********************

;; Since org-mode is for everyone, let's add the dotnet new work item capture
;; templates to emacs' "org-capture-templates", rather than setting it from
;; scratch. We don't want to delete the templates from other org-mode components
;; we might have initialized prior to this one :)

(add-to-list 'org-capture-templates
             '("i" "New Work Item"
               entry (file+headline dotnet-work-todos-file "Work Items")
               "** NEW ITEM [#2] %^{Job Item Title} [%]\n\
%^g\n\
:Created: %<%Y/%m/%d %l:%M %P %Z>\n\n\
%^{Job Item Description}\n\n\
*- Repository: %^{Repository Name (what comes after github.com/)}*\n\
*- Issue Number: %^{Issue Number}*\n\
*- Link: [[https://github.com/%\\3/issues/%\\4]]*\n\
%(get-work-item-checklist)\n"
               :empty-lines 1 :immediate-finish t :jump-to-captured t)
             t)

;; **************
;;  Item States!
;; **************

(setq org-todo-keywords
      '((sequence "NEW ITEM(n)"
                  "IN PROGRESS(i!)"
                  "RESEARCHING(r@/!)"
                  "BACKLOGGED(l@/!)"
                  "IN REVIEW(r!)"
                  "BLOCKED(b@/!)"
                  "WARNING(w@/!)"
                  "COMPLETE(c!)"
                  "DISCARDED(d@/!)")))

;; ****************
;;  STATES Colors!
;; ****************

(setq org-todo-keyword-faces
      '(("NEW ITEM"    . (:foreground "#005EB8" :weight bold))
        ("IN PROGRESS" . (:foreground "#3BD1B7" :weight bold))
        ("RESEARCHING" . (:foreground "#FB7306" :weight bold))
        ("BACKLOGGED"  . (:foreground "#A348C8" :weight bold))
        ("IN REVIEW"   . (:foreground "#F1C232" :weight bold))
        ("BLOCKED"     . (:foreground "#D13B55" :weight bold))
        ("WARNING"     . (:foreground "#FF1100" :weight bold))
        ("COMPLETE"    . (:foreground "#19D22A" :weight bold))
        ("DISCARDED"   . (:foreground "#A7A7A7" :weight bold))))

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
