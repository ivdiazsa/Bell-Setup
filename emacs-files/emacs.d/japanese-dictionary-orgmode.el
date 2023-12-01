;; *************************
;;  Org-Mode Configuration!
;; *************************

;; Let's save our org filenames in variables for easier access and better readability.

(defvar japanese-dictionary-file "~/Documents/Emacs/jisho.org")

;; *********************
;;  New Item Templates!
;; *********************

;; We will be classifying our work items per area/lane of work, so we will have
;; to specify it when filing the individual work items/tasks. This function asks
;; and retrieves that information.

(defun get-word-found-place ()
  "Prompt for the source of where the given word was found."
  (let ((place (read-string "Enter the place you found the word at: ")))
    (find-file japanese-dictionary-file)
    (goto-char 0)
    (search-forward (format "** %s" place) nil t 1)))

;; 'Suru' verbs and 'Na' adjectives use said suffixes to be identified. However,
;; they are only written with them in certain cases. Therefore, we want to write
;; them inside parentheses to show it's not an always used thing, or nothing at
;; all for the case of normal words.

(defun get-suru-na-if-so ()
  "Ask whether the current word is a 'suru' verb, 'na' adjective, or a normal word,
and return its suffix if needed."

  (let ((word-type (read-answer "Is Na-Adjective, Suru-Verb, or other? "
                                '(("na"   nil "Na-adjective")
                                  ("suru" nil "Suru-verb")
                                  (""     nil "Other word")))))

    (cond ((string-equal word-type "na")   "(な)")
          ((string-equal word-type "suru") "(する)")
          ((string-equal word-type "")     ""))))

;; Most words have at least a kanji in their way of writing them. We also want to
;; record the hiragana-only writing to be able to easily review how they are
;; pronounced, since memorizing kanji's pronunciations is... not gonna happen.

(defun get-hiragana-if-has-kanji ()
  "Prompt for the word's hiragana-only writing if it has kanji on it. If not, then
leave this field blank."
  (let ((hiragana-writing (read-string "Enter the word's full hiragana spelling: ")))
    (if (string-equal hiragana-writing "")
        ""
      (format "(%s)" hiragana-writing))))

(defun get-group-of-words-with-example ()
  "Prompt for a set of words with their respective meanings, as well as the example
sentence at the end. It's like calling the 'New Japanese Word' org-capture template
n-times, followed by one of the 'Example Using Japanese Words' one."
  (setq result "")
  (setq continue "yes")

  (while (equal continue "yes")
    (let ((word-type (get-suru-na-if-so))
          (hiragana-writing (get-hiragana-if-has-kanji))
          (kanji-writing (read-string "Enter the word's kanji spelling: "))
          (definition (read-string "Enter the word's definition in English: ")))

      (let ((jisho-entry (format "- %s%s %s: %s\n"
                                 kanji-writing
                                 word-type
                                 hiragana-writing
                                 definition)))
        (setq result (concat result jisho-entry))))

    (setq continue (read-answer "Add another word? "
                                '(("yes" nil "Another word")
                                  ("no"  nil "Continue to Example Sentence")))))

  (let ((want-sentence (read-answer "Do you wish to add an example sentence? "
                                    '(("yes" nil "Add sentence")
                                      ("no"  nil "End this prompt and send it to org-mode")))))

    (when (equal want-sentence "yes")
      (let ((japanese-sentence (read-string "Enter the example sentence in Japanese: "))
            (english-sentence (read-string "Enter the example sentence in English: ")))

        (let ((full-example (format "- *%s -> %s.*\n"
                                    japanese-sentence
                                    english-sentence)))
          (setq result (concat result full-example))))))

  (message "Thanks for stopping by!")
  (format "%s" result))

;; Since org-mode is for everyone, let's add the japanese dictionary capture
;; templates to emacs' "org-capture-templates", rather than setting it from
;; scratch. We don't want to delete the templates from other org-mode components
;; we might have initialized prior to this one :)
;;
;; For better study, I like grouping the words I study by place where I learned
;; them. Makes it easier to remember, and look up whenever needed :)

(add-to-list 'org-capture-templates
 '("j" "New Place of Japanese Knowledge"
   entry (file+headline japanese-dictionary-file "Japanese Learning")
   "** %^{Word Source}"
   :empty-lines 1 :immediate-finish :jump-to-captured)
 t)

;; Template to easily write down the word I found with both, Kanji and Hiragana
;; writings, as well as its definition without dealing with fixing weird spacing
;; and all that stuff.

(add-to-list 'org-capture-templates
 '("w" "New Japanese Word"
   item (file+function japanese-dictionary-file get-word-found-place)
   "%^{With Kanji if has}%(get-suru-na-if-so) %(get-hiragana-if-has-kanji): %^{Definition}"
   :empty-lines 0 :immediate-finish :jump-to-captured)
 t)

;; Template to easily write an example sentence in Japanese, with its respective
;; translation to English.

(add-to-list 'org-capture-templates
 '("e" "Example Using Japanese Words"
   item (file+function japanese-dictionary-file get-word-found-place)
   "*%^{Sentence in Japanese} -> %^{Sentence in English}*"
   :empty-lines-after 1 :immediate-finish :jump-to-captured)
 t)

;; Template that combines and packs the new Japanese word and sentence templates.
;; It prompts for as many words as the user needs, and then for an optional
;; example sentence.

(add-to-list 'org-capture-templates
 '("p" "Set of Words With an Optional Example"
   item (file+function japanese-dictionary-file get-word-found-place)
   #'get-group-of-words-with-example
   :empty-lines 1 :immediate-finish :jump-to-captured)
 t)
