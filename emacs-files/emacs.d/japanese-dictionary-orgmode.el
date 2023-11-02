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

;; Since org-mode is for everyone, let's add the japanese dictionary capture
;; templates to emacs' "org-capture-templates", rather than setting it from
;; scratch. We don't want to delete the templates from other org-mode components
;; we might have initialized prior to this one :)
;;
;; For better study, I like grouping the words I study by place where I learned
;; them. Makes it easier to remember, and look up whenever needed :)

(push
 '("j" "New Place of Japanese Knowledge"
   entry (file+headline japanese-dictionary-file "Japanese Learning")
   "** %^{Word Source}"
   :empty-lines 1 :immediate-finish :jump-to-captured)
 org-capture-templates)

;; Template to easily write down the word I found with both, Kanji and Hiragana
;; writings, as well as its definition without dealing with fixing weird spacing
;; and all that stuff.

(push
 '("w" "New Japanese Word"
   item (file+function japanese-dictionary-file get-word-found-place)
   "%^{With Kanji if has}%(get-suru-na-if-so) %(get-hiragana-if-has-kanji): %^{Definition}"
   :empty-lines 0 :immediate-finish :jump-to-captured)
 org-capture-templates)

;; Template to easily write an example sentence in Japanese, with its respective
;; translation to English.
;;
;; ENHANCEME: Right now, the sentence is appended at the end of the section.
;;            Make it so it's appended at the end of the section where the given
;;            word is found.

(push
 '("e" "Example Using Japanese Words"
   item (file+function japanese-dictionary-file get-word-found-place)
   "*%^{Sentence in Japanese} -> %^{Sentence in English}*"
   :empty-lines-after 1 :immediate-finish :jump-to-captured)
 org-capture-templates)