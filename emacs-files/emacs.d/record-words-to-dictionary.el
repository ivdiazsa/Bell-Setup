;; *************************
;;  Org-Mode Configuration!
;; *************************

;; Let's save our org filenames in variables for easier access and better readability.

(defvar japanese-lexicon-file "~/Documents/Emacs/jlexicon.org")
(defvar japanese-dictionary-file "~/Documents/Emacs/jdictionary.org")

;; Since we want to output our new words to both files, the dictionary and the
;; lexicon, we need to somehow tell Emacs to write it down to the second file
;; after 'org-capture' takes care of the first. We will use a function for this,
;; which is described and defined later, but we need a buffer to store the
;; word's information temporarily.

(setq-default word-capture-buffer '())

;; *********************************
;;  New Item Data Prompt Functions!
;; *********************************

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

(defun get-word-written-form (spelling-type)
  "Prompt for the word's writing. This function is usually called twice per word
capture: One for kanji+hiragana or katakana, and one for hiragana-only."
  (let ((word-writing (read-string
                       (format "Enter the word's %s spelling: " spelling-type))))
    (format "%s" word-writing)))

;; Get the new word's information: Hiragana writing, Kanji writing if the word has
;; one, and the word's definition in English.

(defun get-new-word ()
  "Prompt for the word's necessary information to record it to the lexicon and
the dictionary."
  (let ((hiragana-writing (get-word-written-form "hiragana"))
        (kanji-writing (get-word-written-form "kanji"))
        (word-type (get-suru-na-if-so))
        (definition (read-string "Enter the word's definition in English: ")))

    (record-word-to-dictionary hiragana-writing kanji-writing word-type definition)
    (format "- %s%s (%s): %s\n"
            kanji-writing
            word-type
            hiragana-writing
            definition)))

;; We will be classifying our words per semantic field in which we found them. So,
;; we have to specify said field when filing the individual words' information. This
;; function asks and retrieves that information.

(defun get-word-found-place ()
  "Prompt for the source of where the given word was found, and move the cursor to
said position in the file."
  (let ((place (read-string "Enter the place you found the word at: ")))
    (find-file japanese-lexicon-file)
    (goto-char 0)
    (search-forward (format "** %s" place) nil t 1)))

;; Every time we add a new word to our lexicon, we also want it added to our dictionary,
;; so that we also have it in an easy way to look it up alphabetically... Or perhaps
;; I should say syllabarily :)

;; ALGORITHM DRAFT:
;;
;; - Open the dictionary file.
;; - Go to the very beginning of the file.
;; - Search for the specific heading. In this case, it's the first syllable of the word.
;; - If found, go the bottom of said heading, and insert the word item.
;; - If not found, add a new header for this syllable, and insert the word item there.

(defun record-word-to-dictionary (hiragana kanji nasuru english)
  "Add captured word entry to its respective place in the dictionary after being
added to the lexicon.")

;; *********************
;;  New Item Templates!
;; *********************

;; Since org-mode is for everyone, let's add the japanese dictionary capture
;; templates to emacs' "org-capture-templates", rather than setting it from
;; scratch. We don't want to delete the templates from other org-mode components
;; we might have initialized prior to this one :)
;;
;; For better study, I like grouping the words I study by place where I learned
;; them. Makes it easier to remember, and look up whenever needed :)

(add-to-list 'org-capture-templates
             '("j" "New Place of Japanese Knowledge"
               entry (file+headline japanese-lexicon-file "Japanese Learning")
               "** %^{Word Source}"
               :empty-lines 1 :jump-to-captured t)
             t)
