;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar kanji-quiz-next-terms nil
  "A list of the terms that will be shown on the next pass through the quiz.")

(defvar kanji-quiz-current-term nil
  "The current term being displayed.")

(defvar kanji-quiz-steps nil
  "A list of the steps for the current quiz.

In practice, this will be one of either ‘kanji-quiz-progression-kanji-first’
or ‘kanji-quiz-progression-english-first’.")

(defvar kanji-quiz-pages nil
  "A list of the terms remaining in the current quiz.

This does not include terms that have been ejected by ‘kanji-quiz-eject-term’.")

(defvar kanji-quiz-progression nil
  "A list of the steps remaining in the current quiz.")

(defconst kanji-quiz-size-factor 6.0
  "The factor by which the Japanese characters in this quiz will be enlarged.")

(defconst kanji-quiz-progression-kanji-first
  '(((show kanji) (hide furigana english))
    ((show furigana))
    ((show english)))
  "The steps for a kanji-first quiz.

First, show the Japanese term and hide the furigana and English definition;
then, show the furigana (if any); then, show the English definition.")

(defconst kanji-quiz-progression-english-first
  '(((show english) (hide kanji furigana))
    ((show kanji))
    ((show furigana)))
  "The steps for an English-first quiz.

First, show the English definition and hide the Japanese term and furigana;
then, show the Japanese term; then, show the furigana (if any).")

(defvar kanji-quiz-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" #'kanji-quiz-advance)
    (define-key keymap "x" #'kanji-quiz-eject-term)
    (define-key keymap "q" #'bury-buffer)
    keymap)
  "Keymap for `kanji-quiz-mode'.")

(define-derived-mode kanji-quiz-mode fundamental-mode "漢字"
  "Major mode for a kanji quiz")

(defun kanji-quiz-advance ()
  "Advance to the next step or term of the current quiz.

Retain the current term to be presented again later."
  (interactive)
  (unless (cl-loop while kanji-quiz-steps
                   thereis (/= 0 (kanji-quiz-show-and-hide (pop kanji-quiz-steps))))
    (kanji-quiz-next-term t)))

(defun kanji-quiz-eject-term ()
  "Advance to the next term in the current quiz.

The current term will not be presented again.  If no terms would remain,
present a message to that effect instead."
  (interactive)
  (if (and (null kanji-quiz-pages) (null kanji-quiz-next-terms))
      (message "No more terms")
    (kanji-quiz-next-term nil)))

(defun kanji-quiz-next-term (save-current-term)
  "Advance to the next term; retain the current term is SAVE-CURRENT-TERM is not nil."
  (when (and save-current-term kanji-quiz-current-term)
    (push kanji-quiz-current-term kanji-quiz-next-terms))
  (when (null kanji-quiz-pages)
    (setq kanji-quiz-pages (kanji-quiz-shuffle kanji-quiz-next-terms))
    (setq kanji-quiz-next-terms nil))
  (setq kanji-quiz-current-term (pop kanji-quiz-pages))
  (setq kanji-quiz-steps kanji-quiz-progression)
  (widen)
  (while (zerop (kanji-quiz-show-and-hide (pop kanji-quiz-steps))))
  (goto-char (cdar (alist-get 'english kanji-quiz-current-term)))
  (narrow-to-page)
  (recenter-top-bottom -1))

(defun kanji-quiz-show-and-hide (actions)
  "Show and/or hide text in the currently displayed term according to ACTIONS.

ACTIONS should be a list of one or two lists, each consisting of the symbol
‘show’ or ‘hide’ and followed by one or more of the symbols ‘kanji’, ‘furigana’,
or ‘english’.  The total number of changes performed is returned.  In practice,
this will be zero if only a single action referring to furigana is present, and
the current term has no associated furigana.  Otherwise, a positive number is
returned."
  (cl-loop with inhibit-read-only = t
    for (action . labels) in actions
    sum (cl-loop with color = (face-attribute 'default (if (eq action 'show) :foreground :background))
          for label in labels
          sum (cl-loop with positions = (alist-get label kanji-quiz-current-term)
                for (start . end) in positions
                do (put-text-property start end 'face `(:foreground ,color))
                finally return (length positions)))))

(defun kanji-quiz-parse-term (limit)
  "Parse the kanji quiz term at point, to a maximum buffer position of LIMIT.

The format for a quiz term is as follows:

- a Japanese term occupying exactly one line;
- an optional list of furigana strings on one line, separated by any combination
  of whitespace-syntax characters or the Unicode character IDEOGRAPHIC SPACE
  (U+3000); and
- an English definition occupying one or more lines, terminated by two or more
  newline characters.

A three-element list is returned, containing the Japanese term as a string; a list
of furigana characters equal in length to the number of kanji characters in the term,
or nil if there aren't any; and the English term as a single string with embedded
newline characters.

An error is raised if the furigana count does not match the count of kanji characters
in the term, or if the definition is missing."
  (let ((lines (cl-loop while (re-search-forward (rx point (group (+ nonl)) (? ?\n)) limit t)
                 collect (cons (cons (match-beginning 1) (match-end 1)) (match-string-no-properties 1)))))
    (re-search-forward (rx point (* ?\n)) limit t)
    (when (cdr lines)
      (let* ((term (cdar lines))
             (kanji-count (how-many (rx (category chinese-two-byte)) (caaar lines) (cdaar lines))))
        (cond
         ((zerop kanji-count)
          (list term nil (string-join (mapcar #'cdr (cdr lines)) "\n")))
         ((null (cddr lines))
          (throw 'kanji-quiz-format (cons "Missing definition" (caaadr lines))))
         ((/= kanji-count (how-many (rx (+ (not (any space "　")))) (caaadr lines) (cdaadr lines)))
          (throw 'kanji-quiz-format (cons "Furigana count does not match kanji count" (caaadr lines))))
         (t
          (list term (split-string (cdadr lines) (rx (+ (| (syntax whitespace) "　")))) (string-join (mapcar #'cdr (cddr lines)) "\n"))))))))

(defun kanji-quiz-shuffle (list)
  "Return a copy of LIST with its elements randomly shuffled."
  (cl-loop with shuffled = (vconcat list)
           for i from (length shuffled) downto 2
           do (cl-rotatef (elt shuffled (random i)) (elt shuffled (1- i)))
           finally return (append shuffled nil)))

(defun kanji-quiz-region ()
  "Return the bounds of the region which should be parsed for quiz terms.

If the region is active, return its bounds; otherwise, if there is a prefix
argument, return the bounds from point to that many paragraphs beyond point;
otherwise, return the bounds from point to the end of the buffer."
  (cond
   ((region-active-p)
    (list (min (point) (mark)) (max (point) (mark))))
   (current-prefix-arg
    (let ((end (save-excursion (forward-paragraph (prefix-numeric-value current-prefix-arg)) (point))))
      (list (min (point) end) (max (point) end))))
   (t
    (list (point) (point-max)))))

(defun kanji-quiz-start-english-first (start end)
  "Start a quiz where the English definition is presented first.

Interactively, the START and END arguments are supplied by
‘kanji-quiz-region’."
  (interactive (kanji-quiz-region))
  (kanji-quiz-start start end kanji-quiz-progression-english-first))

(defun kanji-quiz-start-kanji-first (start end)
  "Start a quiz where the Japanese term is presented first.

Interactively, the START and END arguments are supplied by
‘kanji-quiz-region’."
  (interactive (kanji-quiz-region))
  (kanji-quiz-start start end kanji-quiz-progression-kanji-first))

(defun kanji-quiz-populate-quiz-buffer (next-term)
  "Populate the current buffer with quiz terms supplied by the function NEXT-TERM.

NEXT-TERM will be called repeatedly, without arguments.  Each
invocation should return a quiz term in the format emitted by
‘kanji-quiz-parse-term’, or nil, signifying that no more terms
are available.

An alist will be returned with the following symbolic keys:

- ‘term’: the Japanese term
- ‘furigana’: a list of buffer positions for the term's furigana, if any,
  in the form (start . end)
- ‘kanji’: a list containing a single cons cell containing the buffer
  positions of the start and end of the term's Japanese text
- ‘english’: a list containing a single cons cell containing the
  buffer positions of the start and end of the term's English definition"
  (cl-loop
   with background = (face-attribute 'default :background)
   for (term furigana definition) = (funcall next-term)
   while term
   collect
   (let ((line (propertize (concat term "　") 'display `(height ,kanji-quiz-size-factor)))
         (p (point))
         (furigana-pos nil))
     (save-excursion
       (insert line "\n" line "\n"))
     (while (re-search-forward (rx (category chinese-two-byte)) (line-end-position) t)
       (put-text-property p (point) 'face `(:foreground ,background))
       (let* ((start (point))
              (next-furigana (pop furigana)))
         (replace-match
          (propertize
           (concat "　" next-furigana "　")
           'display
           `(height ,(/ kanji-quiz-size-factor (+ 2 (length next-furigana))))))
         (push (cons start (point)) furigana-pos))
       (setq p (point)))
     (put-text-property p (line-end-position) 'face `(:foreground ,background))
     (forward-line 2)
     (let* ((kanji-pos (cons (line-beginning-position 0) (line-end-position 0)))
            (english-pos (cons (point) (progn (insert definition "\n") (point)))))
       (insert "\n\f\n")
       (list (cons 'term term)
             (cons 'furigana furigana-pos)
             (cons 'kanji (list kanji-pos))
             (cons 'english (list english-pos)))))))

(defun kanji-quiz-start (start end progression)
  "Create a kanji quiz buffer, populate it with terms, and switch to it.

Terms are parsed from the current buffer, from buffer positions START to END.
PROGRESSION is a list of steps to follow for each term; in practice this will
be one of `kanji-quiz-progression-kanji-first' or ‘kanji-quiz-progression-english-first’.

The new buffer will always be named \"*kanji-quiz*\" and will be placed into
mode ‘kanji-quiz-mode’.  The supplied terms will be randomized and the first
one displayed."
  (let ((terms-buffer (current-buffer))
        (terms-point (point)))
    (goto-char start)
    (switch-to-buffer (get-buffer-create "*kanji-quiz*"))
    (kanji-quiz-mode)
    (setq-local
     kanji-quiz-next-terms
     (let ((inhibit-read-only t))
       (erase-buffer)
       (kanji-quiz-populate-quiz-buffer
        (lambda () (with-current-buffer terms-buffer (kanji-quiz-parse-term end))))))
    (with-current-buffer terms-buffer (goto-char terms-point))
    (setq-local kanji-quiz-pages nil)
    (setq-local kanji-quiz-steps nil)
    (setq-local kanji-quiz-current-term nil)
    (setq-local kanji-quiz-progression progression)
    (kanji-quiz-advance)))
