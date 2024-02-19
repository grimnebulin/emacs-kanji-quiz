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

(defvar kanji-quiz-extra-text-function nil
  "A function which produces extra text for terms.")

(defconst kanji-quiz-size-factor 6.0
  "The factor by which the Japanese characters in this quiz will be enlarged.")

(defconst kanji-quiz-progression-kanji-first
  '(((show kanji) (hide furigana english extra))
    ((show furigana))
    ((show english)))
  "The steps for a kanji-first quiz.

First, show the Japanese term and hide the furigana and English definition;
then, show the furigana (if any); then, show the English definition.")

(defconst kanji-quiz-progression-english-first
  '(((show english) (hide kanji furigana extra))
    ((show kanji))
    ((show furigana)))
  "The steps for an English-first quiz.

First, show the English definition and hide the Japanese term and furigana;
then, show the Japanese term; then, show the furigana (if any).")

(defconst kanji-quiz-progression-all
  '(((show english kanji furigana)))
  "The single step for a \"quiz\" that shows everything at once.")

(defvar kanji-quiz-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" #'kanji-quiz-advance)
    (define-key keymap "x" #'kanji-quiz-eject-term)
    (define-key keymap "m" #'kanji-quiz-extra-text)
    (define-key keymap "q" #'bury-buffer)
    keymap)
  "Keymap for `kanji-quiz-mode'.")

(defun kanji-quiz-extra-text ()
  (interactive)
  (kanji-quiz-show-and-hide '((show extra))))

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
          sum (if-let (positions (alist-get label kanji-quiz-current-term))
                  (cl-loop for (start . end) in positions
                           do (put-text-property start end 'face `(:foreground ,color))
                           finally return (length positions))
                0))))

(defun kanji-quiz-parse-term (limit)
  "Parse the kanji quiz term at point, to a maximum buffer position of LIMIT.

The line following point is taken to be a Japanese term.

If the term contains any kanji characters, then the next line is taken to
contain furigana describing how to pronounce those characters.  The line is
split on whitespace--here including the Unicode character IDEOGRAPHIC SPACE
(U+3000)--to get a list of furigana strings, which describe how to pronounce
the kanji, in the same order.  Each furigana string may be followed by a
positive integer in parentheses, in which case that integer describes how many
kanji the furigana covers; if there is no trailing parenthesized number, the
furigana covers a single kanji.

Lines following the furigana, if present, or the term otherwise, up to the
smaller of the LIMIT buffer position and two successive newlines, are the
English description of the term.

A three-element list is returned, containing the Japanese term as a string;
a list of cons cells, one for each furigana, where the car is the furigana
text and the cdr is the number of kanji covered by the furigana; and the
English term as a single string with embedded newline characters.

An error is raised if the number of kanji characters in the term does not
match the total of the furigana cover counts, if a count of zero is provided,
or if the definition is missing."
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
         (t
          (let ((furigana (kanji-quiz-parse-furigana (cdadr lines))))
            (cond
             ((/= kanji-count (apply #'+ (mapcar #'cdr furigana)))
              (throw 'kanji-quiz-format (cons "Sum of furigana spans does not match kanji count" (caaadr lines))))
             (t
              (list term furigana (string-join (mapcar #'cdr (cddr lines)) "\n")))))))))))

(defun kanji-quiz-parse-furigana (str)
  "Parse a string STR into a list of furigana.

The string is split on whitespace characters, which for this purpose includes
the Unicode character IDEOGRAPHIC SPACE (U+3000).  Each extracted substring
is checked to see if it ends with a positive integer in parentheses, which
is the number of kanji the furigana spans.  If so, the corresponding element
of the returned list is a cons cell containing the text preceding the opening
parenthesis and the span; if not, the element is a cons cell containing
the entirety of the furigana text and the number 1."
  (mapcar
   (lambda (str)
     (if (string-match (rx "(" (group (+ (any digit))) ")" string-end) str)
         (let ((count (string-to-number (match-string 1 str))))
           (if (zerop count)
               (error "Furigana may not span zero characters")
             (cons (substring str 0 (1- (match-beginning 1))) count)))
       (cons str 1)))
   (split-string str (rx (+ (| (syntax whitespace) "　"))))))

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

(defun kanji-quiz-start-show-everything (start end)
  (interactive (kanji-quiz-region))
  (kanji-quiz-start start end kanji-quiz-progression-all))

(defun kanji-quiz-buffer-substring-width (start end)
  "Sum the widths of all characters in the buffer from START to END."
  (cl-loop for pos from start to (1- end)
           sum (aref (aref (font-get-glyphs (font-at pos) pos (1+ pos)) 0) 4)))

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
  buffer positions of the start and end of the term's English definition
- ‘extra’: a list containing a single cons cell containing the buffer
  positions of the start and end of extra text"
  (cl-loop
   with background = (face-attribute 'default :background)
   for (term furigana definition) = (funcall next-term)
   while term
   collect
   (let ((line (propertize (concat term "　") 'display `(height ,kanji-quiz-size-factor)))
         (p (point))
         (furigana-pos nil))
     (save-excursion (insert line "\n" line "\n"))
     (cl-loop for (furigana-text . furigana-span) in furigana do
       (re-search-forward (format "\\cC\\{%d\\}" furigana-span) (line-end-position))
       (put-text-property p (point) 'face `(:foreground ,background))
       (let ((kanji-width (kanji-quiz-buffer-substring-width (- (point) furigana-span) (point))))
         (replace-match (concat " " furigana-text " "))
         (let* ((furigana-start (- (point) 2 (length furigana-text)))
                (furigana-width (kanji-quiz-buffer-substring-width furigana-start (point))))
           (put-text-property
            furigana-start
            (point)
            'display (list 'height (/ (* kanji-width kanji-quiz-size-factor) furigana-width)))
           (push (cons furigana-start (point)) furigana-pos)))
       (setq p (point)))
     (put-text-property p (line-end-position) 'face `(:foreground ,background))
     (forward-line 2)
     (let* ((kanji-pos (cons (line-beginning-position 0) (line-end-position 0)))
            (english-pos (cons (point) (progn (insert definition "\n") (point))))
            (extra-pos (when-let (extra-text (and kanji-quiz-extra-text-function
                                                  (funcall kanji-quiz-extra-text-function term)))
                         (cons (progn (insert "\n") (point)) (progn (insert extra-text "\n") (point))))))
       (insert "\n\f\n")
       (list (cons 'term term)
             (cons 'furigana furigana-pos)
             (cons 'kanji (list kanji-pos))
             (cons 'english (list english-pos))
             (cons 'extra (and extra-pos (list extra-pos))))))))

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
    (setq buffer-read-only t)
    (kanji-quiz-advance)))
