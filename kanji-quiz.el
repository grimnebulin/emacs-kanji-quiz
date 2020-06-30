;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar kanji-quiz-terms nil)
(defvar kanji-quiz-current-term nil)
(defvar kanji-quiz-next-step nil)
(defvar kanji-quiz-next-page nil)
(defvar kanji-quiz-progression nil)
(defconst kanji-quiz-size-factor 6.0)

(defconst kanji-quiz-progression-kanji-first
  '(((show kanji) (hide furigana english))
    ((show furigana))
    ((show english))))

(defconst kanji-quiz-progression-english-first
  '(((hide kanji furigana) (show english))
    ((show kanji))
    ((show furigana))))

(defvar kanji-quiz-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" #'kanji-quiz-advance)
    (define-key keymap "x" #'kanji-quiz-eject-term)
    (define-key keymap "q" #'bury-buffer)
    keymap))

(define-derived-mode kanji-quiz-mode fundamental-mode "漢字"
  "Major mode for a kanji quiz")

(defun kanji-quiz-advance ()
  (interactive)
  (unless (cl-loop while kanji-quiz-next-step
                   thereis (/= 0 (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))))
    (kanji-quiz-next-term t)))

(defun kanji-quiz-eject-term ()
  (interactive)
  (if (null kanji-quiz-terms)
      (message "No more terms")
    (kanji-quiz-next-term nil)))

(defun kanji-quiz-next-term (save-current-term)
  (when (null kanji-quiz-next-page)
    (setq kanji-quiz-next-page (kanji-quiz-shuffle kanji-quiz-terms))
    (setq kanji-quiz-terms nil))
  (when (and save-current-term kanji-quiz-current-term)
    (push kanji-quiz-current-term kanji-quiz-terms))
  (setq kanji-quiz-current-term (pop kanji-quiz-next-page))
  (setq kanji-quiz-next-step kanji-quiz-progression)
  (widen)
  (while (zerop (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))))
  (goto-char (cdar (alist-get 'english kanji-quiz-current-term)))
  (narrow-to-page))

(defun kanji-quiz-show-and-hide (actions)
  (cl-loop with inhibit-read-only = t
    for (action . labels) in actions
    sum (cl-loop with color = (face-attribute 'default (if (eq action 'show) :foreground :background))
          for label in labels
          sum (cl-loop with positions = (alist-get label kanji-quiz-current-term)
                for (start . end) in positions
                do (put-text-property start end 'face `(:foreground ,color))
                finally return (length positions)))))

(defun kanji-quiz-parse-term (limit)
  (let ((lines (cl-loop while (re-search-forward (rx point (group (+ nonl)) (? ?\n)) limit t)
                 collect (cons (cons (match-beginning 1) (match-end 1)) (match-string-no-properties 1)))))
    (re-search-forward (rx point (* ?\n)) limit t)
    (when (cdr lines)
      (let* ((term (propertize (concat (cdar lines) "　") 'display `(height ,kanji-quiz-size-factor)))
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
  (cl-loop with shuffled = (copy-sequence list)
           for i from (length shuffled) downto 2
           do (cl-rotatef (elt shuffled (random i)) (elt shuffled (1- i)))
           finally return shuffled))

(defun kanji-quiz-region ()
  (cond
   ((region-active-p)
    (list (min (point) (mark)) (max (point) (mark))))
   (current-prefix-arg
    (let ((end (save-excursion (forward-paragraph (prefix-numeric-value current-prefix-arg)) (point))))
      (list (min (point) end) (max (point) end))))
   (t
    (list (point) (point-max)))))

(defun kanji-quiz-start-english-first (start end)
  (interactive (kanji-quiz-region))
  (kanji-quiz-start start end kanji-quiz-progression-english-first))

(defun kanji-quiz-start-kanji-first (start end)
  (interactive (kanji-quiz-region))
  (kanji-quiz-start start end kanji-quiz-progression-kanji-first))

(defun kanji-quiz-populate-quiz-buffer (next-term)
  (cl-loop
   with background = (face-attribute 'default :background)
   for (line furigana definition) = (funcall next-term)
   while line
   collect
   (let ((p (point))
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
       (list (cons 'furigana furigana-pos)
             (cons 'kanji (list kanji-pos))
             (cons 'english (list english-pos)))))))

(defun kanji-quiz-start (start end progression)
  (let ((terms-buffer (current-buffer))
        (terms-point (point)))
    (goto-char start)
    (pop-to-buffer (get-buffer-create "*kanji-quiz*"))
    (kanji-quiz-mode)
    (setq-local
     kanji-quiz-terms
     (let ((inhibit-read-only t))
       (erase-buffer)
       (kanji-quiz-populate-quiz-buffer
        (lambda () (with-current-buffer terms-buffer (kanji-quiz-parse-term end))))))
    (with-current-buffer terms-buffer (goto-char terms-point))
    (setq-local kanji-quiz-next-page nil)
    (setq-local kanji-quiz-next-step nil)
    (setq-local kanji-quiz-progression progression)
    (kanji-quiz-advance)))
