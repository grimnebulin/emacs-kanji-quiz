;; -*- lexical-binding: t; -*-
; In a new buffer, show:
; copy of first line with all non-kanji characters invisible
; and all kanji replaced by successive chars from the following line

; https://emacs.stackexchange.com/questions/5495/how-can-i-determine-the-width-of-characters-on-the-screen

(require 'cl-lib)

(defvar kanji-quiz-terms nil)
(defvar kanji-quiz-current-term nil)
(defvar kanji-quiz-next-step nil)
(defvar kanji-quiz-next-page nil)
(defconst kanji-quiz-size-factor 5.0)

(defconst kanji-quiz-progression
  '(((show kanji) (hide furigana english))
    ((show furigana))
    ((show english))))

(defvar kanji-quiz-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" #'kanji-quiz-advance)
    (define-key keymap "q" #'bury-buffer)
    keymap))

(define-derived-mode kanji-quiz-mode fundamental-mode "漢字"
  "Major mode for a kanji quiz")

(defun kanji-quiz-advance ()
  (interactive)
  (if kanji-quiz-next-step
      (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))
    (when (null kanji-quiz-next-page)
      (setq kanji-quiz-next-page (kanji-quiz-shuffle kanji-quiz-terms)))
    (setq kanji-quiz-current-term (pop kanji-quiz-next-page))
    (setq kanji-quiz-next-step kanji-quiz-progression)
    (widen)
    (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))
    (goto-char (cdar (alist-get 'english kanji-quiz-current-term)))
    (narrow-to-page)))

(defun kanji-quiz-show-and-hide (actions)
  (cl-loop with inhibit-read-only = t
    for (action . labels) in actions do
    (cl-loop with color = (face-attribute 'default (if (eq action 'show) :foreground :background))
      for label in labels do
      (cl-loop for (start . end) in (alist-get label kanji-quiz-current-term) do
        (put-text-property start end 'face (list :foreground color))))))

(defun kanji-quiz-next-term (limit)
  (when (re-search-forward "\\(.+\\)\n\\(.+\\)\n\\(\\(?:.+\n?\\)+\\)\n*" limit t)
    (list
     (propertize (concat (match-string-no-properties 1) "　") 'display `(height ,kanji-quiz-size-factor))
     (save-match-data (split-string (match-string-no-properties 2) "\\(?:　\\|\\s-\\)+"))
     (match-string-no-properties 3))))

(defun kanji-quiz-shuffle (list)
  (cl-loop with shuffled = (copy-sequence list)
           for i from (length shuffled) downto 2
           do (cl-rotatef (elt shuffled (random i)) (elt shuffled (1- i)))
           finally return shuffled))

(defun kanji-quiz-start (start end)
  (interactive
   (if (region-active-p)
       (list (min (point) (mark)) (max (point) (mark)))
     (list (point) (point-max))))
  (let ((terms-buffer (current-buffer))
        (terms-point (point))
        (quiz-buffer (get-buffer-create "*kanji-quiz*"))
        (background (face-attribute 'default :background)))
    (goto-char start)
    (pop-to-buffer quiz-buffer)
    (kanji-quiz-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq-local kanji-quiz-next-page nil)
    (setq-local kanji-quiz-terms
      (cl-loop with furigana-pos = nil
               with kanji-pos = nil
               with english-pos = nil
               for (line furigana definition) = (with-current-buffer terms-buffer (kanji-quiz-next-term end))
               while line collect
        (let ((p (point)))
          (save-excursion
            (insert line "\n" line "\n"))
          (while (re-search-forward (rx (category chinese-two-byte)) (line-end-position) t)
            (put-text-property p (point) 'face `(:foreground ,background))
            (push (cons (point)
                        (progn
                          (replace-match
                           (let ((next-furigana (pop furigana)))
                             (propertize
                              (concat "　" next-furigana "　")
                              'display
                              `(height ,(/ kanji-quiz-size-factor (+ 2 (length next-furigana)))))))
                          (point)))
                  furigana-pos)
            (setq p (point)))
          (put-text-property p (line-end-position) 'face `(:foreground ,background))
          (forward-line 2)
          (setq kanji-pos (cons (line-beginning-position 0) (line-end-position 0)))
          (setq english-pos (cons (point) (progn (insert definition) (point))))
          (insert "\n\f\n")
          (list (cons 'furigana (prog1 furigana-pos (setq furigana-pos nil)))
                (cons 'kanji (list kanji-pos))
                (cons 'english (list english-pos))))))
    (with-current-buffer terms-buffer (goto-char terms-point))
    (setq buffer-read-only t)
    (setq-local kanji-quiz-next-step nil)
    (kanji-quiz-advance)))
