;; -*- lexical-binding: t; -*-
; In a new buffer, show:
; copy of first line with all non-kanji characters invisible
; and all kanji replaced by successive chars from the following line

; https://emacs.stackexchange.com/questions/5495/how-can-i-determine-the-width-of-characters-on-the-screen

;; Want to implement:
;; Option A: Start with Kanji only showing; "n" -> show pronunciation; "n" -> show English
;; Option B: Start with English; "n" -> show Kanji; "n" -> show pronunciation
;; Need to store for each term:
;;   - buffer position range for English text
;;   - buffer position range for kanji
;;   - buffer position ranges for visible pronunciation chars

(require 'cl)

(defvar kanji-quiz-terms nil)
(defvar kanji-quiz-current-term nil)
(defvar kanji-quiz-next-page nil)
(defconst kanji-quiz-size-factor 5.0)

(defvar kanji-quiz-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" #'kanji-quiz-next-word)
    (define-key keymap "q" #'bury-buffer)
    keymap))

(define-derived-mode kanji-quiz-mode fundamental-mode "漢字"
  "Major mode for a kanji quiz")

(defun kanji-quiz-next-word ()
  (interactive)
  (when (null kanji-quiz-next-page)
    (setq kanji-quiz-next-page (kanji-quiz-shuffle kanji-quiz-terms)))
  (widen)
  (setq kanji-quiz-current-term (pop kanji-quiz-next-page))
  (goto-char (cdr (alist-get 'english kanji-quiz-current-term)))
  (narrow-to-page))

(defun kanji-quiz-next-term (limit)
  (when (re-search-forward "\\(.+\\)\n\\(.+\\)\n\\(\\(?:.+\n?\\)+\\)\n*" limit t)
    (list
     (propertize (concat (match-string-no-properties 1) "　") 'display `(height ,kanji-quiz-size-factor))
     (save-match-data (split-string (match-string-no-properties 2) "\\(?:　\\|\\s-\\)+"))
     (match-string-no-properties 3))))

(defun kanji-quiz-shuffle (list)
  (cl-loop with shuffled = (copy-sequence list)
           for i from (length shuffled) downto 2
           do (rotatef (elt shuffled (random i)) (elt shuffled (1- i)))
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
            (insert line "\n")
            (setq kanji-pos (cons (point) (prog2 (insert line) (point) (insert "\n")))))
          (while (re-search-forward "\\cC" (line-end-position) t)
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
          (setq english-pos (cons (point) (progn (insert definition) (point))))
          ;; Want instead:
          ;; ((furigana . ((furigana-pos-start . furigana-pos-end) ...))
          ;;  (kanji . (kanji-pos-start . kanji-pos-end))
          ;;  (english . (english-pos-start . english-pos-end)))
          (insert "\n\f\n")
          (list (cons 'furigana (prog1 furigana-pos (setq furigana-pos nil)))
                (cons 'kanji kanji-pos)
                (cons 'english english-pos)))))
    (with-current-buffer terms-buffer (goto-char terms-point))
    (setq buffer-read-only t)
    (kanji-quiz-next-word)))
