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
    (define-key keymap "q" #'bury-buffer)
    keymap))

(define-derived-mode kanji-quiz-mode fundamental-mode "漢字"
  "Major mode for a kanji quiz")

(defun kanji-quiz-advance ()
  (interactive)
  (unless (cl-loop while kanji-quiz-next-step
                   thereis (/= 0 (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))))
    (when (null kanji-quiz-next-page)
      (setq kanji-quiz-next-page (kanji-quiz-shuffle kanji-quiz-terms)))
    (setq kanji-quiz-current-term (pop kanji-quiz-next-page))
    (setq kanji-quiz-next-step kanji-quiz-progression)
    (widen)
    (while (zerop (kanji-quiz-show-and-hide (pop kanji-quiz-next-step))))
    (goto-char (cdar (alist-get 'english kanji-quiz-current-term)))
    (narrow-to-page)))

(defun kanji-quiz-show-and-hide (actions)
  (cl-loop with inhibit-read-only = t
    for (action . labels) in actions sum
    (cl-loop with color = (face-attribute 'default (if (eq action 'show) :foreground :background))
      for label in labels sum
      (cl-loop with positions = (alist-get label kanji-quiz-current-term)
        for (start . end) in positions
        do (put-text-property start end 'face `(:foreground ,color))
        finally return (length positions)))))

(defun kanji-quiz-next-term (limit)
  (let ((lines (cl-loop while (re-search-forward "\\=\\(.+\\)\n?" limit t)
                 collect (list (match-string-no-properties 1)
                               (match-beginning 1)
                               (match-end 1)))))
    (re-search-forward "\\=\n*" limit t)
    (when (<= 2 (length lines))
      (let* ((term (propertize (concat (caar lines) "　") 'display `(height ,kanji-quiz-size-factor)))
             (kanji-count (how-many "\\cC" (nth 1 (car lines)) (nth 2 (car lines)))))
        (cond
         ((zerop kanji-count)
          (list term nil (string-join (mapcar #'car (cdr lines)) "\n")))
         ((= 2 (length lines))
          (error "Missing definition"))
         ((/= kanji-count (how-many "[^[:space:]　]+" (nth 1 (nth 1 lines)) (nth 2 (nth 1 lines))))
          (error "Furigana count does not match kanji count"))
         (t
          (list term (split-string (car (nth 1 lines)) "\\(?:　\\|\\s-\\)+") (string-join (mapcar #'car (cddr lines)) "\n"))))))))

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
    (setq-local kanji-quiz-progression kanji-quiz-progression-kanji-first)
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
          (setq english-pos (cons (point) (progn (insert definition "\n") (point))))
          (insert "\n\f\n")
          (list (cons 'furigana (prog1 furigana-pos (setq furigana-pos nil)))
                (cons 'kanji (list kanji-pos))
                (cons 'english (list english-pos))))))
    (with-current-buffer terms-buffer (goto-char terms-point))
    (setq buffer-read-only t)
    (setq-local kanji-quiz-next-step nil)
    (kanji-quiz-advance)))
