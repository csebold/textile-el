; textile.el
; $Id$

; Filters will probably have to wait for v2 of this code;
; that looks even harder.

(defvar textile-block-tag-regexp-start "^\\(")
(defvar textile-block-any-tag-regexp "[a-z0-9]+")
(defvar textile-block-tag-regexp-end "\\)\\(.*?\\)\\(\\.\\{1,2\\}\\) ")

(defvar textile-block-tag-regexp
  (concat textile-block-tag-regexp-start
          textile-block-any-tag-regexp
          textile-block-tag-regexp-end))

(defun textile-this-block-tag-regexp (tag)
  (concat textile-block-tag-regexp-start
          tag "+"
          textile-block-tag-regexp-end))

(defvar textile-error-break nil)

; code for processing each paragraph of an extended block
;;     (while (and
;;             (not (looking-at textile-block-tag-regexp))
;;             (not (eobp)))
;;       (textile-next-paragraph)

(defun textile-code-to-blocks (start end)
  (narrow-to-region start end)
  (goto-char (point-min))
  (while
      (progn
        (if (looking-at textile-block-tag-regexp)
            (let* ((tag (match-string 1))
                   (attributes (textile-attributes (match-string 2)))
                   (extended (string= (match-string 3) ".."))
                   (style (plist-get attributes 'style))
                   (class (plist-get attributes 'class))
                   (id (plist-get attributes 'id))
                   (lang (plist-get attributes 'lang))
                   (my-function
                    (car (read-from-string (concat "textile-block-" tag)))))
              (if (fboundp my-function)
                  (funcall my-function extended style class id lang)
                (textile-block-p nil nil nil nil nil)))
          (textile-block-p nil nil nil nil nil))))
  (widen))

(defun textile-attributes (attrib-string)
  (let ((my-plist nil)
        (style "")
        (class nil)
        (id nil)
        (lang nil)
        (left-pad 0)
        (right-pad 0))
    (with-temp-buffer
      (insert attrib-string)
      (beginning-of-buffer)
      (while
          (let ((this-char (char-after)))
            (cond
             ((looking-at "{\\([^}]*\\)}")
              (setq style (concat style (match-string 1) "; "))
              (re-search-forward "}" nil t))
             ((looking-at "\\[\\(.*?\\)\\]")
              (setq lang (match-string 1))
              (re-search-forward "\\]" nil t))
             ((looking-at "(\\([^)(]+\\))")
              (let ((this-attrib (split-string (match-string 1) "#")))
                (setq class (car this-attrib))
                (setq id (cadr this-attrib)))
              (re-search-forward ")" nil t))
             ((equal this-char ?\()
              (setq left-pad (1+ left-pad))
              (forward-char 1))
             ((equal this-char ?\))
              (setq right-pad (1+ right-pad))
              (forward-char 1))
             ((equal this-char ?\>)
              (setq style (concat style "text-align: right; "))
              (forward-char 1))
             ((looking-at "<>")
              (setq style (concat style "text-align: justify; "))
              (forward-char 2))
             ((equal this-char ?\<)
              (setq style (concat style "text-align: left; "))
              (forward-char 1))
             ((equal this-char ?\=)
              (setq style (concat style "text-align: center; "))
              (forward-char 1))
;;                ((equal this-char ?\|)
;;                 (forward-char 1)) ; filters - FIXME
             (t (forward-char 1)))
            (not (eobp)))))
    (if (> left-pad 0)
        (setq style (concat style "padding-left: "
                            (format "%d" left-pad) "em; ")))
    (if (> right-pad 0)
        (setq style (concat style "padding-right: "
                            (format "%d" right-pad) "em; ")))
    (dolist (this-variable '('style 'class 'id 'lang))
      (if (string= (eval this-variable) "")
          (setq (eval this-variable) nil)))
    (setq my-plist (plist-put my-plist 'style style))
    (setq my-plist (plist-put my-plist 'class class))
    (setq my-plist (plist-put my-plist 'id id))
    (setq my-plist (plist-put my-plist 'lang lang))
    my-plist))

(defun textile-block-p (extended style class id lang)
  (if extended
      (textile-error "Extended <p> block doesn't make sense.")
    (if (looking-at (textile-this-block-tag-regexp "p"))
        (textile-delete-tag "p"))
    (textile-block-start-tag-insert "p" style class id lang)
    (textile-end-of-paragraph)
    (textile-block-end-tag-insert "p")
    (textile-next-paragraph)))

(defun textile-delete-tag (&optional tag)
  (delete-region
   (point)
   (save-excursion
     (re-search-forward
      (if tag
          (textile-this-block-tag-regexp tag)
        textile-block-tag-regexp)
      nil t)
     (point))))

(defun textile-error (error-message)
  (if textile-error-break
      (error "%s" error-message)
    (message "Textile error: %s" error-message)))

(defun textile-region (start end)
  (interactive "r")
  (textile-code-to-blocks start end))

(defun textile-block-start-tag-insert (tag style class id lang)
  (insert (concat "<" tag))
  (if id
      (insert (concat " id=\"" id "\"")))
  (if class
      (insert (concat " class=\"" class "\"")))
  (if style
      (insert (concat " style=\"" style "\"")))
  (if lang
      (insert (concat " lang=\"" lang "\"")))
  (insert ">"))

(defun textile-block-end-tag-insert (tag)
  (insert (concat "</" tag ">")))

(defun textile-next-paragraph ()
  (re-search-forward "^\n+" nil 1))

(defun textile-end-of-paragraph ()
  (textile-next-paragraph)
  (backward-char 1)
  (while (looking-at "\n")
    (backward-char 1))
  (forward-char 1))

(provide 'textile)