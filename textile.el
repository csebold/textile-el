; textile.el
; $Id$

; ideas for breaking this code out into subroutines: Instead of using
; regular expressions to process everything, let's go through the
; buffer and deal with things one block at a time, and in cases where
; there are blocks within blocks, we can reprocess the buffer after
; removing things like the bq. tag.  Then we can call a handler for
; each different kind of block, and if the handler doesn't exist, then
; we assume that it's supposed to be there in the actual text.

; I need to figure out how to process
; p{style}. p[lang]. p(class#id). and p> p< p= p<> and p( or p)
; handlers.  Filters will probably have to wait for v2 of this code;
; that looks even harder.

; Is there a standard emacs way to process paragraphs?  Will it work
; with traditional Textile code?

; I think that Textile was really made for OOP and I'm not sure how
; well Emacs will stand up to the task.  I don't mind requiring 'cl if
; I have to but I really didn't want to have to use EIEIO for this.

(defvar textile-block-tag-regexp-start "\\(")
(defvar textile-block-any-tag-regexp "[^ .]+")
(defvar textile-block-tag-regexp-end "\\)\\(\\.\\{1,2\\}\\) ")

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
;;             (not (equal (point) (point-max))))
;;       (textile-next-paragraph)

(defun textile-code-to-blocks (start end)
  (narrow-to-region start end)
  (goto-char (point-min))
  (while
      (progn
        (if (looking-at textile-block-tag-regexp)
            (let ((tag (match-string 1))
                  (extended (string= (match-string 2) ".."))
                  (style nil)
                  (class nil)
                  (id nil)) ; FIXME - fix style/class/id handling
              (if (fboundp
                   (car (read-from-string (concat "textile-block-" tag))))
                  (eval (car (read-from-string
                              (concat "(textile-block-" tag " " extended
                                      " " style " " class " " id))))
                (textile-block-p nil nil nil nil)))
          (textile-block-p nil nil nil nil))))
  (widen))

(defun textile-block-p (extended style class id)
  (if extended
      (textile-error "Extended <p> block doesn't make sense.")
    (if (looking-at (textile-this-block-tag-regexp "p"))
        (textile-delete-tag "p"))
    (textile-block-start-tag-insert "p" style class id)
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

(defun textile-block-start-tag-insert (tag style class id)
  (insert (concat "<" tag))
  (if id
      (insert (concat " id=\"" id "\"")))
  (if class
      (insert (concat " class=\"" class "\"")))
  (if style
      (insert (concat " style=\"" style "\"")))
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