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

(defun textile-code-to-blocks (start end)
  (let ((my-region (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert my-region)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (message (format "Point is at %d." (point)))
        (let ((this-block-type "p")
              (this-block-tag "p")
              (this-block-class "")
              (this-block-style "")
              (this-block-id ""))
          (narrow-to-region (point)
                            (save-excursion
                              (re-search-forward "^\n" nil 1)
                              ; Not working right at the end; FIXME
                              (backward-char 2)
                              (point)))
          (goto-char (point-min))
          (while (looking-at "\n")
            (forward-char 1))
          ; deal with extended blocks (Textile 2), FIXME
          (if (looking-at "\\([^ .]+\\)\\. ")
              (progn
                (setq this-block-type (match-string 1))
                (setq this-block-tag
                      (cond
                       ((string= this-block-type "bq") "blockquote")
                       ((string-match "^h[1-6]$" this-block-type)
                        this-block-type)
                       ((string-match "^fn[0-9]+$" this-block-type)
                        (progn
                          (setq this-block-class "footnote")
                          (setq this-block-id this-block-type)
                          "p"))
                       ((string= this-block-type "bc") "pre")
                       (t nil)))
                (if this-block-tag
                    (delete-region (point)
                                   (save-excursion
                                     (re-search-forward "\\([^ ]+\\)\\. ")))
                  (setq this-block-tag "p"))))
          (goto-char (point-min))
          (while (looking-at "\n")
            (forward-char 1))
          (insert (concat "<" this-block-tag 
                          (if (not (string= this-block-class ""))
                              (concat " class=\"" this-block-class
                                      "\"" ))
                          (if (not (string= this-block-id ""))
                              (concat " id=\"" this-block-id
                                      "\"" ))
                          (if (not (string= this-block-style ""))
                              (concat " style=\"" this-block-style
                                      "\"" ))
                          ">"
                          (if (string= this-block-type "bc")
                              "<code>")))
          (goto-char (point-max))
          (insert (concat (if (string= this-block-type "bc")
                              "</code>") "</" this-block-tag ">"))
          (widen)
          (re-search-forward "^\n" nil 1)))
      (buffer-string))))

(defun textile-region (start end)
  (interactive "r")
  (textile-code-to-blocks start end))

(provide 'textile)