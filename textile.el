; textile.el
; $Id$

(defun textile-code-to-blocks (start end)
  (let ((my-region (buffer-substring-no-properties start end))
        (this-block-type "p"))
    (with-temp-buffer
      (insert my-region)
      (beginning-of-buffer)
      (while (not (equal (point)
                         (save-excursion
                           (forward-paragraph 1)
                           (point))))
        (narrow-to-region (point)
                          (save-excursion
                            (forward-paragraph 1)
                            (if (looking-at "\n")
                                (backward-char 1))
                            (point)))
        (beginning-of-buffer)
        (if (looking-at "\n")
            (forward-char 1))
        (if (looking-at "\\([^ ]+\\)\\. ")
            (progn
              (setq this-block-type (match-string 1))
              (delete-region (point)
                             (save-excursion
                               (re-search-forward "\\([^ ]+\\)\\. ")
                               (point))))
          (setq this-block-type "p"))
        ; should determine what kind of block this is - FIXME
        (beginning-of-buffer)
        (if (looking-at "\n")
            (forward-char 1))
        (insert (concat "<" this-block-type ">"))
        (end-of-buffer)
        (insert (concat "</" this-block-type ">"))
        (widen)
        (forward-paragraph 1))
      (buffer-string))))

(defun textile-region (start end)
  (interactive "r")
  (textile-code-to-blocks start end))

(provide 'textile)