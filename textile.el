; textile.el
; $Id$

(defun textile-code-to-blocks (start end)
  (let ((my-region (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert my-region)
      (beginning-of-buffer)
      (while (not (equal (point)
                         (save-excursion
                           (forward-paragraph 1)
                           (point))))
        (let ((this-block-type "p")
              (this-block-tag "p")
              (this-block-class "")
              (this-block-style "")
              (this-block-id ""))
          (narrow-to-region (point)
                            (save-excursion
                              (forward-paragraph 1)
                              (if (looking-at "\n")
                                  (backward-char 1))
                              (point)))
          (beginning-of-buffer)
          (while (looking-at "\n")
            (forward-char 1))
          (if (looking-at "\\([^ ]+\\)\\. ")
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
                       (t nil)))
                (if this-block-tag
                    (delete-region (point)
                                   (save-excursion
                                     (re-search-forward "\\([^ ]+\\)\\. ")
                                     (point)))
                  (setq this-block-tag "p"))))
          (beginning-of-buffer)
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
                          ">"))
          (end-of-buffer)
          (insert (concat "</" this-block-tag ">"))
          (widen)
          (forward-paragraph 1)))
      (buffer-string))))

(defun textile-region (start end)
  (interactive "r")
  (textile-code-to-blocks start end))

(provide 'textile)