; textile-list-problem.el
; proof of concept for a problem I'm having creating circular lists
; $Id$

(defun Textile-list-context (str)
  (let (my-list)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((= (char-after) ?#)
          (push (copy-sequence "ol") my-list))
         ((= (char-after) ?*)
          (push (copy-sequence "ul") my-list)))
        (forward-char 1)))
    my-list))

(defun Textile-clear-tokens ()
  "Clear token hash table."
  (clrhash Textile-tokens))

(defvar Textile-list-tag-regexp
  "^\\(([^ ]+?)\\|\\)\\([*#]+\\)\\([^ ]*\\) "
  "All list block tags must match this.")

(defun Textile-list-difference (small-list large-list)
  (if (equal small-list large-list)
      nil
    (if (= (length small-list) (length large-list))
        (cons (pop large-list)
              (Textile-list-difference
               (cdr small-list)
               large-list))
      (cons (pop large-list)
            (Textile-list-difference
             small-list
             large-list)))))

(defvar Textile-tokens (make-hash-table :test 'eql)
  "Token hash table; this is currently a global, which is probably unwise.")

(defun Textile-new-token (my-string)
  "Add MY-STRING to token hash table; returns string tag which replaces it."
  (let ((current-index (hash-table-count Textile-tokens)))
    (puthash current-index my-string Textile-tokens)
    (format "\000e%dx\000" current-index)))

(defun Textile-open-list (tag style)
  (Textile-new-token
   (concat "<" tag " et_context=\""
           tag "\" et_style=\""
           style "\" et_cite=\"\"\n")))

(defun Textile-close-list (tag)
  (Textile-new-token
   (concat "</" tag ">\n")))

(defun Textile-list-item (context style)
  (Textile-new-token
   (concat "<li et_context=\"" context
           "\" et_style=\"" style
           "\" et_cite=\"\">")))

(defun Textile-list-process (my-string)
  "Process Textile list code in MY-STRING, return tokenized text."
  (let ((list-level nil)
        (current-string ""))
    (with-temp-buffer
      (insert my-string)
      (goto-char (point-min))
      (while (re-search-forward Textile-list-tag-regexp nil t)
        (setq current-string "")
        (let* ((tag-string (match-string 0))
               (list-style (match-string 1))
               (item-style (match-string 2))
               (current-list-context (Textile-list-context tag-string)))
          (if (< (length current-list-context)
                 (length list-level))
              (progn
                (dolist (close-tag (Textile-list-difference current-list-context
                                                            list-level))
                  (setq current-string
                        (concat current-string
                                (Textile-close-list close-tag)))
                  (pop list-level))))
          (if (> (length current-list-context)
                 (length list-level))
              (progn
                (setq current-string
                      (concat current-string
                              (Textile-open-list
                               (car current-list-context)
                               list-style)
                              (Textile-list-item
                               (car current-list-context)
                               item-style)))
                ; THE PROBLEM IS HERE!
                (push (car current-list-context) list-level))
          ))
        (replace-match current-string))
      (buffer-string))))

(defun textile-string (my-string)
  "The workhorse loop.  Take MY-STRING, dump it in a temp buffer, and
  start operating on it."
  (Textile-clear-tokens)
  (with-temp-buffer
    (insert my-string)
      ; lists
    (goto-char (point-min))
    (while (or (looking-at Textile-list-tag-regexp)
               (re-search-forward Textile-list-tag-regexp
                                  nil t))
      (let ((first-part (match-string 0))
            (start-point (point)))
        (replace-match "")
        (setq start-point (point))
        (if (re-search-forward "\n\n" nil t)
            (re-search-backward "\n\n" nil t)
          (goto-char (point-max)))
        (insert (Textile-list-process (concat first-part
                                              (delete-and-extract-region
                                               start-point (point)))))))
    (buffer-string)))

(textile-string "* 1.0
** pass Brad Choate's test cases, which I received thanks to \"Roberto
   De Almeida\":http://dealmeida.net/en/Projects/PyTextile/, the
   excellent hacker currently responsible for PyTextile.
** blockquote citations (and citations for other blocks)
** any reported bugfixes
* 2.0
** Total rewrite using parser (see docs/engineering.txt for details as
   they become available)
")