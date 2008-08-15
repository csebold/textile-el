; textile2.el
; $Id$

; by Charles Sebold <csebold@gmail.com>

;;; Copyright: (C) 2004, 2008 Charles Sebold
;; 
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with GNU Emacs; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Latest version should be available at:
;;    <URL:http://zancanda.staticcling.org/emacs/textile/>

; Note - in comments, DA = Dean Allen (original author of PHP Textile),
; BC = Brad Choate (implemented Textile v2 in Perl for Movable Type)

; To use textile.el, put this file into your load path, and put the
; following into your .emacs file:
;
; (require 'textile)
;
; At this time Textile markup is only documented at
; <http://www.textism.com/tools/textile/> and
; <http://bradchoate.com/mt/docs/mtmanual_textile2.html>, so to
; understand how to write Textile code, for now you should check those
; web sites.  In the future there will be a Texinfo manual detailing the
; format that comes with textile.el.
;
; In a buffer with Textile markup, you can run:
;
;   M-x textile-buffer RET
;
; and have the entire buffer processed by Textile, which (by default)
; will create a new buffer with valid XHTML markup in it.  If you want
; the returned XHTML in the same buffer as the old code, you can change
; textile-output-to-new-buffer to nil (it defaults to t).  You can also
; call textile-region in similar fashion.  If you want to use textile.el
; in your own Emacs Lisp programs, you can pass a string to
; textile-string and it will return XHTML in another string.
;
; Standard Textile behavior is to treat newlines in the middle of a
; block (like a paragraph) as intentional newlines, and it replaces them
; with <br /> when it sees them.  To change this behavior so that you
; can edit Textile text with auto-fill-mode on, you can either load
; longlines.el (found elsewhere) or you can change
; textile-br-all-newlines to nil (it defaults to t).  It is hard to
; guarantee that textile.el will always do the right thing if you change
; textile-br-all-newlines, and you may find it difficult in some cases
; to enter a manual linebreak unless you escape it with ==; the best
; thing to do is either edit without auto-fill-mode or use longlines,
; but I will try to keep textile-br-all-newlines working as expected
; when possible (because I like auto-fill-mode).
;
; Do NOT send bug reports on textile.el to Dean Allen or Brad Choate;
; they had nothing to do with the Emacs implementation.  Send bug
; reports to csebold@gmail.com, preferably along with sample text and
; some description of what you expected to see.
;
; See docs/bugs.txt for the bugs in this version.

(defvar textile-version "Textile.el v1.99.1"
  "Version number for textile.el.")

(defvar Textile-tokens (make-hash-table :test 'eql)
  "Token hash table; this is currently a global, which is probably unwise.")

(defvar Textile-token-re
  "emacs_textile_token_[0-9]+_x"
  "Regular expression for finding a token.")

(defun Textile-new-token (my-string)
  "Add MY-STRING to token hash table; returns string tag which replaces it."
  (let ((current-index (hash-table-count Textile-tokens)))
    (puthash current-index my-string Textile-tokens)
    (format "emacs_textile_token_%d_x" current-index)))

(defun Textile-get-token (my-index)
  "Get Textile token by hash index; does not change token table."
  (if (stringp my-index)
      (save-match-data
        (if (string-match "^emacs_textile_token_\\([0-9]+\\)_x$"
                          my-index)
            (setq my-index (string-to-int (match-string 1 my-index)))
          (setq my-index (string-to-int my-index)))))
  (gethash my-index Textile-tokens))

(defun Textile-clear-tokens ()
  "Clear token hash table."
  (clrhash Textile-tokens))

(defvar Textile-output-to-new-buffer t
  "Should Textile output go to a new buffer?")

(defvar Textile-xhtml-version-default "XHTML 1.0 Transitional"
  "What is the default version of XHTML that Textile will produce?")

(defvar Textile-xhtml-docstrings
  '("XHTML 1.0 Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    "XHTML 1.0 Transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    "XHTML 1.0 Frameset" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
    "XHTML 1.1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
  "Standard HTML doctypes so that a Textile document can be self-contained.")

(defvar Textile-tag-re-list
  '(("bq" "blockquote" "p") ("p" "p") ("h1" "h1") ("h2" "h2") ("h3" "h3")
    ("h4" "h4") ("h5" "h5"))
  "List of Textile tags and their HTML counterparts.")

(defvar Textile-tags
  (regexp-opt (mapcar 'car Textile-tag-re-list) t)
  "Generated regular expression for Textile tags.")

(defun textile-region (start end)
  "Call textile-code-to-blocks on region from point to mark."
  (interactive "r")
  (save-excursion
    (let ((my-string (buffer-substring start end)))
      (if Textile-output-to-new-buffer
          (switch-to-buffer (get-buffer-create
                             (Textile-new-buffer-name)))
        (delete-region start end)
        (goto-char start))
      (insert (textile-string my-string)))))

(defun Textile-new-buffer-name ()
  "Create new buffer name based on old buffer name."
  (let ((inc 1))
    (if (get-buffer (concat (buffer-name) ".html"))
        (progn
          (while (get-buffer (concat (buffer-name) "-"
                                     (number-to-string inc) ".html"))
            (setq inc (+ inc 1)))
          (concat (buffer-name) "-" (number-to-string inc) ".html"))
      (concat (buffer-name) ".html"))))

(if (condition-case nil
        (split-string "a" "" t)
      (error nil))
    (defun Textile-split-string (my-string separator)
      "Split MY-STRING by SEPARATOR and don't return empty substrings."
      (split-string my-string separator t))
  (defun Textile-split-string (my-string separator)
    "Split MY-STRING by SEPARATOR and don't return empty substrings."
    (split-string my-string separator)))

(defun textile-buffer ()
  "Call textile-code-to-blocks on the entire buffer."
  (interactive)
  (textile-region (point-min) (point-max)))

(defun Textile-interpret-attributes (context-arg style-arg cite-arg)
  "Interpret the attributes for block tags and return the actual XHTML string."
  (save-match-data
    (let ((return-string ""))
      (if (string-match "^:" cite-arg)
          (setq cite-arg (replace-match "" t t cite-arg)))
      (with-temp-buffer
        (insert style-arg)
        (goto-char (point-min))
        (while (re-search-forward "\n" nil t)
          (replace-match " "))
        (goto-char (point-min))
        (let ((my-plist nil)
              (style nil)
              (class nil)
              (id nil)
              (lang nil)
              (left-pad 0)
              (right-pad 0)
              (align nil)
              (valign nil)
              (rowspan nil)
              (colspan nil)
              (textile-header nil)
              (not-finished t)
              (textile-well-formed t))
          (while (not (eobp))
            (let ((this-char (char-after)))
            (cond
             ((looking-at "{\\([^}]*\\)}")
              (push (match-string 1) style)
              (re-search-forward "}" nil t))
             ((looking-at "\\[\\(.*?\\)\\]")
              (setq lang (match-string 1))
              (re-search-forward "\\]" nil t))
             ((looking-at "(\\([^) (]+\\))")
              (let ((this-attrib (save-match-data
                                   (Textile-split-string
                                    (match-string 1) "#"))))
                (if (and (string-match "#" (match-string 1))
                         (= (match-beginning 0) 0))
                    (setq id (car this-attrib))
                  (push (car this-attrib) class)
                  (setq id (cadr this-attrib))))
              (re-search-forward ")" nil t))
             ((equal this-char ?\()
              (setq left-pad (1+ left-pad))
              (forward-char 1))
             ((equal this-char ?\))
              (setq right-pad (1+ right-pad))
              (forward-char 1))
             ((equal this-char ?\>)
              ; this will change if it's in a table
              (push "right" class)
              (push "text-align:right" style)
              (forward-char 1))
             ((looking-at "<>")
              (push "justify" class)
              (push "text-align:justify" style)
              (forward-char 2))
             ((equal this-char ?\<)
              (push "left" class)
              (push "text-align:left" style)
              (forward-char 1))
             ((equal this-char ?\=)
              (push "center" class)
              (push "text-align:center" style)
              (forward-char 1)))))
          (if (> left-pad 0)
              (push (concat "padding-left:" (format "%d" left-pad) "em") style))
          (if (> right-pad 0)
              (push (concat "padding-right:" (format "%d" right-pad) "em")
                    style))
          (when (and (or (> left-pad 0) (> right-pad 0))
                     style)
            (when (member "text-align:left" style)
              (setq style (delete "text-align:left" style))
              (push "float:left" style))
            (when (member "text-align:right" style)
              (setq style (delete "text-align:right" style))
              (push "float:right" style)))
          (dolist (this-variable '(style class id lang align valign
                                         colspan rowspan
                                         textile-header textile-well-formed))
            (when (and (stringp (eval this-variable))
                       (string= (eval this-variable) ""))
              (set this-variable nil)))
          (if class
              (setq return-string (concat return-string " class=\""
                                          (Textile-string-concat class " ")
                                          "\"")))
          (if id
              (setq return-string (concat return-string " id=\"" id "\"")))
          (if style
              (setq return-string (concat return-string " style=\""
                                          (Textile-string-concat style ";")
                                          "\"")))
          (if lang
              ; don't forget to change this if running XHTML 1.1 (xml:lang)
              (setq return-string (concat return-string " lang=\"" lang "\"")))
          ))
      (if (and (string= context-arg "blockquote")
               (> (length cite-arg) 0))
          (setq return-string (concat return-string " cite=\"" cite-arg "\"")))
      return-string)))

(defun Textile-string-concat (my-list separator)
  "Return all strings concatenated with separator between them."
  (if (cdr my-list)
      (concat (car my-list) separator (Textile-string-concat (cdr my-list)
                                                             separator))
    (car my-list)))

(defun Textile-table-process (my-string)
  "Process Textile table code in MY-STRING, return tokenized text for each
cell."
  (with-temp-buffer
    (insert "TABLE HERE" my-string "TABLE ENDS")
    (buffer-string)))

(defun textile-string (my-string)
  "The workhorse loop.  Take MY-STRING, dump it in a temp buffer, and
  start operating on it."
  (Textile-clear-tokens)
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    ; begin tokenizing
    ; first provide for BC's unit test; may remove this later
    (while (re-search-forward "^==>" nil t)
      (let ((start-pos (save-excursion
                         (beginning-of-line)
                         (point)))
            (end-pos nil))
        (if (re-search-forward "-----$" nil t)
            (setq end-pos (point))
          (setq end-pos (point-max)))
        (insert (Textile-new-token (delete-and-extract-region
                                    start-pos end-pos)))))
    ; blockcode processor, sticky (goes here)
    (goto-char (point-min))
    (while (or (looking-at "bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) ")
               (re-search-forward "^bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) " nil t))
      (replace-match (Textile-new-token
                      (concat "<pre et_context=\"pre\" et_style=\""
                              (match-string 1) "\" et_cite=\"" (match-string 2)
                              "\"><code>")))
      (let ((start-point (point))
            (end-point (re-search-forward
                        (concat "\\(.*\\)\\(\n\n" Textile-tags
                                "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) \\)") nil t)))
        (if end-point
            (replace-match (concat (Textile-new-token
                                    (concat (match-string 1)
                                            "</code></pre>"))
                                   (match-string 2)))
          (goto-char (point-max))
          (setq end-point (point))
          (insert (Textile-new-token (concat (delete-and-extract-region
                                              start-point end-point)
                                             "</code></pre>"))))))
    ; blockcode processor, non-sticky
    (goto-char (point-min))
    (while (or (looking-at "bc\\([^.]*\\)\\.\\(:[^ ]*\\|\\) ")
               (re-search-forward "^bc\\([^.]*\\)\\.\\(:[^ ]*\\|\\) " nil t))
      (replace-match (Textile-new-token
                      (concat "<pre et_context=\"pre\" et_style=\""
                              (match-string 1) "\" et_cite=\"" (match-string 2)
                              "\"><code>")))
      (if (looking-at "\\(.*\\)\n\n")
          (replace-match (Textile-new-token (concat (match-string 1)
                                                    "</code></pre>\n\n")))
        (insert (Textile-new-token (concat (delete-and-extract-region
                                            (point) (point-max)
                                            "</code></pre>\n\n"))))))
    ; footnote processor
    (goto-char (point-min))
    (while (re-search-forward "^fn\\([0-9]+\\)\\. " nil t)
      (replace-match (Textile-new-token (concat "<p class=\"footnote\" id=\"fn"
                                                (match-string 1)
                                                "\"><sup>"
                                                (match-string 1)
                                                "</sup> ")))
      (if (re-search-forward "\n\n" nil t)
          (replace-match (Textile-new-token "</p>\n\n"))
        (goto-char (point-max))
        (insert (Textile-new-token "</p>"))))
    ; go through Textile sticky blockquote tags
    (goto-char (point-min))
    (while (or (looking-at "bq\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) ")
               (re-search-forward "bq\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) " nil t))
      (replace-match (Textile-new-token
                      (concat "<blockquote et_context=\"blockquote\" et_style=\""
                              (match-string 1) "\" et_cite=\"" (match-string 2)
                              "\"><p>")))
      (let ((end-tag-found nil))
        (while (and (not end-tag-found)
                    (re-search-forward "\n\n" nil t))
          (if (save-match-data
                (looking-at (concat "\\(" Textile-token-re "\\)\\|\\("
                                    Textile-tags
                                    "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) \\)")))
              (progn
                (replace-match (Textile-new-token "</p></blockquote>\n\n"))
                ; be careful, this could cause us to skip a token later
                (setq end-tag-found t))
            (replace-match (Textile-new-token "</p>\n\n<p>"))))))
    ; find tables, call out to table processor
    (goto-char (point-min))
    (while (or (looking-at "table[^.]*\\. ")
               (re-search-forward "^table[^.]*\\. " nil t))
      (let ((first-part (match-string 0))
            (second-part ""))
        ; this isn't working to find the end of the table, FIXME
        (replace-match "")
        (save-match-data
          (if (looking-at "\\(.*\\)\n\n")
              (progn
                (setq second-part (match-string 1))
                (replace-match "\n\n"))
            (setq second-part (delete-and-extract-region
                               (point) (point-max)))))
        (insert (Textile-table-process (concat first-part second-part)))))
    ; go through Textile tags
    (goto-char (point-min))
    (while (or (looking-at (concat Textile-tags
                                   "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) "))
               (re-search-forward (concat "^" "\\(?:" Textile-token-re "\\|\\)"
                                          Textile-tags
                                          "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) ") nil t))
      (let* ((my-tags (cdr (assoc (match-string 1) Textile-tag-re-list)))
             (my-1st-tag (car my-tags))
             (my-2nd-tag (cadr my-tags))
             (my-close-tag (concat (if my-2nd-tag
                                       (concat "</" my-2nd-tag ">"))
                                   "</" my-1st-tag ">")))
        (replace-match (Textile-new-token (concat "<" my-1st-tag
                                                  " et_context=\""
                                                  my-1st-tag
                                                  "\" et_style=\""
                                                  (match-string 2)
                                                  "\" et_cite=\""
                                                  (match-string 3) "\">"
                                                  (if my-2nd-tag
                                                      (concat "<"
                                                              my-2nd-tag
                                                              ">")))))
        (if (re-search-forward "\n\n" nil t)
            (replace-match (Textile-new-token (concat my-close-tag "\n\n")))
          (goto-char (point-max))
          (insert (Textile-new-token my-close-tag)))))
    ; revert tokens
    (goto-char (point-min))
    (while (or (looking-at Textile-token-re)
               (re-search-forward Textile-token-re nil t))
      (replace-match (Textile-get-token (match-string 0)) t t))
    ; interpret attributes
    (goto-char (point-min))
    (while (re-search-forward (concat " et_context=\"\\(.*\\)\""
                                      " et_style=\"\\([^\"]*\\)\""
                                      " et_cite=\"\\(.*\\)\"")
                              nil t)
      (replace-match (Textile-interpret-attributes (match-string 1)
                                                   (match-string 2)
                                                   (match-string 3)))
      (goto-char (point-min)))
    (buffer-string)))

(provide 'textile)