; textile2.el
; $Id$

; by Charles Sebold <csebold+textile@gmail.com>

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
; (require 'textile2)
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
; reports to csebold+textile@gmail.com, preferably along with sample
; text and some description of what you expected to see.
;
; See docs/bugs.txt for the bugs in this version.

(defvar textile-version "textile2.el v1.99.1"
  "Version number for textile.el.")

(defvar Textile-tokens (make-hash-table :test 'eql)
  "Token hash table; this is currently a global, which is probably unwise.")

(defvar Textile-token-re
  "\000e[0-9]+x\000"
  "Regular expression for finding a token.")

(defun Textile-new-token (my-string)
  "Add MY-STRING to token hash table; returns string tag which replaces it."
  (let ((current-index (hash-table-count Textile-tokens)))
    (puthash current-index my-string Textile-tokens)
    (format "\000e%dx\000" current-index)))

(defun Textile-get-token (my-index)
  "Get Textile token by hash index; does not change token table."
  (if (stringp my-index)
      (save-match-data
        (if (string-match "^\000e\\([0-9]+\\)x\000$"
                          my-index)
            (setq my-index (string-to-int (match-string 1 my-index)))
          (setq my-index (string-to-int my-index)))))
  (gethash my-index Textile-tokens))

(defun Textile-clear-tokens ()
  "Clear token hash table."
  (clrhash Textile-tokens))

(defmacro Textile-set-align (my-table my-column alignment)
  "Define table alignment for MY-TABLE, column MY-COLUMN."
  (list 'puthash my-column alignment my-table))

(defmacro Textile-get-align (my-table my-column)
  "Get Textile table alignment for MY-TABLE, column MY-COLUMN."
  (list 'gethash my-column my-table))

(defun Textile-new-table-alignment ()
  "Create table alignment hash table."
  (make-hash-table :test 'eql))

(defmacro Textile-clear-table-alignment (my-table)
  "Clear table alignment hash table."
  (list 'clrhash my-table))

(defun Textile-next-name (my-table-name)
  "Create a new table index name based on the previous one."
  (save-match-data
    (make-symbol
     (Textile-string-concat
      (append (butlast
               (Textile-split-string
                (symbol-name my-table-name) "-"))
              (list
               (format "%d" (1+ (string-to-int
                                 (car (last (Textile-split-string
                                             (symbol-name my-table-name)
                                             "-")))))))) "-"))))

(defmacro Textile-increment (my-table-name)
  "Create a new table index based on the previous one."
  (list 'setq my-table-name
        (list 'Textile-next-name my-table-name)))

(defvar Textile-output-to-new-buffer t
  "Should Textile output go to a new buffer?")

(defvar Textile-xhtml-version-default "XHTML 1.0 Transitional"
  "What is the default version of XHTML that Textile will produce?")

(defvar Textile-xhtml-version
  Textile-xhtml-version-default
  "Set this to the default unless it's already been set.")

(defvar Textile-xhtml-docstrings
  '("XHTML 1.0 Strict"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    "XHTML 1.0 Transitional"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    "XHTML 1.0 Frameset"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
    "XHTML 1.1"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
  "Standard HTML doctypes so that a Textile document can be self-contained.")

(defvar Textile-tag-re-list
  '(("bq" "blockquote" "p") ("p" "p") ("h1" "h1") ("h2" "h2") ("h3" "h3")
    ("h4" "h4") ("h5" "h5"))
  "List of Textile tags and their HTML counterparts.")

(defvar Textile-tags
  (regexp-opt (mapcar 'car Textile-tag-re-list) t)
  "Generated regular expression for Textile block tags.")

(defvar Textile-inline-tag-list
  '(("*" "strong") ("_" "em") ("**" "b") ("__" "i") ("++" "big")
    ("--" "small") ("-" "del") ("+" "ins") ("^" "sup") ("~" "sub")
    ("%" "span") ("@" "code") ("??" "cite"))
  "Link textile to HTML tags for inline formatting.")

(defvar Textile-inline-tags
  (regexp-opt (mapcar 'car Textile-inline-tag-list) t)
  "Generated regular expression for Textile inline tags.")

(defvar Textile-inline-tag-re
  (concat "\\(?:^\\|\\W\\)" Textile-inline-tags
          "\\([^\000]+?\\)\\(\\1\\)\\(?:$\\|\\W\\)")
  "This will match any inline tag and what has been tagged.")

(defvar Textile-escape-tag-re
  (concat "\\(^\\|\\W\\)=="
          "\\([^\000]+?\\)==\\($\\|\\W\\)")
  "This should only match inline escaped code.")

(defvar Textile-alias-list-defaults nil
  "Standard link aliases.
For each string to match should be either a string which is the URL, or
a list whose car is the title and cadr is the URL.")

(defvar Textile-macros-list-defaults
  '(("\\(->\\)" "&#8594;") ("\\((C)\\)" "&#169;") ("\\((R)\\)" "&#174;")
    ("\\((TM)\\)" "&#8482;") ("\\(x\\)[0-9]" "&#215;") ; 3x3
    ("\\(<-\\)" "&#8592;"))
  "Code to be automatically converted to HTML entities or other things.")

(defvar Textile-smart-quotes-list
  '(("\\(?:^\\| \\)\\(--\\)\\(?: \\|$\\)" "&#8212;") ; em-dash
    ("\\(?:^\\| \\)\\(-\\)\\(?: \\|$\\)" "&#8211;") ; en-dash
    ("\\(\\.\\( ?\\)\\.\\2\\.\\)" "&#8230;") ; ellipsis
    ("\\w\\('\\)\\w" "&#8217;") ; word-apostrophe-letter
    ("[^ ]\\('\\)s" "&#8217;") ; special case apostrophe-s
    ("\\('\\)[0-9]\\{2\\}s" "&#8217;") ; decades like the '80s
    (" \\(\"\\)" "&#8220;") ; any double-quote preceded by a space
    (" \\('\\)" "&#8216;") ; any single-quote preceded by a space
    ("\\(\"\\)\\(?: \\|$\\)" "&#8221;") ; any double-quote followed by space
    ("\\('\\)\\(?: \\|$\\)" "&#8217;") ; any single-quote followed by space
    ("[0-9]\\('\\)\\(?:[0-9]\\|x[0-9]\\)" "&#8217;") ; 5'11 works
    ("[0-9]\\(\"\\)\\(?:[0-9]\\|x[0-9]\\)" "&#8221;") ; 5'11" works
    ("\\(\\`\"\\|\"\\b\\)" "&#8220;")
    ("\\(\\b\"\\|\"\n\\|\"\\'\\)" "&#8221;")
    ("\\(\\`'\\|'\\b\\)" "&#8216;")
    ("\\(\\b'\\|'\n\\|'\\'\\)" "&#8217;")
    ("\\(&#8220;'\\)" "&#8220;&#8216;")
    ("\\(&#8216;\"\\)" "&#8216;&#8220;")
    ("\\('&#8221;\\)" "&#8217;&#8221;")
    ("\\(\"&#8217;\\)" "&#8221;&#8217;"))
  "Code to be automatically converted to HTML entities or other things.")

(defvar Textile-alias-list
  nil
  "FIXME: placeholder")

(defun Textile-alias-to-url (lookup alias-list)
  "FIXME: placeholder"
  nil)

(defvar Textile-list-tag-regexp
  "^\\(([^ ]+?)\\|\\)\\([*#]+\\)\\([^ ]*\\) "
  "All list block tags must match this.")

(defun Textile-process-ampersand (my-string)
  "Convert ampersands to &amp; as necessary."
  (save-match-data
    (with-temp-buffer
      (insert my-string)
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "&" nil t)
          (if (looking-at "#?\\w+;")
              (re-search-forward "#?\\w+;" nil t)
            (replace-match "&amp;"))))
      (buffer-string))))

(defun textile-header (title &optional html-version charset &rest headers)
  "Insert HTML header so that Textile documents can be self-contained."
  (let ((my-docstrings Textile-xhtml-docstrings)
        (output ""))
    (unless html-version
      (setq html-version Textile-xhtml-version-default))
    (if (member html-version Textile-xhtml-docstrings)
        (setq Textile-xhtml-version html-version)
      (setq Textile-xhtml-version Textile-xhtml-version-default)
      (setq html-version Textile-xhtml-version-default))
    (while my-docstrings
      (if (string= html-version (car my-docstrings))
          (setq output (cadr my-docstrings)))
      (setq my-docstrings (cddr my-docstrings)))
    (setq output (concat output "\n\n"
                         "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                         "<head>\n"
                         "<meta http-equiv=\"Content-Type\" "
                         "content=\"text/html; charset="))
    (unless charset
      (setq charset "iso-8859-1"))
    (setq output (concat output charset "\" />\n"))
    (setq output (concat output "<meta name=\"generator\" content=\""
                         textile-version ", " (car (Textile-split-string
                                                    (emacs-version) "\n"))
                         "\" />\n"))
    (while headers
      (setq output (concat output (car headers) "\n"))
      (setq headers (cdr headers)))
    (setq output (concat output "<title>" title "</title>\n"))
    (setq output (concat output "</head>\n<body>"))
    output))

(defun textile-footer (&optional &rest footers)
  "Insert HTML footer so that Textile documents can be self-contained."
  (let ((output ""))
    (while footers
      (setq output (concat output (car footers) "\n"))
      (setq footers (cdr footers)))
    (setq output (concat output "</body>\n</html>"))
    output))

(defun textile-version (&optional arg)
  "Version information for this version of textile.el.
If ARG, insert string at point."
  (interactive "P")
  (if arg
      textile-version
    (message textile-version)))

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

(defun Textile-interpret-attributes (context-arg style-arg cite-arg
                                                 &optional my-table
                                                 my-column)
  "Interpret the attributes for block tags and return the actual XHTML
string."
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
              (cond
               ((string= context-arg "table")
                (push "float:right" style))
               ((or (string= context-arg "th")
                    (string= context-arg "td"))
                (setq align "right")
                (if (string= context-arg "th")
                    (Textile-set-align my-table my-column "right")))
               (t
                (push "right" class)
                (push "text-align:right" style)))
              (forward-char 1))
             ((looking-at "<>")
              (push "justify" class)
              (push "text-align:justify" style)
              (forward-char 2))
             ((equal this-char ?\<)
              (cond
               ((string= context-arg "table")
                (push "float:left" style))
               ((or (string= context-arg "th")
                    (string= context-arg "td"))
                (setq align "left")
                (if (string= context-arg "th")
                    (Textile-set-align my-table my-column "left")))
               (t
                (push "left" class)
                (push "text-align:left" style)))
              (forward-char 1))
             ((equal this-char ?\=)
              (if (string= context-arg "table")
                  (progn
                    (push "margin-left: auto" style)
                    (push "margin-right: auto" style))
                (push "text-align:center" style)
                (push "center" class))
              (forward-char 1))
             ((and (or (string= context-arg "tr")
                       (string= context-arg "td")
                       (string= context-arg "th"))
                   (equal this-char ?^ ))
              (setq valign "top")
              (forward-char 1))
             ((and (or (string= context-arg "tr")
                       (string= context-arg "td")
                       (string= context-arg "th"))
                   (equal this-char ?\~))
              (setq valign "bottom")
              (forward-char 1))
             ((and (or (string= context-arg "td")
                       (string= context-arg "th"))
                   (looking-at "[\\]\\([0-9]+\\)"))
              (setq colspan (match-string 1))
              (re-search-forward "[\\]\\([0-9]+\\)" nil t))
             ((and (or (string= context-arg "td")
                       (string= context-arg "th"))
                   (looking-at "/\\([0-9]+\\)"))
              (setq rowspan (match-string 1))
              (re-search-forward "/\\([0-9]+\\)" nil t))
             (t
              (forward-char 1)))))
          (if (> left-pad 0)
              (push (concat "padding-left:" (format "%d" left-pad) "em")
                    style))
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
                                         textile-header
                                         textile-well-formed))
            (when (and (stringp (eval this-variable))
                       (string= (eval this-variable) ""))
              (set this-variable nil)))
          (if class
              (setq return-string (concat return-string " class=\""
                                          (Textile-string-concat class " ")
                                          "\"")))
          (if id
              (setq return-string (concat return-string " id=\"" id "\"")))
          (if align
              (setq return-string (concat return-string " align=\""
                                          align "\""))
            (if (and my-column
                     (string= context-arg "td")
                     (Textile-get-align my-table my-column))
                (setq return-string (concat return-string " align=\""
                                            (Textile-get-align my-table
                                                               my-column)
                                            "\""))))
          (if style
              (setq return-string (concat return-string " style=\""
                                          (Textile-string-concat style ";")
                                          "\"")))
          (if lang
              (setq return-string (concat return-string
                                          (if (string= Textile-xhtml-version
                                                       "XHTML 1.1")
                                              " xml:lang=\""
                                            " lang=\"")
                                          lang "\"")))
          (if colspan
              (setq return-string (concat return-string " colspan=\""
                                          colspan "\"")))
          (if rowspan
              (setq return-string (concat return-string " rowspan=\""
                                          rowspan "\"")))
          ))
      (if (and (string= context-arg "blockquote")
               (> (length cite-arg) 0))
          (setq return-string (concat return-string " cite=\""
                                      cite-arg "\"")))
      return-string)))

(defun Textile-string-concat (my-list separator)
  "Return all strings concatenated with separator between them."
  (if (cdr my-list)
      (concat (car my-list) separator (Textile-string-concat (cdr my-list)
                                                             separator))
    (car my-list)))

(defun Textile-list-level (textile-list-tag)
  "Return number corresponding to level of this list."
  (save-match-data
    (if (string-match Textile-list-tag-regexp textile-list-tag)
        (length (match-string 2 textile-list-tag))
      nil)))

(defun Textile-list-context (textile-list-tag)
  "Return symbol corresponding to context (ol, ul) of this list."
  (save-match-data
    (if (string-match Textile-list-tag-regexp textile-list-tag)
        (let* ((this-tag (match-string 2 textile-list-tag))
               (final-tag (string (elt this-tag
                                       (- (length this-tag) 1)))))
          (cond
           ((string= final-tag "#")
            "ol")
           ((string= final-tag "*")
            "ul")
           (t
            nil))))))

(defun Textile-list-process (my-string)
  "Process Textile list code in MY-STRING, return tokenized text."
  (let ((list-level 0)
        (list-context nil)
        (current-string ""))
    (with-temp-buffer
      (insert my-string)
      (goto-char (point-min))
      (while (re-search-forward Textile-list-tag-regexp nil t)
        (setq current-string "")
        (cond
         ((> (Textile-list-level (match-string 0)) list-level)
          (setq current-string
                (concat current-string
                        (Textile-new-token
                         (concat "<"
                                 (Textile-list-context (match-string 0))
                                 " et_context=\""
                                 (Textile-list-context (match-string 0))
                                 "\" et_style=\""
                                 (match-string 1)
                                 "\" et_cite=\"\">\n"))))
          (setq list-level (Textile-list-level (match-string 0)))
          (setq list-context (Textile-list-context (match-string 0))))
         ((< (Textile-list-level (match-string 0)) list-level)
          (setq current-string
                (concat current-string
                        (Textile-new-token
                         (concat "</" list-context ">\n"))))
          (setq list-context (Textile-list-context (match-string 0)))
          (setq list-level (Textile-list-level (match-string 0))))
         ((not (string= list-context
                        (Textile-list-context (match-string 0))))
          (setq current-string
                (concat current-string
                        (Textile-new-token
                         (concat "</" list-context ">\n"))
                        (Textile-new-token
                         (concat "<"
                                 (Textile-list-context (match-string 0))
                                 " et_context=\""
                                 (Textile-list-context (match-string 0))
                                 "\" et_style=\""
                                 (match-string 1)
                                 "\" et_cite=\"\">\n"))))))
        (setq current-string
              (concat current-string
                      (Textile-new-token
                       (concat "<li et_context=\"li\" et_style=\""
                               (match-string 2)
                               "\" et_cite=\"\">"))))
        (replace-match current-string))
      (buffer-string))))

(defun Textile-table-process (my-string)
  "Process Textile table code in MY-STRING, return tokenized text for each
cell."
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    (if (looking-at "table\\([^|]*\\)\\. *")
        (progn
          (replace-match (Textile-new-token (concat "<table"
                                                    " et_context=\"table\""
                                                    " et_style=\""
                                                    (match-string 1)
                                                    "\" et_cite=\"\">\n")))
          ; start processing rows
          (dolist (this-row
                   (Textile-split-string (delete-and-extract-region
                                          (point) (point-max)) "\n"))
            ; need to read to apply row style here
            (insert
             (with-temp-buffer
               (insert this-row)
               (goto-char (point-min))
               (if (or (looking-at " *\\([^| ]*\\) *")
                       (re-search-forward "^ *\\([^| ]*\\) *" nil t))
                   (replace-match (Textile-new-token
                                   (concat "<tr"
                                           " et_context=\"tr\""
                                           " et_style=\""
                                           (match-string 1)
                                           "\" et_cite=\"\">"))))
               (dolist (this-cell
                        (Textile-split-string
                         (delete-and-extract-region (point) (point-max))
                         "|"))
                ; need to read to apply cell style here
                 (insert
                  (with-temp-buffer
                    (insert this-cell)
                    (goto-char (point-min))
                    (if (looking-at "\\(\\([^. ]*\\)\\. \\|\\) *\\(.+\\) *$")
                        (let ((my-tag
                               (if (and
                                    (match-string 2)
                                    (save-match-data
                                      (string-match "_" (match-string 2))))
                                   "th"
                                 "td")))
                          (replace-match
                           (concat
                            (Textile-new-token
                             (concat "<" my-tag
                                     " et_context=\"" my-tag "\""
                                     " et_style=\""
                                     (match-string 2)
                                     "\" et_cite=\"\">"))
                            (Textile-trim (match-string 3))
                            (Textile-new-token (concat
                                                "</" my-tag ">"))))))
                    (buffer-string))))
               (buffer-string)))
            (insert (Textile-new-token "</tr>\n")))
          ; end table tag
          (goto-char (point-max))
          (insert (Textile-new-token "</table>"))))
    (buffer-string)))

(defun Textile-trim (my-string)
  "Trims extra whitespace from the front and back of MY-STRING."
  (save-match-data
    (if (string-match "^ *\\(.*[^ ]\\) *$" my-string)
        (match-string 1 my-string)
      my-string)))

(defun textile-string (my-string)
  "The workhorse loop.  Take MY-STRING, dump it in a temp buffer, and
  start operating on it."
  (Textile-clear-tokens)
  (let ((Textile-macros-list Textile-macros-list-defaults)
        (my-table (Textile-new-table-alignment))
        (my-table-col 0))
    (with-temp-buffer
      (insert my-string)
    ; begin tokenizing
    ; First we need to remove the \000 strings
      (goto-char (point-min))
      (while (re-search-forward "\000" nil t)
        (replace-match (Textile-new-token (match-string 0))))
    ; first provide for BC's unit test; may remove this later
      (goto-char (point-min))
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
      (goto-char (point-min))
    ; BC one-line comments
      (while (re-search-forward "^//.*$" nil t)
        (replace-match (Textile-new-token (match-string 0))))
    ; inline elisp
      (goto-char (point-min))
      (while (re-search-forward "#\\[(\\(([\000-\177]+?)\\))\\]#" nil t)
        (replace-match
       ; interesting question: tokenize elisp output, or not?
       ; more powerful (and more bug-capable) if we don't;
       ; then we can generate Textile code, not just escaped code
;       (Textile-new-token
         (if noninteractive
             "elisp evaluation not available"
           (let ((my-response
                  (save-match-data
                    (eval (car (read-from-string
                                (match-string 1)))))))
             (if (stringp my-response)
                 my-response
               (format "%S" my-response))))
;        )
         t nil nil 0))
    ; double-equals escapes
      (goto-char (point-min))
      (while (or (looking-at "^==\n")
                 (re-search-forward "^==\n" nil t))
        (replace-match "")
        (let ((start-pos (point))
              (end-pos (if (re-search-forward "^==\n" nil t)
                           (progn
                             (replace-match "")
                             (point))
                         (point-max))))
          (insert (Textile-new-token (delete-and-extract-region
                                      start-pos end-pos)))))
    ; double-equals escapes inline
      (goto-char (point-min))
      (while (or (looking-at Textile-escape-tag-re)
                 (re-search-forward Textile-escape-tag-re nil t))
        (replace-match (Textile-new-token (concat
                                           (match-string 1)
                                           (match-string 2)
                                           (match-string 3)))))
    ; blockcode processor, sticky
      (goto-char (point-min))
      (while (or (looking-at "bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) ")
                 (re-search-forward "^bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) "
                                    nil t))
        (replace-match (Textile-new-token
                        (concat "<pre et_context=\"pre\" et_style=\""
                                (match-string 1) "\" et_cite=\""
                                (match-string 2)
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
                                (match-string 1) "\" et_cite=\""
                                (match-string 2)
                                "\"><code>")))
        (if (looking-at "\\(.*\\)\n\n")
            (replace-match (Textile-new-token (concat (match-string 1)
                                                      "</code></pre>\n\n")))
          (insert (Textile-new-token (concat (delete-and-extract-region
                                              (point) (point-max)
                                              "</code></pre>\n\n"))))))
    ; footnote processor
      (goto-char (point-min))
      (while (or (looking-at "fn\\([0-9]+\\)\\. ")
                 (re-search-forward "^fn\\([0-9]+\\)\\. " nil t))
        (replace-match (Textile-new-token (concat
                                           "<p class=\"footnote\" id=\"fn"
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
                        (concat
                         "<blockquote et_context=\"blockquote\" et_style=\""
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
    ; Fixup tables with "table." codes
      (goto-char (point-min))
      (while (re-search-forward "^\\(table[^.]*\\.\\)\n" nil t)
        (replace-match (concat (match-string 1) " ")))
      (goto-char (point-min))
      (while (or (looking-at "\n\n|.*|")
                 (re-search-forward "\n\n|.*|" nil t))
        (replace-match (concat "table. " (match-string 0))))
    ; find tables, call out to table processor
      (goto-char (point-min))
      (while (or (looking-at "table[^.]*\\. ")
                 (re-search-forward "^table[^.]*\\. " nil t)
                 (re-search-forward "^table[^.]*\\.$" nil t))
        (let ((first-part (match-string 0))
              (second-part "")
              (start-point (point)))
          (replace-match "")
          (setq start-point (point))
          (if (re-search-forward "\n\n" nil t)
              (re-search-backward "\n\n" nil t)
            (goto-char (point-max)))
          (insert (Textile-table-process (concat first-part
                                                 (delete-and-extract-region
                                                  start-point (point)))))))
      ; acronyms
      (setq case-fold-search nil)
      (goto-char (point-min))
      (while (or (looking-at "\\<\\([A-Z]\\{3,\\}\\|[0-9][A-Z]\\{2,\\}\\|[A-Z][0-9A-Z]\\{2,\\}\\)\\((\\(.*?\\))\\|\\)")
                 (re-search-forward
                  "\\<\\([A-Z]\\{3,\\}\\|[0-9][A-Z]\\{2,\\}\\|[A-Z][0-9A-Z]\\{2,\\}\\)\\((\\(.*?\\))\\|\\)"
                  nil t))
        (if (match-string 3)
            (replace-match
             (Textile-new-token
              (concat "<acronym title=\""
                      (match-string 3)
                      "\">"
                      (match-string 1)
                      "</acronym>")))
          (replace-match
           (concat
            (Textile-new-token "<span class=\"caps\">")
            (match-string 1)
            (Textile-new-token "</span>")))))
      (setq case-fold-search t)
    ; links
      (goto-char (point-min))
      (while (or (looking-at "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\(&#[0-9]+;\\)")
                 (re-search-forward "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\(&#[0-9]+;\\)"
                                    nil t)
                 (looking-at "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\([],.;:\"']?\\(?:\000\\| \\|$\\)\\)")
                 (re-search-forward "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\([],.;:\"']?\\(?:\000\\| \\|$\\)\\)"
                                    nil t))
        (let* ((text (match-string 1))
               (url (match-string 2))
               (delimiter (match-string 3))
               (title "")
               (alias-info (Textile-alias-to-url url Textile-alias-list)))
          (if alias-info
              (progn
                (setq title (car alias-info))
                (setq url (cadr alias-info)))
            (save-match-data
              (if (string-match "\\(.*\\) +(\\(.*\\))" text)
                  (progn
                    (setq title (match-string 2 text))
                    (setq text (match-string 1 text))))))
          (if (string= title "")
              (setq title nil))
          (replace-match (concat
                          (Textile-new-token
                           (concat "<a href=\""
                                   (Textile-process-ampersand url)
                                   "\""
                                   (if title
                                       (concat " title=\"" title
                                               "\""))
                                   ">"))
                          text
                          (Textile-new-token "</a>") delimiter))))
    ; go through Textile tags
      (goto-char (point-min))
      (while (or (looking-at (concat Textile-tags
                                     "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) "))
                 (re-search-forward (concat "^" "\\(?:" Textile-token-re
                                            "\\|\\)"
                                            Textile-tags
                                            "\\([^.]*\\)\\.\\(:[^ ]*\\|\\) ")
                                    nil t))
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
      ; lists
      (goto-char (point-min))
      (while (or (looking-at Textile-list-tag-regexp)
                 (re-search-forward Textile-list-tag-regexp
                                    nil t))
        (let ((first-part (match-string 0))
              (second-part "")
              (start-point (point)))
          (replace-match "")
          (setq start-point (point))
          (if (re-search-forward "\n\n" nil t)
              (re-search-backward "\n\n" nil t)
            (goto-char (point-max)))
          (insert (Textile-list-process (concat first-part
                                                (delete-and-extract-region
                                                 start-point (point)))))))
    ; inline footnotes
      (goto-char (point-min))
      (while (or (looking-at "\\[\\([0-9]+\\)\\]")
                 (re-search-forward "\\[\\([0-9]+\\)\\]" nil t))
        (replace-match (Textile-new-token
                        (concat "<sup class=\"footnote\">"
                                "<a href=\"#fn"
                                (match-string 1)
                                "\">"
                                (match-string 1)
                                "</a></sup>"))))
    ; macros and quotes
      (dolist (next-item Textile-smart-quotes-list)
        (goto-char (point-min))
        (while (or (looking-at (car next-item))
                   (re-search-forward (car next-item) nil t))
          (replace-match (Textile-new-token (cadr next-item))
                         t t nil 1)))
      (dolist (next-item Textile-macros-list)
        (goto-char (point-min))
        (while (or (looking-at (car next-item))
                   (re-search-forward (car next-item) nil t))
          (replace-match (Textile-new-token (cadr next-item))
                         t t nil 1)))
    ; inline tags
      (goto-char (point-min))
    ; FIXME: not working so well.  Will probably have to hand-carve the RE
      (while (or (looking-at Textile-inline-tag-re)
                 (re-search-forward Textile-inline-tag-re nil t))
        (let ((my-tag (cadr (assoc (match-string 1) Textile-inline-tag-list))))
          (replace-match (concat (Textile-new-token
                                  (concat "<" my-tag ">"))
                                 (match-string 2)
                                 (Textile-new-token
                                  (concat "</" my-tag ">"))) t t))
        (goto-char (point-min)))
    ; OK, any block that stands alone is a paragraph by now.
    ; So, figure out how to mark standalone paragraphs.
    ; FIXME: this is where the unmarked p tags go
    ; revert tokens
      (goto-char (point-min))
      (while (or (looking-at Textile-token-re)
                 (re-search-forward Textile-token-re nil t))
        (replace-match (Textile-get-token (match-string 0)) t t)
        (goto-char (point-min)))
    ; interpret attributes
      (goto-char (point-min))
      (while (re-search-forward (concat " et_context=\"\\([^\"]*\\)\""
                                        " et_style=\"\\([^\"]*\\)\""
                                        " et_cite=\"\\([^\"]*\\)\"")
                                nil t)
        (if (string= (match-string 1) "table")
            (Textile-clear-table-alignment my-table))
        (if (string= (match-string 1) "tr")
            (setq my-table-col 0))
        (if (or (string= (match-string 1) "th")
                (string= (match-string 1) "td"))
            (setq my-table-col (1+ my-table-col)))
        (replace-match (Textile-interpret-attributes (match-string 1)
                                                     (match-string 2)
                                                     (match-string 3)
                                                     my-table
                                                     my-table-col))
        (goto-char (point-min)))
    (buffer-string))))

(provide 'textile2)