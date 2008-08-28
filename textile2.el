; -*- mode: emacs-lisp; mode: auto-fill; -*-
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

(defvar textile-version "textile2.el v1.99.5"
  "Version number for textile.el.")

(defvar textile-br-all-newlines t
  "Should all single newlines be made into <br /> tags? Defaults to
  yes.  If you don't like that, either change this to nil or use
  longlines.el.")

(defvar Textile-tokens (make-hash-table :test 'eql)
  "Token hash table; this is currently a global, which is probably unwise.")

(defvar Textile-token-re
  "\000e[bi<>][0-9]+x\000"
  "Regular expression for finding a token.")

(defun Textile-new-token (block-or-inline &rest my-strings)
  "Add MY-STRINGS to token hash table; returns one string tag which replaces them."
  (let ((current-index (hash-table-count Textile-tokens))
        (b-or-i (cond
                ((equal block-or-inline 'block)
                 "b")
                ((equal block-or-inline 'inline)
                 "i")
                ((equal block-or-inline 'open-quote)
                 "<")
                ((equal block-or-inline 'close-quote)
                 ">")
                (t
                 (error "Bad argument, %S" block-or-inline)))))
    (puthash current-index (apply 'concat my-strings) Textile-tokens)
    (format "\000e%s%dx\000" b-or-i current-index)))

(defun Textile-get-token (my-index)
  "Get Textile token by hash index; does not change token table."
  (if (stringp my-index)
      (save-match-data
        (if (string-match "^\000e[bi<>]\\([0-9]+\\)x\000$"
                          my-index)
            (setq my-index (string-to-number (match-string 1 my-index)))
          (setq my-index (string-to-number my-index)))))
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
               (delete "" (split-string
                           (symbol-name my-table-name) "-")))
              (list
               (format "%d" (1+ (string-to-number
                                 (car
                                  (last
                                   (delete ""
                                           (split-string
                                            (symbol-name my-table-name)
                                            "-"))))))))) "-"))))

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

(defvar Textile-all-blocks-re
  "\\(?:\`\\|\n\n\\)\\(?:bc\\|bq\\|clear[<>]?\\.\\|p\\|h[1-5]\\|dl\\|==\n\\|[*] \\|[#] \\)"
  "Possible tags that could end a blockcode/blockquote.")

(defvar Textile-tags
  (regexp-opt (mapcar 'car Textile-tag-re-list) t)
  "Generated regular expression for Textile block tags.")

(defvar Textile-inline-tag-list
  '(("*" "strong") ("_" "em") ("**" "b") ("__" "i")
    ("++" "big") ("--" "small") ("-" "del") ("+" "ins") ("^" "sup")
    ("~" "sub") ("%" "span") ("??" "cite"))
  "Link textile to HTML tags for inline formatting.")

(defvar Textile-inline-tags
  (regexp-opt (mapcar 'car Textile-inline-tag-list) t)
  "Generated regular expression for Textile inline tags.")

(defvar Textile-inline-tag-re
  (concat "\\(^\\|\\W\\)" Textile-inline-tags
          "\\(.+?\\)\\(\\2\\)\\($\\|\\W\\)")
  "This will match any inline tag and what has been tagged.")

(defvar Textile-inline-code-re
  "\\(^\\|\\W\\)\\(@\\)\\([^\000]+?\\)\\(\\2\\)\\($\\|\\W\\)"
  "This will match any inline code tag and what has been tagged.")

(defvar Textile-escape-tag-re
  (concat "\\(^\\|\\W\\)=="
          "\\([^\000]+?\\)==\\($\\|\\W\\)")
  "This should only match inline escaped code.")

(defvar Textile-alias-list-defaults nil
  "Standard link aliases.
For each string to match should be either a string which is the URL, or
a list whose car is the title and cadr is the URL.")

(defvar Textile-alias-hash
  (make-hash-table :test 'equal)
  "First empty value for alias hash.")

(defvar Textile-macros-list-defaults
  '(("\\(->\\)" "&#8594;") ("\\((C)\\)" "&#169;") ("\\((R)\\)" "&#174;")
    ("\\((TM)\\)" "&#8482;") ("\\(x\\)[0-9]" "&#215;") ; 3x3
    ("\\(<-\\)" "&#8592;"))
  "Code to be automatically converted to HTML entities or other things.")

(defvar Textile-macros-list
  Textile-macros-list-defaults
  "Code to be automatically converted to HTML entities or other things.")

(defvar Textile-smart-quotes-list
  '(("\\(?:^\\| \\)\\(--\\)\\(?: \\|$\\)" "&#8212;") ; em-dash
    ("\\(?:^\\| \\)\\(-\\)\\(?: \\|$\\)" "&#8211;") ; en-dash
    ("\\(\\.\\( ?\\)\\.\\(?:\\2\\.\\)\\{1,2\\}\\)" "&#8230;") ; ellipsis
    ("\\w\\('\\)\\w" "&#8217;") ; word-apostrophe-letter
    ("[^ ]\\('\\)s" "&#8217;") ; special case apostrophe-s
    ("\\('\\)[0-9]\\{2,4\\}s" "&#8217;") ; decades like the '80s
    ("\\(?: \\|\n\\)\\(\"\\)" "&#8220;") ; any double-quote preceded by a space
    ("\\(?: \\|\n\\)\\('\\)" "&#8216;") ; any single-quote preceded by a space
    ("\\(\"\\)\\(?: \\|$\\)" "&#8221;") ; any double-quote followed by space
    ("\\('\\)\\(?: \\|$\\)" "&#8217;") ; any single-quote followed by space
    ("\000e<[0-9]+x\000\\('\\)" "&#8216;") ; open quote then single
    ("\000e<[0-9]+x\000\\(\"\\)" "&#8220;") ; open quote then double
    ("\\('\\)\000e>[0-9]+x\000" "&#8217;") ; single quote then closed
    ("\\(\"\\)\000e>[0-9]+x\000" "&#8221;") ; double quote then closed
    ("[0-9]\\('\\)\\(?:[0-9]\\|x[0-9]\\)" "&#8217;") ; 5'11 works
    ("[0-9]\\(\"\\)\\(?:[0-9]\\|x[0-9]\\)" "&#8221;") ; 5'11" works
    ("\\(\"\\)\\<" "&#8220;") ; any double-quote before a word
    ("\\('\\)\\<" "&#8216;") ; any single-quote before a word
    ("\\(?:\\>\\|[.?!]\\)\\(\"\\)" "&#8221;") ; any double-quote after a word
    ("\\(?:\\>\\|[.?!]\\)\\('\\)" "&#8217;") ; any single-quote after a word
    ("\\(\"\\)\000ei[0-9]+x\000" "&#8220;") ; any double-quote before inline
    ("\\('\\)\000ei[0-9]+x\000" "&#8216;") ; any single-quote before inline
    ("\000ei[0-9]+x\000\\(\"\\)" "&#8221;") ; any double-quote after inline
    ("\000ei[0-9]+x\000\\('\\)" "&#8217;") ; any single-quote after inline
    ("\\(\\`\"\\|\"\\b\\)" "&#8220;")
    ("\\(\\b\"\\|\"\n\\|\"\\'\\)" "&#8221;")
    ("\\(\\`'\\|'\\b\\)" "&#8216;")
    ("\\(\\b'\\|'\n\\|'\\'\\)" "&#8217;"))
  "Code to be automatically converted to HTML entities or other things.")

(defun Textile-alias-to-url (lookup)
  "Lookup the link associated with LOOKUP in the alias list and
  returns a list containing the title (if any) and the URL."
  (let ((my-alias (gethash lookup Textile-alias-hash)))
    (if my-alias
        (if (listp my-alias)
            my-alias
          (list "" my-alias))
      nil)))

(defvar Textile-list-tag-regexp
  "\\(([^ ]+?)\\|\\)\\([*#]+\\)\\([^\n]+\\)"
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
            (replace-match "&amp;" t))))
      (buffer-string))))

(defun Textile-process-code (my-string)
  "Replace five XML entities in this string."
  (save-match-data
    (with-temp-buffer
      (insert (Textile-process-ampersand my-string))
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "<" nil t)
          (replace-match "&lt;" t)))
      (save-excursion
        (while (re-search-forward ">" nil t)
          (replace-match "&gt;" t)))
      (save-excursion
        (while (re-search-forward "\"" nil t)
          (replace-match "&quot;" t)))
      (save-excursion
        (while (re-search-forward "'" nil t)
          (replace-match "&apos;" t)))
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
                         textile-version ", " (car (delete ""
                                                           (split-string
                                                            (emacs-version) "\n")))
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

(defun Textile-process-non-ascii ()
  "Process non-ASCII chars in this buffer."
  (goto-char (point-min))
  (while (re-search-forward "\\([^\000-\177]+\\)" nil t)
    (replace-match (Textile-non-ascii-to-unicode (match-string 1)))))

(defun Textile-non-ascii-to-unicode (string)
  "Convert STRING to Unicode entities."
  (save-match-data
    (cond
     ((coding-system-p 'utf-16be) ; Emacs 21.4, 22
      (let ((unicode-string (encode-coding-string string
                                                  'utf-16be))
            (unicode-values nil)
            (output ""))
        (setq unicode-values (mapcar 'string-to-char
                                     (delete ""
                                             (split-string
                                              unicode-string ""))))
        (while (cdr unicode-values)
          (setq output (concat output "&#" (number-to-string
                                            (+ (* (car unicode-values) 256)
                                               (cadr unicode-values)))
                               ";"))
          (setq unicode-values (cddr unicode-values)))
        output))
     ((coding-system-p 'utf-16-be) ; Emacs 21.3
      (let ((unicode-string (encode-coding-string string 'utf-16-be))
            (unicode-values nil)
            (output ""))
        (setq unicode-values (mapcar 'string-to-char
                                     (nthcdr 2
                                             (delete ""
                                                     (split-string
                                                      unicode-string "")))))
        (while (cdr unicode-values)
          (setq output (concat output "&#" (number-to-string
                                            (+ (* (car unicode-values) 256)
                                               (cadr unicode-values)))
                               ";"))
          (setq unicode-values (cddr unicode-values)))
        output))
     ((coding-system-p 'utf-16-be-no-signature) ; Mule-UCS
      (let ((unicode-string (encode-coding-string string
                                                  'utf-16-be-no-signature))
            (unicode-values nil)
            (output ""))
        (setq unicode-values (mapcar 'string-to-char
                                     (delete "" (split-string
                                                 unicode-string ""))))
        (while (cdr unicode-values)
          (setq output (concat output "&#" (number-to-string
                                            (+ (* (car unicode-values) 256)
                                               (cadr unicode-values)))
                               ";"))
          (setq unicode-values (cddr unicode-values)))
        output))
     (t
      string))))

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

(defun textile-buffer ()
  "Call textile-code-to-blocks on the entire buffer."
  (interactive)
  (textile-region (point-min) (point-max)))

(defmacro push-assoc (new-item akey my-alist)
  "Push something onto an alist."
  `(if (assoc ,akey ,my-alist)
        (setcdr (assoc ,akey ,my-alist)
                 (append (list ,new-item)
                          (cdr
                            (assoc ,akey ,my-alist))))
     (push (list ,akey ,new-item) ,my-alist)))

(defun Textile-split-attribs (my-string valid-attribs)
  "For MY-STRING, return a list with two strings: valid attributions in
  the first string, and the rest of the line as the cadr."
  (save-match-data
    (let ((attrib-string "")
          (rest-string ""))
      (with-temp-buffer
        (insert my-string)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((this-char (char-after)))
            (if (not (member this-char (append valid-attribs nil)))
                (progn
                  (setq rest-string
                        (concat rest-string
                                (buffer-substring (point)
                                                  (point-max))))
                  (goto-char (point-max)))
              (cond
               ((or
                 (looking-at "{\\([^}]*\\)}")
                 (looking-at "\\[\\(.*?\\)\\]")
                 (looking-at "(\\([^) (]+\\))")
                 (looking-at "<>")
                 (looking-at "[\\]\\([0-9]+\\)")
                 (looking-at "/\\([0-9]+\\)"))
                (setq attrib-string (concat attrib-string (match-string 0)))
                (replace-match ""))
               (t
                (setq attrib-string
                      (concat attrib-string (string this-char)))
                (forward-char 1)))))))
      (if (string-match "^ \\(.*\\)$" rest-string)
          (setq rest-string (match-string 1 rest-string)))
      (list attrib-string rest-string))))

; FIXME: there has to be a way to make split-attribs and interpret-style
;        work together

(defun Textile-interpret-style (my-string valid-attribs)
  "Interpret style string and return the rest of the string and a list
  of the attributes found."
  (save-match-data
    (let (my-list)
      (with-temp-buffer
        (insert my-string)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((this-char (char-after)))
            (if (not (member this-char (append valid-attribs nil)))
                (progn
                  (push (buffer-substring (point) (point-max)) my-list)
                  (goto-char (point-max)))
              (cond
               ((looking-at "{\\([^}]*\\)}")
                (push-assoc (match-string 1) 'style my-list)
                (replace-match ""))
               ((looking-at "\\[\\(.*?\\)\\]")
                (push-assoc (match-string 1) 'lang my-list)
                (replace-match ""))
               ((looking-at "(\\([^) (]+\\))")
                (let ((this-attrib (save-match-data
                                     (delete ""
                                             (split-string
                                              (match-string 1) "#")))))
                  (if (and (string-match "#" (match-string 1))
                           (= (match-beginning 0) 0))
                      (push-assoc (car this-attrib) 'id my-list)
                    (push-assoc (car this-attrib) 'class my-list)
                    (push-assoc (cadr this-attrib) 'id my-list)))
                (re-search-forward ")" nil t))
               ((equal this-char ?\()
                (push-assoc 1 'left-pad my-list)
                (forward-char 1))
               ((equal this-char ?\))
                (push-assoc 1 'right-pad my-list)
                (forward-char 1))
               ((equal this-char ?\>)
                (push-assoc 'right 'alignment my-list)
                (forward-char 1))
               ((looking-at "<>")
                (push-assoc "justify" 'class my-list)
                (push-assoc "text-align:justify" 'style my-list)
                (forward-char 2))
               ((equal this-char ?\<)
                (push-assoc 'left 'alignment my-list)
                (forward-char 1))
               ((equal this-char ?\=)
                (push-assoc 'center 'margin my-list)
                (forward-char 1))
               ((equal this-char ?^ )
                (push-assoc 'top 'valignment my-list)
                (forward-char 1))
               ((equal this-char ?\~)
                (push-assoc 'bottom 'valignment my-list)
                (forward-char 1))
               ((equal this-char ?\_)
                (forward-char 1))
               ((looking-at "[\\]\\([0-9]+\\)")
                (push-assoc (match-string 1) 'colspan my-list)
                (re-search-forward "[\\]\\([0-9]+\\)" nil t))
               ((looking-at "/\\([0-9]+\\)")
                (push-assoc (match-string 1) 'rowspan my-list)
                (re-search-forward "/\\([0-9]+\\)" nil t))
               ((equal this-char ?-)
                (push-assoc "vertical-align: middle" 'style my-list)
                (forward-char 1))
               (t
                (push (buffer-substring (point) (point-max)) my-list)
                (goto-char (point-max)))))))
        (if (and
             (eobp)
             (listp (car my-list)))
            (push "" my-list)))
      my-list)))

(defvar Textile-valid-attribs
  '((default "{[()<>=") ("table" "{[()<>=^~\\/-")
    ("th" "{[()<>=^~\\/-_") ("td" "{[()<>=^~\\/-")
    ("img" "{[()<>=^~") ("strong" "{") ("em" "{") ("b" "{") ("i" "{")
    ("big" "{") ("small" "{") ("del" "{") ("ins" "{") ("sup" "{")
    ("sub" "{") ("span" "{[()") ("cite" "{"))
  "Characters which can start an attrib string in a Textile tag.")

(defun Textile-get-valid-attribs (my-tag)
  "Return a string of valid attribute chars for MY-TAG."
  (cadr
   (or
    (assoc my-tag Textile-valid-attribs)
    (assoc 'default Textile-valid-attribs))))

(defmacro cdr-assoc (my-symbol my-alist)
  "Return the CDR of key MY-SYMBOL from MY-ALIST."
  `(cdr (assoc ,my-symbol ,my-alist)))

(defmacro cadr-assoc (my-symbol my-alist)
  "Return the CADR of key MY-SYMBOL from MY-ALIST."
  `(cadr (assoc ,my-symbol ,my-alist)))

(defun Textile-interpret-attributes (context-arg style-arg cite-arg
                                                 &optional my-table
                                                 my-column)
  "Interpret the attributes for block tags and return the actual XHTML
string."
  (save-match-data
    (let* ((return-string "")
           (my-attribs (Textile-interpret-style
                        style-arg
                        (Textile-get-valid-attribs context-arg)))
           (string-rest (car my-attribs))
           (my-plist (cdr my-attribs)))
      (if (string-match "^:" cite-arg)
          (setq cite-arg (replace-match "" t t cite-arg)))
      (let ((style (cdr-assoc 'style my-plist))
            (class (cdr-assoc 'class my-plist))
            (id (cadr-assoc 'id my-plist))
            ; note that id and lang should both have only one value
            (lang (cadr-assoc 'lang my-plist))
            (left-pad 0)
            (right-pad 0)
            (align (cadr-assoc 'alignment my-plist))
            (valign nil)
            (rowspan (cadr-assoc 'rowspan my-plist))
            (colspan (cadr-assoc 'colspan my-plist))
            (textile-header nil)
            (not-finished t)
            (height nil)
            (width nil)
            (src nil)
            (textile-well-formed t))
        (when (cdr-assoc 'left-pad my-plist)
          (setq left-pad (apply '+ (cdr-assoc 'left-pad my-plist)))
          (push-assoc (format "padding-left:%dem" left-pad)
                      'style my-plist))
        (when (cdr-assoc 'right-pad my-plist)
          (setq right-pad (apply '+ (cdr-assoc 'right-pad my-plist)))
          (push-assoc (format "padding-right:%dem" right-pad)
                      'style my-plist))
        (when (and
               (or
                (> left-pad 0)
                (> right-pad 0))
               (assoc 'style my-plist))
          (when (member "text-align:left" (assoc 'style my-plist))
            ; FIXME: need a delete-assoc macro?
            (setq style (delete "text-align:left" style))
            (push-assoc "float:left" 'style my-plist))
          (when (member "text-align:right" (assoc 'style my-plist))
            ; FIXME: need a delete-assoc macro?
            (setq style (delete "text-align:right" style))
            (push-assoc "float:right" 'style my-plist)))
        (when (equal align 'left)
          (cond
           ((string= context-arg "table")
            (push-assoc "float:left" 'style my-plist))
           ((or (string= context-arg "th")
                (string= context-arg "td"))
            (setq align "left")
            (if (string= context-arg "th")
                (Textile-set-align my-table my-column "left")))
           (t
            (push-assoc "left" 'class my-plist)
            (push-assoc "text-align:left" 'style my-plist))))
        (when (equal (cadr-assoc 'margin my-plist) 'center)
          (cond
           ((string= context-arg "table")
            (push-assoc "margin-left: auto" 'style my-plist)
            (push-assoc "margin-right: auto" 'style my-plist))
           ((or
             (string= context-arg "th")
             (string= context-arg "td"))
            (setq align "center"))
           (t
            (push-assoc "center" 'class my-plist)
            (push-assoc "text-align:center" 'style my-plist))))
        (when (equal align 'right)
          (cond
           ((string= context-arg "table")
            (push-assoc "float:right" 'style my-plist))
           ((or (string= context-arg "th")
                (string= context-arg "td"))
            (setq align "right")
            (if (string= context-arg "th")
                (Textile-set-align my-table my-column "right")))
           (t
            (push-assoc "right" 'class my-plist)
            (push-assoc "text-align:right" 'style my-plist))))
        (when (equal (cdr-assoc 'valignment my-plist) 'top)
          (cond
           ((or (string= context-arg "tr")
                (string= context-arg "th")
                (string= context-arg "td"))
            (setq valign "top"))
           ((string= context-arg "img")
            (push-assoc "vertical-align: text-top" 'style my-plist))))
        (when (equal (cdr-assoc 'valignment my-plist) 'bottom)
          (cond
           ((or (string= context-arg "tr")
                (string= context-arg "th")
                (string= context-arg "td"))
            (setq valign "bottom"))
           ((string= context-arg "img")
            (push-assoc "vertical-align: text-bottom" 'style
                                                 my-plist))))
        (setq style (cdr-assoc 'style my-plist)
              class (cdr-assoc 'class my-plist))
        (dolist (this-variable '(style class id lang align valign
                                       colspan rowspan
                                       textile-header
                                       textile-well-formed))
          (when (and (stringp (eval this-variable))
                     (string= (eval this-variable) ""))
            (set this-variable nil)))
        (if (and (string= context-arg "img")
                 (setq src string-rest))
            (let* ((image-data (split-string src " "))
                   (image-dim (if (cadr image-data)
                                  (split-string (cadr image-data) "x")
                                nil)))
              (setq return-string (concat return-string " src=\""
                                          (car image-data) "\""))
              (if (> (safe-length image-dim) 1)
                  (setq width (car image-dim)
                        height (cadr image-dim))
                (dolist (this-parm (cdr image-data))
                  (if (string-match "^\\(.*\\)w$" this-parm)
                      (setq width (match-string 1 this-parm))
                    (if (string-match "^\\(.*\\)h$" this-parm)
                        (setq height (match-string 1 this-parm))))))))
        (if width
            (setq return-string (concat return-string " width=\""
                                        width "\"")))
        (if height
            (setq return-string (concat return-string " height=\""
                                        height "\"")))
        (if class
            (setq return-string (concat return-string " class=\""
                                        (Textile-string-concat class " ")
                                        "\"")))
        (if id
            (setq return-string (concat return-string " id=\"" id "\"")))
        (if (and align (stringp align))
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
                                        rowspan "\""))))
      (if (and (string= context-arg "img")
               (> (length cite-arg) 0))
          (setq return-string (concat return-string " alt=\""
                                      cite-arg "\"")))
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
  "Return list of HTML tags corresponding to list context (ol, ul)."
  (let ((my-list)
        (string-list (delete "" (split-string textile-list-tag ""))))
    (while string-list
      (cond
       ((string= (car string-list) "#")
        (push (copy-sequence "ol") my-list))
       ((string= (car string-list) "*")
        (push (copy-sequence "ul") my-list)))
      (pop string-list))
    my-list))

(defun Textile-list-difference (small-list large-list)
  (if (> (length small-list)
         (length large-list))
      (error "Negative result, LARGE-LIST smaller than SMALL-LIST"))
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

(defun Textile-open-list (tag style)
  (Textile-new-token 'block
   "<" tag " et_context=\""
   tag "\" et_style=\""
   style "\" et_cite=\"\">"))

(defun Textile-close-list (tag)
  (Textile-new-token 'block
   "</" tag ">"))

(defun Textile-list-item (context style)
  (Textile-new-token 'block
   "<li et_context=\"" context
   "\" et_style=\"" style
   "\" et_cite=\"\">"))

(defun Textile-close-list-item ()
  (Textile-new-token 'block "</li>"))

(defun Textile-def-list-process (my-string)
  "Process Textile definition list code in MY-STRING, return
tokenized text."
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^:\n]+\\):" nil t)
      (replace-match (concat (Textile-new-token 'block "<dt>")
                             (match-string 1)
                             (Textile-new-token 'block "</dt>")
                             "\n"
                             (Textile-new-token 'block "<dd>"))
                     t)
      (save-excursion
        (if (re-search-forward "\n\\([^:\n]+:\\)" nil t)
            (replace-match (concat (Textile-new-token 'block "</dd>")
                                   "\n"
                                   (match-string 1))
                           t)
          (goto-char (point-max))
          (insert (Textile-new-token 'block "</dd>")))))
    (goto-char (point-max))
    (insert "\n" (Textile-new-token 'block "</dl>"))
    (buffer-string)))

(defun Textile-list-process (my-string)
  "Process Textile list code in MY-STRING, return tokenized text."
  (let ((list-level nil)
        (current-string ""))
    (with-temp-buffer
      (insert my-string)
      (goto-char (point-min))
      (while (re-search-forward Textile-list-tag-regexp nil t)
        (setq current-string "")
        (save-match-data
          (let* ((tag-string (match-string 2))
                 (list-style (match-string 1))
                 (item-split (Textile-split-attribs
                              (match-string 3)
                              (Textile-get-valid-attribs "li")))
                 (item-style (car item-split))
                 (rest-of-item (cadr item-split))
                 (current-list-context (Textile-list-context tag-string)))
            ; list level is dropping
            (if (< (length current-list-context)
                   (length list-level))
                (progn
                  (dolist (close-tag (Textile-list-difference current-list-context
                                                              list-level))
                    (setq current-string
                          (concat current-string
                                  (Textile-close-list close-tag)
                                  "\n"
                                  (Textile-close-list-item)
                                  "\n"))
                    (pop list-level))))
            ; list level is increasing
            (if (> (length current-list-context)
                   (length list-level))
                (progn
                  (setq current-string
                        (concat current-string
                                (Textile-open-list
                                 (car current-list-context)
                                 list-style)
                                "\n"
                                (Textile-list-item
                                 (car current-list-context)
                                 item-style)
                                rest-of-item))
                  (push (car current-list-context) list-level))
              ; no change in list level
              (save-excursion
                (save-match-data
                  (beginning-of-line)
                  (re-search-backward "\\([^\n]\\)" nil t)
                  (replace-match (concat (match-string 0)
                                         (Textile-new-token 'block
                                                            "</li>"))
                                 t t)))
              (setq current-string
                    (concat current-string
                            (Textile-list-item
                             (car current-list-context)
                             item-style)
                            rest-of-item)))))
        (replace-match current-string t t))
      (goto-char (point-max))
      (while list-level
        (insert (Textile-close-list-item) "\n" (Textile-close-list (pop list-level))))
      (buffer-string))))

(defun Textile-table-process (my-string)
  "Process Textile table code in MY-STRING, return tokenized text for each
cell."
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    (if (looking-at "table\\([^|]*\\)\\. *")
        (progn
          (replace-match (Textile-new-token 'block
                                            "<table"
                                            " et_context=\"table\""
                                            " et_style=\""
                                            (match-string 1)
                                            "\" et_cite=\"\">\n")
                         t)
          ; start processing rows
          (dolist (this-row
                   (delete ""
                           (split-string (delete-and-extract-region
                                          ; this is wrong; splitting on
                                          ; a newline means no more
                                          ; newlines inside table cells
                                          (point) (point-max)) "\n")))
            ; need to read to apply row style here
            (insert
             (with-temp-buffer
               (insert this-row)
               (goto-char (point-min))
               (if (or (looking-at " *\\([^| ]*\\) *")
                       (re-search-forward "^ *\\([^| ]*\\) *" nil t))
                   (replace-match (Textile-new-token
                                   'block
                                   "<tr"
                                   " et_context=\"tr\""
                                   " et_style=\""
                                   (match-string 1)
                                   "\" et_cite=\"\">") t))
               (dolist (this-cell
                        (delete ""
                                (split-string
                                 (delete-and-extract-region (point) (point-max))
                         "|")))
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
                             'block
                             "<" my-tag
                             " et_context=\"" my-tag "\""
                             " et_style=\""
                             (match-string 2)
                             "\" et_cite=\"\">")
                            (Textile-trim (match-string 3))
                            (Textile-new-token 'block "</" my-tag ">"))
                           t)))
                    (buffer-string))))
               (buffer-string)))
            (insert (Textile-new-token 'block "</tr>\n")))
          ; end table tag
          (goto-char (point-max))
          (insert (Textile-new-token 'block "</table>"))))
    (buffer-string)))

(defun Textile-trim (my-string)
  "Trims extra whitespace from the front and back of MY-STRING."
  (save-match-data
    (if (string-match "^ *\\(.*[^ ]\\) *$" my-string)
        (match-string 1 my-string)
      my-string)))

(defmacro with-crs-temp-buffer (&rest body)
  "I need to be able to see what's happening in this buffer - debug
purposes only!"
  (declare (debug (&rest form)))
  `(with-current-buffer
     (get-buffer-create "*crs-temp*")
     (erase-buffer)
     ,@body))

(defun Textile-tokenize-ascii-zero ()
  "Tokenize all ASCII-char=zero values in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\000" nil t)
    (replace-match (Textile-new-token 'inline (match-string 0)) t)))

(defun Textile-BC-unit-test ()
  "Tokenize escapes for Brad Choate's unit testing."
  (goto-char (point-min))
  (while (re-search-forward "^==>" nil t)
    (let ((start-pos (save-excursion
                       (beginning-of-line)
                       (point)))
          (end-pos nil))
      (if (re-search-forward "-----$" nil t)
          (setq end-pos (point))
        (setq end-pos (point-max)))
      (insert (Textile-new-token 'block
                                 (delete-and-extract-region
                                  start-pos end-pos)))))
  (goto-char (point-min))
      ; BC one-line comments
  (while (re-search-forward "^//.*$" nil t)
    (replace-match (Textile-new-token 'block (match-string 0)) t)))

(defun Textile-inline-elisp-process ()
  "Process inline elisp evaluation in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "#\\[(\\(([\000-\177]+?)\\))\\]#" nil t)
    (replace-match
     (if noninteractive
         "elisp evaluation not available"
       (let ((my-response
              (save-match-data
                (eval (car (read-from-string
                            (match-string 1)))))))
         (if (stringp my-response)
             my-response
           (format "%S" my-response))))
     t nil nil 0)))

(defun Textile-escape-double-equals-blocks ()
  "Tokenize double-equals blocks."
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
      (insert (Textile-new-token 'block
                                 (delete-and-extract-region
                                  start-pos end-pos))))))

(defun Textile-escape-double-equals-inline ()
  "Tokenize double-equals inlines."
  (goto-char (point-min))
  (while (or (looking-at Textile-escape-tag-re)
             (re-search-forward Textile-escape-tag-re nil t))
    (replace-match (Textile-new-token 'inline
                                      (match-string 1)
                                      (match-string 2)
                                      (match-string 3))
                   t)))

(defun Textile-blockcode-sticky ()
  "Process sticky blockcode blocks."
  (goto-char (point-min))
  (while (or (looking-at "bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) ")
             (re-search-forward "^bc\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) "
                                nil t))
    (replace-match (Textile-new-token
                    'block
                    "<pre et_context=\"pre\" et_style=\""
                    (match-string 1) "\" et_cite=\""
                    (match-string 2)
                    "\"><code>")
                   t)
    (let ((start-point (point))
          (end-point (if (re-search-forward
                          ; go until you find either a normal block
                          ; token or a sticky blockquote, which we'll
                          ; replace next
                          "\n\n\\(?:\000eb[0-9]+x\000\\|bq[^\n]?\.\.\\)" nil t)
                         (progn
                           (re-search-backward "\n\n" nil t)
                           (point))
                       (goto-char (point-max))
                       (point))))
      (insert (Textile-new-token 'block
                                 (Textile-process-code
                                  (delete-and-extract-region
                                   start-point end-point))
                                 "</code></pre>")))))

(defun Textile-blockcode ()
  "Process single blockcode blocks."
  (goto-char (point-min))
  (while (or (looking-at "bc\\([^.]*\\)\\.\\(:[^ ]*\\|\\) ")
             (re-search-forward "^bc\\([^.]*\\)\\.\\(:[^ ]*\\|\\) " nil t))
    (replace-match (Textile-new-token
                    'block
                    "<pre et_context=\"pre\" et_style=\""
                    (match-string 1) "\" et_cite=\""
                    (match-string 2)
                    "\"><code>")
                   t)
    (if (looking-at "\\(.*\\)\n\n")
        (replace-match (Textile-new-token 'block
                                          (Textile-process-code
                                           (match-string 1))
                                          "</code></pre>\n\n")
                       t)
      (insert (Textile-new-token 'block
                                 (Textile-process-code
                                  (delete-and-extract-region
                                   (point) (point-max)))
                                 "</code></pre>\n\n")))))

(defun Textile-footnotes ()
  "Process Textile footnote blocks in this buffer."
  (goto-char (point-min))
  (while (or (looking-at "fn\\([0-9]+\\)\\. ")
             (re-search-forward "^fn\\([0-9]+\\)\\. " nil t))
    (replace-match (Textile-new-token 'block
                                      "<p class=\"footnote\" id=\"fn"
                                      (match-string 1)
                                      "\"><sup>"
                                      (match-string 1)
                                      "</sup> ")
                   t)
    (if (re-search-forward "\n\n" nil t)
        (replace-match (concat (Textile-new-token 'block
                                                  "</p>") "\n\n")
                       t)
      (goto-char (point-max))
      (insert (Textile-new-token 'block "</p>")))))

(defun Textile-blockquote-sticky ()
  "Process sticky blockquote blocks in this buffer."
  (goto-char (point-min))
  (while (or (looking-at "bq\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) ")
             (re-search-forward "bq\\([^.]*\\)\\.\\.\\(:[^ ]*\\|\\) " nil t))
    (replace-match (Textile-new-token
                    'block
                    "<blockquote et_context=\"blockquote\" et_style=\""
                    (match-string 1) "\" et_cite=\"" (match-string 2)
                    "\"><p>")
                   t)
    (let* ((start-point (point))
           (end-point (if (re-search-forward
                          ; go until you find a normal block token
                           "\n\n\000eb[0-9]+x\000" nil t)
                          (progn
                            (re-search-backward "\n\n" nil t)
                            (point))
                        (goto-char (point-max))
                        (point)))
           (block-string (buffer-substring start-point end-point)))
      (delete-region start-point end-point)
      (with-temp-buffer
        (insert block-string)
        (goto-char (point-min))
        (while (re-search-forward "\n\n" nil t)
          (replace-match (Textile-new-token 'block "</p>\n\n<p>") t))
        (setq block-string (buffer-string)))
      (insert block-string
              (Textile-new-token 'block "</p></blockquote>")))))

(defun Textile-table-fixup ()
  "Add Textile-style table tags to untagged tables."
  (goto-char (point-min))
  (while (re-search-forward "^\\(table[^.]*\\.\\)\n" nil t)
    (replace-match (concat (match-string 1) " ") t))
  (goto-char (point-min))
  (while (or (looking-at "|[^\n]*|")
             (re-search-forward (concat "\\(\`\\|\n\n\\)"
                                        "\\(|.*?|\\)") nil t))
    (replace-match (concat (match-string 1)
                           "table. " (match-string 2)) t)))

(defun Textile-table-replace ()
  "Replace Textile tables in this buffer with tokenized code."
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
                                              start-point (point))))))))

(defun Textile-acronyms ()
  "Textile process acronyms in this buffer."
  (setq case-fold-search nil)
  (goto-char (point-min))
  (while (or (looking-at "\\<\\([A-Z]\\{3,\\}\\|[0-9][A-Z]\\{2,\\}\\|[A-Z][0-9A-Z]\\{2,\\}\\)\\((\\(.*?\\))\\|\\)")
             (re-search-forward
              "\\<\\([A-Z]\\{3,\\}\\|[0-9][A-Z]\\{2,\\}\\|[A-Z][0-9A-Z]\\{2,\\}\\)\\((\\(.*?\\))\\|\\)"
              nil t))
    (if (match-string 3)
        (replace-match
         (Textile-new-token 'inline
                            "<acronym title=\""
                            (match-string 3)
                            "\">"
                            (match-string 1)
                            "</acronym>")
         t)
      (replace-match
       (concat
        (Textile-new-token 'inline
                           "<span class=\"caps\">")
        (match-string 1)
        (Textile-new-token 'inline "</span>"))
       t)))
  (setq case-fold-search t))

(defun Textile-images ()
  "Process Textile images in this buffer."
  (goto-char (point-min))
  (while
      (re-search-forward
       "\\(!\\([^ \n][^!\n]+?\\) *\\((\\([^\n]*?\\))\\)?!\\)\\(:\\([^ ]*?\\)\\([,.;:]?\\(?: \\|$\\)\\)\\)?"
       nil t)
    ; wow.  1 = the image code
    ;       2 = data (URL, style) of image
    ;       3 = title code with parentheses
    ;       4 = title code sans parentheses
    ;       5 = all code following image
    ;       6 = link if it exists
    ;       7 = delimiter if it exists
    (let ((image-code (match-string 1))
          (image-data (match-string 2))
          (title-with-parens (match-string 3))
          (title (match-string 4))
          (link-code (match-string 5))
          (link-url (match-string 6))
          (delimiter (match-string 7)))
      (replace-match (concat
                      (if link-url
                          "\"")
                      (Textile-image-process image-data title)
                      (if link-url
                          (concat "\":" link-url delimiter)
                        link-code)) t))))

(defun Textile-image-process (image-data title)
  "Return Textile token for image at IMAGE-DATA, with TITLE."
  (Textile-new-token 'inline
                     "<img et_context=\"img\""
                     " et_style=\"" image-data
                     "\" et_cite=\"" title
                     "\"/>"))

(defun Textile-links ()
  "Process Textile links in this buffer."
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
           (alias-info (Textile-alias-to-url url)))
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
                       'inline
                       "<a href=\""
                       (Textile-process-ampersand url)
                       "\""
                       (if title
                           (concat " title=\"" title
                                   "\""))
                       ">")
                      text
                      (Textile-new-token 'inline "</a>") delimiter)
                     t t))))

(defun Textile-definition-lists ()
  "Process Textile definition lists in this buffer."
  (goto-char (point-min))
  (while (or (looking-at "^dl\\([^.]*\\)\\. ")
             (re-search-forward "^dl\\([^.]*\\)\\. " nil t))
    (let ((my-style (match-string 1)))
      (replace-match (Textile-new-token 'block
                                        "<dl et_context=\"dl\""
                                        " et_style=\"" my-style
                                        "\" et_cite=\"\">\n")
                     t)
      (insert
       (Textile-def-list-process
        (delete-and-extract-region
         (point)
         (if (re-search-forward "\n\n" nil t)
             (progn
               (re-search-backward "\n\n" nil t)
               (point))
           (point-max))))))))

(defun Textile-blocks ()
  "Process block Textile tags in this buffer."
  (goto-char (point-min))
  (while (or (looking-at (concat Textile-tags
                                 "\\([^.\n]*\\)\\.\\(:[^ ]*\\|\\) "))
             (re-search-forward (concat "^" "\\(?:" Textile-token-re
                                        "\\|\\)"
                                        Textile-tags
                                        "\\([^.\n]*\\)\\.\\(:[^ ]*\\|\\) ")
                                nil t))
    (let* ((my-tags (cdr (assoc (match-string 1) Textile-tag-re-list)))
           (my-1st-tag (car my-tags))
           (my-2nd-tag (cadr my-tags))
           (my-close-tag (concat (if my-2nd-tag
                                     (concat "</" my-2nd-tag ">"))
                                 "</" my-1st-tag ">")))
      (replace-match (Textile-new-token 'block
                                        "<" my-1st-tag
                                        " et_context=\""
                                        my-1st-tag
                                        "\" et_style=\""
                                        (match-string 2)
                                        "\" et_cite=\""
                                        (match-string 3) "\">"
                                        (if my-2nd-tag
                                            (concat "<"
                                                    my-2nd-tag
                                                    ">")))
                     t)
      (if (re-search-forward "\n\n" nil t)
          (replace-match (concat (Textile-new-token 'block
                                                    my-close-tag) "\n\n")
                         t)
        (goto-char (point-max))
        (insert (Textile-new-token 'block
                                   my-close-tag))))))

(defun Textile-lists ()
  "Process Textile lists in this buffer."
  (goto-char (point-min))
  (while (or (looking-at Textile-list-tag-regexp)
             (re-search-forward (concat "\\(?:\`\\|\n\n\\)"
                                        Textile-list-tag-regexp)
                                nil t))
    (let ((first-part (match-string 0))
          (second-part "")
          (start-point (point)))
      (replace-match "")
      (setq start-point (point))
      (if (re-search-forward "\n\n" nil t)
          (re-search-backward "\n\n" nil t)
        (goto-char (point-max)))
      (let ((possible-list (concat first-part
                                   (buffer-substring start-point (point)))))
        (if (and (= (length (delete "" (split-string possible-list
                                                     "\n"))) 1)
                 (save-match-data
                   (and
                    (string-match Textile-inline-tag-re possible-list)
                    (string= (match-string 2 possible-list) "*"))))
            (insert first-part)
          (insert (Textile-list-process (concat first-part
                                                (delete-and-extract-region
                                                 start-point (point))))))))))

(defun Textile-inline-footnotes ()
  "Process Textile inline footnotes in this buffer."
  (goto-char (point-min))
  (while (or (looking-at "\\[\\([0-9]+\\)\\]")
             (re-search-forward "\\[\\([0-9]+\\)\\]" nil t))
    (replace-match (Textile-new-token
                    'inline
                    "<sup class=\"footnote\">"
                    "<a href=\"#fn"
                    (match-string 1)
                    "\">"
                    (match-string 1)
                    "</a></sup>")
                   t)))

(defun Textile-macros ()
  "Process Textile macros in this buffer."
  (dolist (next-item Textile-macros-list)
    (goto-char (point-min))
    (while (or (looking-at (car next-item))
               (re-search-forward (car next-item) nil t))
      (replace-match (Textile-new-token 'inline
                                        (cadr next-item))
                     t t nil 1))))

(defun Textile-quotes ()
  "Process Textile quote marks in this buffer."
  (dolist (next-item Textile-smart-quotes-list)
    (goto-char (point-min))
    (while (or (looking-at (car next-item))
               (re-search-forward (car next-item) nil t))
      (let* ((next-quote (cadr next-item))
             (open-or-close (cond
                             ((or
                               (string= next-quote "&#8220;")
                               (string= next-quote "&#8216;"))
                              'open-quote)
                             ((or
                               (string= next-quote "&#8221;")
                               (string= next-quote "&#8217;"))
                              'close-quote)
                             (t
                              'inline))))
        (replace-match (Textile-new-token open-or-close next-quote)
                       t t nil 1))
      (goto-char (point-min)))))

(defun Textile-inlines ()
  "Process Textile inline tags in this buffer."
  (goto-char (point-min))
  (while (or (looking-at Textile-inline-tag-re)
             (re-search-forward Textile-inline-tag-re nil t))
    (let ((inner-text (match-string 3)))
      (if (save-match-data
            (string-match "\000eb[0-9]+x\000" inner-text))
          (replace-match (Textile-new-token 'inline
                                            (match-string 2))
                         t t nil 2)
        (let* ((my-tag (cadr (assoc (match-string 2)
                                    Textile-inline-tag-list)))
               (my-tag-valid-attribs (Textile-get-valid-attribs my-tag))
               (text-split (Textile-split-attribs
                            inner-text my-tag-valid-attribs))
               (text-style (car text-split))
               (rest-of-text (cadr text-split)))
          (replace-match (concat (match-string 1)
                                 (Textile-new-token 'inline
                                                    "<"
                                                    my-tag
                                                    " et_context=\""
                                                    my-tag
                                                    "\" et_style=\""
                                                    text-style
                                                    "\" et_cite=\"\">")
                                 rest-of-text
                                 (Textile-new-token 'inline "</" my-tag ">")
                                 (match-string 5))
                         t t))))
    (goto-char (point-min))))

(defun Textile-inline-code ()
  "Process Textile inline code tags in this buffer."
  (goto-char (point-min))
  (while (or (looking-at Textile-inline-code-re)
             (re-search-forward Textile-inline-code-re nil t))
    (replace-match (concat (match-string 1)
                           (Textile-new-token
                            'inline
                            "<code>"
                            (Textile-process-code (match-string 3))
                            "</code>")
                           (match-string 5)) t t)
    (goto-char (point-min))))

(defun Textile-unmarked-paragraphs ()
  "Process unmarked paragraphs in this buffer."
  (goto-char (point-min))
  (when (looking-at "\\([^\000\n]\\|\000e[i<>][0-9]+x\000\\)")
    (replace-match (concat (Textile-new-token 'block "<p>")
                           (match-string 1))
                   t)
    (save-excursion
          (if (re-search-forward "\n\\{2,\\}" nil t)
              (replace-match (concat
                              (Textile-new-token 'block
                                                 "</p>")
                              (match-string 0)) t))))
  (while (re-search-forward
          "\\(\n\\{2,\\}\\)\\([^\000\n]\\|\000e[i<>][0-9]+x\000\\)"
          nil t)
    (replace-match (concat (match-string 1)
                           (Textile-new-token 'block "<p>")
                           (match-string 2))
                   t)
    (save-excursion
      (if (re-search-forward "\n\\{2,\\}" nil t)
          (replace-match (concat
                          (Textile-new-token 'block
                                             "</p>")
                          (match-string 0)) t)))))

(defun Textile-autolink ()
  "Process URLs in this buffer and link them."
  (goto-char (point-min))
  (while (re-search-forward
          "\\(\\(?:https?\\|ftp\\|mailto\\):\\(?://\\)?.*?\\)\\([],.;:\"']?\\(?:\000\\| \\|$\\)\\)"
          nil t)
    (replace-match (Textile-new-token 'inline
                                      "<a href=\""
                                      (Textile-process-ampersand (match-string 1))
                                      "\">"
                                      (Textile-process-ampersand (match-string 1))
                                      "</a>"
                                      (match-string 2)) t)))

(defun Textile-linebreaks ()
  "Turn single linebreaks into <br /> tags if textile-br-all-newlines is
t."
  (when textile-br-all-newlines
    (goto-char (point-min))
    (while (re-search-forward
            "\\([^\n]\\)\n\\(\000e[i<>][0-9]+x\000\\|\000e[^b]\\|[^\n\000]\\)"
            nil t)
      (replace-match (concat (match-string 1)
                             (Textile-new-token 'inline
                                                "<br />\n")
                             (match-string 2)) t))))

(defun Textile-revert-tokens ()
  "Revert all Textile tokens in this buffer."
  (goto-char (point-min))
  (while (or (looking-at Textile-token-re)
             (re-search-forward Textile-token-re nil t))
    (replace-match (Textile-get-token (match-string 0)) t t)
    (goto-char (point-min))))

(defun Textile-compile-attributes ()
  "Interpret attributes locked into HTML tags and Textile strings."
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
                                                 my-table-col)
                   t)
    (goto-char (point-min))))

(defun Textile-aliases ()
  "In the current buffer, read in aliases and replace with empty
strings."
  ; reset alias hash
  (clrhash Textile-alias-hash)
  ; read in defaults, if any
  (dolist (this-pair Textile-alias-list-defaults)
    (puthash (car this-pair) (cadr this-pair) Textile-alias-hash))
  ; now let's read them in from the buffer
  (goto-char (point-min))
  (while (or (looking-at
              "\\(\\)\\[\\([^]]+\\)\\]\\([^ \000]*?\\)\\(?:\n\\|$\\)")
             (re-search-forward
              "\\(\`\\|\n\n\\)\\[\\([^]]+\\)\\]\\([^ \000]*?\\)\\(?:\n\\|$\\)"
              nil t))
    (let* ((alias (save-match-data
                    (delete "" (split-string (match-string 2)
                                             " (\\|)"))))
           (alias-name (car alias))
           (alias-title (cadr alias))
           (url (match-string 3)))
      (puthash alias-name
               (if alias-title
                   (list alias-title url)
                 url)
               Textile-alias-hash))
    (replace-match (match-string 1) t)))

(defun Textile-braces ()
  "In the current buffer, tokenize out the braces, which should activate
  embedded inline tags between them."
  (goto-char (point-min))
  (while (re-search-forward "\\[\\([^]]+\\)\\]" nil t)
    (replace-match
     (concat (Textile-new-token 'inline "")
             (match-string 1)
             (Textile-new-token 'inline "")) t)))

(defun Textile-clears ()
  "Find the next block token after the clear, dig it out, and add the
  \"clear:left|right|both\" style to it."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\000eb[0-9]+x\000clear\\([<>]?\\)\\. *\000eb[0-9]+x\000\n\n" nil t)
    (let ((clear-horizontal
           (cond
            ((string= (match-string 1) "<")
             "left")
            ((string= (match-string 1) ">")
             "right")
            (t
             "both"))))
      (replace-match "\n\n")
      (when (re-search-forward "\000eb\\([0-9]+\\)x\000" nil t)
        (let* ((my-index (string-to-number (match-string 1)))
               (my-token (gethash my-index Textile-tokens)))
          (with-temp-buffer
            (insert my-token)
            (goto-char (point-min))
            (if (re-search-forward " et_style=\"\\(.*?\\)\"" nil t)
                (replace-match (concat " et_style=\"{clear:"
                                       clear-horizontal
                                       ";}"
                                       (match-string 1)
                                       "\"") t t))
            (puthash my-index (buffer-string) Textile-tokens)))))))

(defun textile-string (my-string)
  "The workhorse loop.  Take MY-STRING, dump it in a temp buffer, and
  start operating on it."
  (Textile-clear-tokens)
  ; FIXME: this is wrong, all these should be either global or passed
  ; into the functions, shouldn't they?
  (let ((Textile-macros-list Textile-macros-list-defaults)
        (my-table (Textile-new-table-alignment))
        (my-table-col 0))
    (with-temp-buffer
      (insert my-string)
      ; begin tokenizing
      ; First we need to remove the \000 strings
      (Textile-tokenize-ascii-zero)
      ; first provide for BC's unit test; may remove this later
      (Textile-BC-unit-test)
      ; inline elisp
      (Textile-inline-elisp-process)
      ; double-equals escapes
      (Textile-escape-double-equals-blocks)
      ; double-equals escapes inline
      (Textile-escape-double-equals-inline)
      ; blockcode processor, non-sticky
      (Textile-blockcode)
      ; footnote processor
      (Textile-footnotes)
      ; read in aliases
      (Textile-aliases)
      ; Fixup tables with "table." codes
      (Textile-table-fixup)
      ; find tables, call out to table processor
      (Textile-table-replace)
      ; image support
      (Textile-images)
      ; links
      (Textile-links)
      ; definition lists
      (Textile-definition-lists)
      ; go through Textile tags
      (Textile-blocks)
      ; lists
      (Textile-lists)
      ; blockcode processor, sticky
      (Textile-blockcode-sticky)
      ; go through Textile sticky blockquote tags
      (Textile-blockquote-sticky)
      ; inline footnotes
      (Textile-inline-footnotes)
      ; autolink URLs
      (Textile-autolink)
      ; macros and quotes
      (Textile-quotes)
      (Textile-macros)
      ; inline @ code tag before braces
      (Textile-inline-code)
      ; inline braces
      (Textile-braces)
      ; inline tags
      (Textile-inlines)
      ; acronyms
      (Textile-acronyms)
      ; OK, any block that stands alone is a paragraph by now.
      (Textile-unmarked-paragraphs)
      ; process clear blocks
      (Textile-clears)
      ; Convert non-ASCII values to Unicode
      (Textile-process-non-ascii)
      ; Single newlines become <br /> tags
      (Textile-linebreaks)
      ; All Textile tag interpretation is complete.  Now, revert tokens:
      (Textile-revert-tokens)
      ; interpret attributes
      (Textile-compile-attributes)
      (buffer-string))))

; functions to support batch mode (for CGI operation, perhaps)

(defun Textile-batch-read ()
  "Read file from STDIN."
  (read-from-minibuffer ""))

(defun Textile-batch-write (arg)
  "Write output to STDOUT."
  (send-string-to-terminal arg))

(when noninteractive
  (let ((input-string "")
        (current-line nil))
    (while (condition-case nil
	       (setq current-line (Textile-batch-read))
	     (error nil))
      (while (string-match "\r" current-line)
	(setq current-line (replace-match "" nil nil current-line)))
      (setq input-string (concat input-string current-line "\n")))
    (Textile-batch-write (textile-string
			  (substring input-string 0
				     (- (length input-string) 1))))))

(provide 'textile2)