; textile.el
; $Id$

; by Charles Sebold <csebold@livingtorah.org>

;;; Copyright: (C) 2004 Charles Sebold
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
;;    <URL:http://www.livingtorah.org/~csebold/emacs/textile/>

; Note - in comments, DA = Dean Allen (original author of PHP Textile),
; BC = Brad Choate (implemented Textile v2 in Perl for Movable Type)

; Todo for release 1.0: support all of DA's Textile 1.0 as demonstrated
; at http://www.textism.com/tools/textile/, as well as some of BC's
; Textile 2 at http://bradchoate.com/mt/docs/mtmanual_textile2.html.
; Also include all basic support functions necessary to integrate into
; normal life, like textile-string, textile-buffer, textile-region, and
; support generating a separate HTML buffer and leaving the old one
; intact

; Todo for release 1.5: Texinfo manual.

; Todo for release 2.0: support all of BC's Textile 2.0 as demonstrated
; at http://bradchoate.com/tools/textile/.  Possibly also built-in
; support for longlines.el and things like that, so that you can work
; the way God and RMS intended Emacs to work, but then process Textile
; the way DA and BC intended for Textile to work.

; Todo for release 2.1: integrate into W3 and other HTML-processing
; solutions like w3m.el, so that you can view buffers, convert them to
; plain text, and so forth.  No plans for textile-to-plain-text
; conversion, that's too much to take on; too many ways to convert HTML
; to plain text to make that worthwhile.

; Todo for release 2.2: Integrate into Message from Gnus so a person
; could do that most evil of tasks, compose and send HTML messages
; straight from a *mail* buffer.

; Todo for release 2.3: (May be sooner if I feel like it sooner) Include
; a minor mode for composing textile which will do some
; syntax-highlighting, but don't expect a WYSIWYG textile editor or
; anything like that.  Maybe we could work this into the compile
; support... OK, that's too much.

(defvar textile-block-tag-regexp-start "^\\("
  "All textile tag regexps start with this.")
(defvar textile-block-any-tag-regexp "[a-z0-9]+"
  "Regexp corresponding to the basic xx. tag.
This doesn't count lists and escaped parts.")
; This next one might be able to be replaced by the new textile-attributes
; code, which just reads from the end of the tag to the end of the attrib
; information
(defvar textile-block-tag-regexp-end "\\)\\(.*?\\)\\(\\.\\{1,2\\}\\) "
  "This is how all block tags are supposed to end.")

(defvar textile-block-tag-regexp
  (concat textile-block-tag-regexp-start
          textile-block-any-tag-regexp
          textile-block-tag-regexp-end)
  "Corresponds to standard Textile block tags.
This puts the previous pieces together, making it easier for
us to construct alternate block tag regexps.")

(defvar textile-any-block-tag-regexp
  "^\\(?:[a-z0-9]+\\.\\|[^ ]+?[*#]\\|==\\||\\)"
  "Corresponds to any Textile block tag.
All block tags must match this in some way; useful for
determining where an extended block ends.")

(defun textile-this-block-tag-regexp (tag)
  "Create a block tag regexp out of TAG."
  (concat textile-block-tag-regexp-start
          tag
          textile-block-tag-regexp-end))

(defvar textile-list-tag-regexp
  ; FIXME - support for nested lists, v2 probably
  ; FIXME - adapt code for table rows to this regexp for lists
  "^\\(([^ ]+?)\\|\\)\\([*#]+\\)[^ ]* "
  "All list block tags must match this.")

(defvar textile-inline-tag-regexp
  (concat
   "\\(?:^\\| \\|[>]\\)\\("
   "[*]\\{1,2\\}\\|[_]\\{1,2\\}\\|[+]\\{1,2\\}\\|[-]\\{1,2\\}\\|[~^%@]"
   "\\|\\?\\?\\|=="
   "\\)")
  "Should match any inline tag.")

(defvar textile-inline-tag-list
  '("*" "strong" "_" "em" "**" "b" "__" "i" "++" "big" "--" "small"
    "-" "del" "+" "ins" "^" "sup" "~" "sub" "%" "span" "@" "code" "??" "cite")
  "Link textile to HTML tags for inline formatting.")

(defvar textile-alias-list-defaults nil
  "Standard link aliases.
For each string to match should be either a string which is the URL, or
a list whose car is the title and cadr is the URL.")

(defvar textile-error-break nil
  "Break parsing Textile when hitting a parsing error?
Do you want a total failure when you hit a textile parsing
problem?  If so then make this t.  Otherwise it defaults to nil,
and warns you in the Messages buffer.")

(defvar textile-br-all-newlines t
  "Should single newlines produce HTML linebreaks?
This variable determines whether we are using standard <br /> for
every non-block line break, or we aren't (which is nicer with the
usual way that people use Emacs, text-based modes, and
auto-fill-mode).  Right now the default is standard Textile
behavior (this could probably work with longlines.el or something
like that).")

(defun textile-string-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile list tree."
  (let ((blocks (textile-blockcode-blocks
                 (textile-escape-blocks (split-string my-string "\n\n")))))
    (delete "" (mapcar 'textile-block-to-list blocks))))

(defun textile-escape-blocks (my-list)
  "In a list of block strings, bring escaped blocks together."
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list)))
        (setq my-list (cdr my-list))
        (if (string-match "^== *\n" this-item)
            (let* ((escape-block (replace-match "" nil nil this-item))
                   (hit-end (string-match "\n== *$" escape-block)))
              (if hit-end
                  (setq escape-block (replace-match "" nil nil escape-block)))
              (while (and my-list (not hit-end))
                (if (string-match "\n== *$" (car my-list))
                    (progn
                      (setq escape-block
                            (concat escape-block "\n\n"
                                    (replace-match "" nil nil
                                                   (car my-list))))
                      (setq hit-end t))
                  (setq escape-block (concat escape-block "\n\n"
                                             (car my-list))))
                (setq my-list (cdr my-list)))
              (push (list escape-block (list 'textile-tag nil)) new-list))
          (push this-item new-list))))
    (reverse new-list)))

(defun textile-blockcode-blocks (my-list)
  "In a list of block strings, bring blockcode blocks together."
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list)))
        (setq my-list (cdr my-list))
        (if (and (stringp this-item)
                 (string-match (textile-this-block-tag-regexp "bc") this-item))
            (let* ((my-tag (match-string 1 this-item))
                   (my-attr (match-string 2 this-item))
                   (extended-p (match-string 3 this-item))
                   (code-block (replace-match "" nil nil this-item))
                   (hit-end nil))
              (if extended-p
                  (while (and my-list (not hit-end))
                    (if (or (not (stringp (car my-list)))
                            (string-match textile-any-block-tag-regexp
                                      (car my-list)))
                        (progn
                          (setq hit-end t)
                          (push (list (list code-block
                                            (list 'textile-tag "code"))
                                      (plist-put my-attr 'textile-tag
                                                 "pre"))
                                new-list)
                          (push (car my-list) new-list))
                      (setq code-block (concat code-block "\n\n"
                                               (car my-list))))
                    (setq my-list (cdr my-list)))
                (push (list (list code-block (list 'textile-tag "code"))
                            (plist-put my-attr 'textile-tag "pre"))
                                  new-list)))
          (push this-item new-list))))
    (reverse new-list)))

(defun textile-block-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile block tree."
  (if (listp my-string)
      my-string
    (let ((my-plist (plist-put nil 'textile-explicit t)))
      (cond
                                        ; test for block tags
       ((string-match "^clear[<>]?\\. *$" my-string)
        (setq my-string (textile-block-clear my-string)))
       ((string-match "^.*?|.*| *$" my-string)
        (textile-block-table my-string nil))
       ((string-match textile-block-tag-regexp my-string)
        (let* ((tag (match-string 1 my-string))
               (attributes (textile-attributes " " (match-string 2 my-string)))
               (extended (string= (match-string 3 my-string) ".."))
               (my-function
                (car (read-from-string (concat "textile-block-" tag)))))
          (setq attributes (plist-put attributes 'textile-extended extended))
          (setq attributes (plist-put attributes 'textile-explicit t))
          (cond
           ((fboundp my-function)
            (funcall my-function my-string attributes))
           ((string-match "^h[1-6]$" tag)
            (setq attributes (plist-put attributes 'textile-hlevel
                                        (substring tag 1 2)))
            (textile-block-header my-string attributes))
           ((string-match "^fn[0-9]+$" tag)
            (setq attributes (plist-put attributes 'textile-fn-num
                                        (substring tag 2)))
            (setq attributes (plist-put attributes 'class "footnote"))
            (setq attributes (plist-put attributes 'id tag))
            (textile-block-footnote my-string attributes))
           (t
            (setq my-plist (plist-put my-plist 'textile-tag "p"))
            (setq my-plist (plist-put my-plist 'textile-explicit nil))
            (list (textile-inline-to-list my-string) my-plist)))))
       ((string-match textile-list-tag-regexp my-string)
        (textile-block-list my-string))
       (t
        (setq my-plist (plist-put my-plist 'textile-tag "p"))
        (setq my-plist (plist-put my-plist 'textile-explicit nil))
        (list (textile-inline-to-list my-string) my-plist))))))

(defun textile-inline-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile inline tree."
  (let ((my-plist nil))
    (cond
     ; break it up for inline tags
     )
    ; from this point on there will be no more converting ampersands
    ; to &amp;
    (while (string-match "\\([^\000-\177]+\\)" my-string)
      (let* ((non-ascii-string (match-string 1 my-string))
             (replacement (save-match-data
                            (textile-non-ascii-to-unicode non-ascii-string))))
        (setq my-string (replace-match replacement nil nil my-string))))
    (if my-plist
        (list my-string my-plist)
      my-string)))

(defun textile-list-to-blocks (my-list)
  "Convert list of textile trees to XHTML string."
  (textile-final-XHTML-tidy
   (mapconcat 'textile-compile-string
              (textile-unextend-blocks my-list) "\n\n")))

(defun textile-append-block (my-list block)
  "Insert BLOCK into MY-LIST as the second-to-last item, and return MY-LIST."
  (setcdr (nthcdr (- (safe-length my-list) 2) my-list)
          (cons block (nthcdr (- (safe-length my-list) 1) my-list)))
  my-list)

(defun textile-unextend-blocks (my-list)
  "In a list of textile trees, pull extended blocks together."
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list))
             (my-attr (car (last this-item))))
        (setq my-list (cdr my-list))
        (if (plist-get my-attr 'textile-extended)
            (while (and my-list
                        (not (plist-get (car (last (car my-list)))
                                        'textile-explicit)))
              (textile-append-block this-item (car my-list))
              (setq my-list (cdr my-list))))
        (push this-item new-list)))
    (reverse new-list)))

(defun textile-compile-string (my-list)
  "Convert textile tree to XHTML string."
  (if (stringp my-list)
      my-list
    (let* ((my-plist (car (last my-list)))
           (args (textile-all-but-last  my-list))
           (my-string (mapconcat (function
                                  (lambda (item)
                                    (if (stringp item)
                                        item
                                      (textile-compile-string item))))
                                 args "")))
      (if (and (listp my-plist) (plist-get my-plist 'textile-tag))
          (textile-enclose-tag my-string my-plist)
        my-string))))

(defun textile-final-XHTML-tidy (my-string)
  "Final pretty-printing details after all processing is done."
  (while (string-match "<br />\\(.\\)" my-string)
    (setq my-string (replace-match "<br />\n\\1" nil nil my-string)))
  (while (string-match "</tr>\\(.\\)" my-string)
    (setq my-string (replace-match "</tr>\n\\1" nil nil my-string)))
  (while (string-match "</p><p" my-string)
    (setq my-string (replace-match "</p>\n\n<p" nil nil my-string)))
  (while (string-match "<\\(/li\\|/?ol\\|/?ul\\)>\\(.\\)" my-string)
    (setq my-string (replace-match "<\\1>\n\\2" nil nil my-string)))
  (while (string-match "\\(.\\)\\(<li[ >]\\)" my-string)
    (setq my-string (replace-match "\\1\n\\2" nil nil my-string)))
  my-string)

(defun textile-process-aliases ()
  "Process the entire buffer, finding and removing aliases."
  (while
      (cond
       ((looking-at "\\[.*?\\].+")
        (textile-block-alias))
       (t (textile-next-paragraph)))))

(defun textile-attributes (&optional stop-regexp attrib-string)
  "Return a plist of attributes from (point) or ATTRIB-STRING.
If ATTRIB-STRING is non-nil, then make a new buffer with that;
otherwise make a new buffer with the entirety of the buffer
from (point) to the end.  Process it until reaching a space that
isn't within some kind of attribute block.  While processing,
handle different kinds of attributes, including styles, classes,
ids, and langs.  Stop processing at the end of the buffer, string,
or STOP-REGEXP."
   (let ((my-plist nil)
        (style (if (boundp 'Textile-next-block-clear)
                   Textile-next-block-clear
                 ""))
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
        (textile-well-formed t)
        (stop-regexp (or stop-regexp
                         " "))
        (attrib-string (or attrib-string
                           (buffer-substring (point) (point-max)))))
     (makunbound 'Textile-next-block-clear)
     (with-temp-buffer
       (insert 
        (or attrib-string ""))
       (goto-char (point-min))
       (while not-finished
         (let ((this-char (char-after)))
           (cond
            ((eobp)
             (setq not-finished nil))
            ((looking-at stop-regexp)
             (re-search-forward stop-regexp nil t)
             (setq not-finished nil))
            ((looking-at "{\\([^}]*\\)}")
             (setq style (concat style (match-string 1) "; "))
             (re-search-forward "}" nil t))
            ((looking-at "\\[\\(.*?\\)\\]")
             (setq lang (match-string 1))
             (re-search-forward "\\]" nil t))
            ((looking-at "(\\([^) (]+\\))")
             (let ((this-attrib (split-string (match-string 1) "#")))
               (setq class (car this-attrib))
               (setq id (cadr this-attrib)))
             (re-search-forward ")" nil t))
            ((looking-at "(.* .*)")
             ; parenthetical statement including a space
             (setq not-finished nil))
            ((equal this-char ?\()
             (setq left-pad (1+ left-pad))
             (forward-char 1))
            ((equal this-char ?\))
             (setq right-pad (1+ right-pad))
             (forward-char 1))
            ((equal this-char ?\>)
             (if (boundp 'Textile-in-table)
                 (setq align "right")
               (setq style (concat style "text-align: right; ")))
             (forward-char 1))
            ((looking-at "<>")
             (setq style (concat style "text-align: justify; "))
             (forward-char 2))
            ((equal this-char ?\<)
             (if (boundp 'Textile-in-table)
                 (setq align "left")
               (setq style (concat style "text-align: left; ")))
             (forward-char 1))
            ((equal this-char ?\=)
             (cond
              ((boundp 'Textile-in-table)
               (setq align "center"))
              ((boundp 'Textile-in-table-outer-tag)
               (setq style (concat style
                                   "margin-left: auto; margin-right: auto; ")))
              (t (setq style (concat style "text-align: center; "))))
             (forward-char 1))
            ((equal this-char ?^ )
             (setq valign "top")
             (forward-char 1))
            ((equal this-char ?\~)
             (setq valign "bottom")
             (forward-char 1))
            ((looking-at "[\\]\\([0-9]+\\)")
             (setq colspan (match-string 1))
             (re-search-forward "[\\]\\([0-9]+\\)" nil t))
            ((looking-at "/\\([0-9]+\\)")
             (setq rowspan (match-string 1))
             (re-search-forward "/\\([0-9]+\\)" nil t))
            ((and
              (equal this-char ?\_)
              (or
               (boundp 'Textile-in-table)
               (boundp 'Textile-in-table-outer-tag)))
             (setq textile-header t)
             (forward-char 1))
            (t
            ; if you hit something you don't recognize, then this
            ; isn't an attribute string
             (setq textile-well-formed nil)
             (setq not-finished nil)))))
       (setq my-plist (plist-put my-plist 'textile-attrib-string
                                 (buffer-substring (point-min)
                                                   (point)))))
     (if (> left-pad 0)
         (setq style (concat style "padding-left: "
                             (format "%d" left-pad) "em; ")))
     (if (> right-pad 0)
         (setq style (concat style "padding-right: "
                             (format "%d" right-pad) "em; ")))
     (when (and (or (> left-pad 0) (> right-pad 0))
                style
                (string-match "text-align: \\(left\\|right\\); " style))
       (setq style (replace-match "float: \\1; " nil nil style)))
     (dolist (this-variable '(style class id lang align valign
                                    colspan rowspan
                                    textile-header textile-well-formed))
       (when (string= (eval this-variable) "")
         (set this-variable nil))
       (setq my-plist (plist-put my-plist this-variable (eval this-variable))))
     my-plist))

(defun textile-process-block ()
  "Generic block processing to call inline textile conversion.
Process the following paragraph by calling various inline
functions.  This is the generic version; some blocks have special
process functions (see textile-process-list-block, etc.)."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (textile-end-of-paragraph)
                          (point)))
;      (textile-inline-entities)
      (textile-inline-newline)
      (textile-inline-generic)
      ; insert more inline tests here
      )))

(defun textile-process-list-block ()
  "Block processing of lists, calling inline textile conversion.
Process the following paragraph by calling various inline
functions, but handle new list items in this block specially."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (textile-end-of-paragraph)
                          (point)))
;      (textile-inline-entities)
      (textile-inline-li)
      (textile-inline-generic)
      ; insert more inline tests here
      )))

(defun textile-process-table-block ()
  "Block processing of tables, calling inline textile conversion.
Process the following paragraph by calling various inline
functions, but handle new table rows in this block specially."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (textile-end-of-paragraph)
                          (point)))
      (textile-inline-table)
      ; insert more inline tests here
      )))

(defun textile-process-definition-block ()
  "Block processing of definition blocks, calling inline textile conversion.
Process the following paragraph by calling various inline
functions, but handle new terms and definitions in this block
specially."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (textile-end-of-paragraph)
                          (point)))
;      (textile-inline-entities)
      (textile-inline-dl)
      (textile-inline-generic)
      ; insert more inline tests here
      )))

(defun textile-process-pre-block ()
  "Block processing of preformatted blocks, calling inline textile conversion.
Process the following paragraph by calling various inline
functions, but handle everything carefully, since this part is
supposed to be preformatted."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (textile-end-of-paragraph)
                          (point)))
      (textile-inline-entities)
      ; insert more inline tests here
      )))

(defun textile-non-ascii-to-unicode (string)
  "Convert STRING to Unicode entities."
  (let ((unicode-string (encode-coding-string string 'utf-16))
        (unicode-values nil)
        (output ""))
    (setq unicode-values (mapcar 'string-to-char
                                    (nthcdr 3
                                            (split-string
                                             unicode-string ""))))
    (while (cdr unicode-values)
      (setq output (concat output "&#" (number-to-string
                                        (+ (* (car unicode-values) 256)
                                           (cadr unicode-values)))
                           ";"))
      (setq unicode-values (cddr unicode-values)))
    output))

(defun textile-inline-generic ()
  "Handle most Textile inline processing of tags.
Practically all block formatting (except preformatted blocks)
should call this function to handle inline tags like em, strong,
footnotes, etc."
  (save-excursion
    (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
      (replace-match
       "<sup class=\"footnote\"><a href=\"#fn\\1\">\\1</a></sup>" t)))
  (save-excursion
    (while (re-search-forward "\\<\\([A-Z]\\{3,\\}\\)\\((\\(.*?\\))\\|\\)"
                              nil t)
      (if (match-string 3)
          (replace-match "<acronym title=\"\\3\">\\1</acronym>" t)
        (replace-match "<acronym>\\1</acronym>" t))))
  (save-excursion
    (while (re-search-forward
            "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\([,.;:]?\\(?: \\|$\\)\\)"
            nil t)
      (let* ((text (match-string 1))
             (url (match-string 2))
             (delimiter (match-string 3))
             (title "")
             (alias-info (textile-alias-to-url url textile-alias-list)))
        (replace-match "")
        (if alias-info
            (progn
              (setq title (car alias-info))
              (setq url (cadr alias-info)))
          (if (string-match "\\(.*\\) +(\\(.*\\))" text)
              (progn
                (setq title (match-string 2 text))
                (setq text (match-string 1 text)))))
        (if (string= title "")
            (setq title nil))
        (textile-tag-insert "a" (list 'title title 'href url))
        (insert text)
        (textile-end-tag-insert "a")
        (insert delimiter))))
  (save-excursion
    (while (re-search-forward textile-inline-tag-regexp nil t)
      (textile-handle-inline-tag (match-string 1)))))

(defun textile-alias-to-url (lookup alias-list)
  "Lookup potential alias LOOKUP in ALIAS-LIST, return nil if none."
  (if alias-list
      (if (and (stringp (car alias-list))
               (string= lookup (car alias-list)))
          (if (listp (cadr alias-list))
              (cadr alias-list)
            (list "" (cadr alias-list)))
        (textile-alias-to-url lookup (cddr alias-list)))
    nil))

(defun textile-handle-inline-tag (tag)
  "Given TAG, properly handle everything to the end of this tag."
  (if (save-excursion
        (re-search-forward (concat (regexp-quote tag)
                                   "\\(?:$\\| \\|[<,;:!?.]\\)") nil t))
      (progn
        (delete-region (- (point) (length tag)) (point))
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point)
             (save-excursion
               (re-search-forward (concat (regexp-quote tag)
                                          "\\(?:$\\| \\|[<,;:!?.]\\)") nil t)
               (point)))
            (if (not (string= tag "=="))
                (let ((attributes (textile-attributes))
                      (html-tag (textile-generate-inline-tag
                                 tag
                                 textile-inline-tag-list)))
                  (textile-delete-attributes attributes)
                  (textile-tag-insert html-tag attributes)
                  (re-search-forward (concat
                                      (regexp-quote tag)
                                      "\\($\\| \\|[<,;:!?.]\\)") nil t)
                  (replace-match (concat "</" html-tag ">\\1")))
              (re-search-forward (concat (regexp-quote tag)
                                         "\\($\\| \\|[<,;:!?.]\\)") nil t)
              (replace-match "\\1")))))))

(defun textile-generate-inline-tag (tag tag-list)
  "Convert textile tag to HTML tag or return nil if no match."
  (if tag-list
      (if (string= tag (car tag-list))
          (cadr tag-list)
        (textile-generate-inline-tag tag (cddr tag-list)))
    nil))

(defun textile-inline-entities ()
  "Handle HTML entity conversion inline.
Call this function to handle converting all characters that
should be escaped to their (X)HTML entity versions."
  (save-excursion
    (while (re-search-forward "&" nil t)
      (replace-match "&amp;")))
  (save-excursion
    (while (re-search-forward "<" nil t)
      (replace-match "&lt;")))
  (save-excursion
    (while (re-search-forward ">" nil t)
      (replace-match "&gt;"))))

(defun textile-inline-newline ()
  "Handle single newlines according to textile-br-all-newlines.
If textile-br-all-newlines is t (the default), then do what the
original Textile does and convert newlines that don't break
paragraphs into <br> tags."
  (if textile-br-all-newlines
      (save-excursion
        (while (re-search-forward "\n" nil t)
          (replace-match "<br />\\&")))))

(defun textile-inline-table ()
  "Handle inline processing of tags and table cells in this block.
Convert table rows into proper HTML, including attributes for
individual cells and rows as necessary."
  (if textile-br-all-newlines
      (save-excursion
        (while (re-search-forward "\\([^|] *\\)\n" nil t)
          (replace-match "\\1<br />\n"))))
  (setq Textile-in-table t)
  (save-excursion
    (while (not (eobp))
      (let ((row-attributes (textile-attributes " *|")))
        (textile-delete-attributes row-attributes)
        (textile-tag-insert "tr" row-attributes)
        (while (not (or (looking-at "\n") (eobp)))
          (let* ((cell-attributes (textile-attributes "[.] +"))
                 (header (or (plist-get row-attributes 'textile-header)
                             (plist-get cell-attributes 'textile-header))))
            (textile-delete-attributes cell-attributes)
            (textile-tag-insert
             (if header
                 "th"
               "td")
             cell-attributes)
            ; FIXME - have to run textile-inline-generic here on the cell
            (if (re-search-forward " *|" nil t)
                (replace-match ""))
            (textile-end-tag-insert (if header
                                        "th"
                                      "td"))))
        (textile-end-tag-insert "tr")
        (re-search-forward "\n" nil 1))))
  (makunbound 'Textile-in-table))

(defun textile-inline-li ()
  "Handle inline processing of tags and list items in this block.
Convert list items starting with list tags into proper HTML,
including attributes if necessary."
  (if textile-br-all-newlines
      (save-excursion
        (while (re-search-forward "\n[^*#]" nil t)
          (replace-match "<br />\\&"))))
  ; FIXME - deal with nested lists, probably v2
  (save-excursion
    (while (re-search-forward "\n\\([*#]\\)" nil t)
      (let ((tag (match-string 1)))
        (replace-match "</li>\n")
        (let ((attributes (textile-attributes)))
          (textile-delete-attributes attributes)
          (textile-tag-insert "li" attributes))))))

(defun textile-inline-dl ()
  "Convert definition-list terms and definitions into proper HTML."
  (save-excursion
    (while (progn
             (if (not (looking-at ".*?[^ \n]+:[^ ]"))
                 (if textile-br-all-newlines
                     (save-excursion
                       (backward-char 1)
                       (insert "<br />")))
               (insert "<dt>")
               (re-search-forward ":" nil t)
               (save-excursion
                 (backward-char 1)
                 (insert "</dt>"))
               (delete-backward-char 1)
               (insert "<dd>"))
             (re-search-forward "\n" nil t))))
  (save-excursion
    (while (re-search-forward "\n<dt>" nil t)
      (replace-match "</dd>\\&"))))

(defun textile-block-clear (my-string)
  "Pass a \"clear:left|right|both\" style to the next block.
The only valid attributes to include in here are \"<\" or \">\" for clearing
left or right floating, or nothing for the default of \"both\"."
  (if (string-match "clear\\([<>]?\\)\\. *" my-string)
      (let ((attrib-string (match-string 1 my-string)))
        (re-search-forward "clear\\([<>]?\\)\\. *" nil t)
        (setq my-string (replace-match "" t nil my-string))
        (setq Textile-next-block-clear
              (cond
               ((string= attrib-string "<")
                "clear: left; ")
               ((string= attrib-string ">")
                "clear: right; ")
               (t "clear: both; "))))
    (textile-error "Clear block's attribute string must be <, >, or nothing."))
  "")

(defun textile-block-alias ()
  "Process an alias for future linking."
  (if (looking-at "\\[\\(.*?\\)\\]\\([^\n]+\\)")
      (let* ((alias-string (match-string 1))
             (url-string (match-string 2))
             (alias "")
             (title ""))
        (if (string-match "\\(.*\\) +(\\(.*\\))" alias-string)
            (progn
              (setq alias (match-string 1 alias-string))
              (setq title (match-string 2 alias-string)))
          (setq alias alias-string))
        (if (member alias textile-alias-list)
            (dotimes (i (safe-length textile-alias-list))
              (if (and
                   (stringp (nth i textile-alias-list))
                   (string= (nth i textile-alias-list) alias))
                  (setcar (nthcdr (1+ i) textile-alias-list)
                          (if (string= title "")
                              url-string
                            (list title url-string)))))
          (setq textile-alias-list
                (cons
                 (if (string= title "")
                     url-string
                   (list title url-string))
                 textile-alias-list))
          (setq textile-alias-list
                (cons alias textile-alias-list)))
        (re-search-forward "\\[.*?\\].*?\n+" nil t)
        (replace-match "")
        t)
    (textile-next-paragraph)))

(defun textile-block-dl (my-string attributes)
  "Handle the definition list block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this definition list."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <dl> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match (textile-this-block-tag-regexp "dl") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "dl"))
  (let ((my-list (split-string my-string "\n"))
        (my-new-list nil))
    (dotimes (i (safe-length my-list))
      (let ((this-line (nth i my-list)))
        (if (string-match ":" this-line)
            (setq my-new-list (nconc my-new-list (list this-line)))
          (setcar (nthcdr (- (safe-length my-new-list) 1) my-new-list)
                  (concat (nth (- (safe-length my-new-list) 1) my-new-list)
                          "<br />"
                          this-line)))))
    (setq my-list nil)
    (dolist (this-string my-new-list)
      (setq my-list
            (if (string-match "^\\(.*?\\):\\(.*\\)$" this-string)
                (append
                 (list (list (textile-inline-to-list
                              (match-string 2 this-string))
                             (list 'textile-tag "dd"))
                       (list (textile-inline-to-list
                              (match-string 1 this-string))
                             (list 'textile-tag "dt")) "\n") my-list)
              (append (textile-inline-to-list this-string) my-list))))
    (append (reverse my-list) (list attributes))))

(defun textile-block-list (my-string)
  "Handled the list block MY-STRING with attributes L-ATTRIBUTES."
  (let ((temp-string (substring my-string
                                (length
                                 (plist-get
                                  (textile-attributes "[#*]" my-string)
                                  'textile-attrib-string)))))
    (if (string-match "^\\([#*]+\\)" temp-string)
        (setq temp-string (replace-match "" nil nil temp-string)))
    (if (plist-get (textile-attributes " " temp-string)
                   'textile-well-formed)
        (if (string-match "^\\([#*]+\\)" my-string)
            (let ((my-start-level (length (match-string 1 my-string)))
                  (my-list (split-string my-string "\n")))
              (setq my-list (textile-organize-lists my-start-level my-list))
              (textile-process-li my-list)))
      (list (textile-inline-to-list my-string)
            (plist-put
             (plist-put nil 'textile-tag "p")
             'textile-explicit nil)))))

(defun textile-process-li (my-list)
  (let ((first-string-pos 0))
    (while (and (< first-string-pos (safe-length my-list))
                (not (stringp (nth first-string-pos my-list))))
      (setq first-string-pos (1+ first-string-pos)))
    (if (not (< first-string-pos (safe-length my-list)))
        nil
      (if (string-match textile-list-tag-regexp (nth first-string-pos
                                                     my-list))
          (let* ((sample-string (nth first-string-pos my-list))
                 (tag (match-string 2 sample-string))
                 (l-attributes
                  (plist-put
                   (textile-attributes " "
                                       (match-string 1 sample-string))
                   'textile-tag
                   (cond
                    ((string-match "^#" tag)
                     "ol")
                    ((string-match "[*]" tag)
                     "ul")
                    (t "?")))))
            (append (mapcar
                     (function
                      (lambda (x)
                        (if (listp x)
                            (textile-process-li x)
                          (if (string-match "^\\(([^ ]+?)\\|\\)[#*]+" x)
                              (setq x (replace-match "" nil nil x)))
                          (let ((attributes (textile-attributes " " x)))
                            (setq x (substring x (length
                                                  (plist-get
                                                   attributes
                                                   'textile-attrib-string))))
                            (list (textile-inline-to-list x)
                                  (plist-put attributes 'textile-tag
                                             "li"))))))
                     my-list) (list l-attributes)))))))
        
(defun textile-organize-lists (last-level my-list)
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list))
             (this-level (textile-list-level
                          (if (string-match textile-list-tag-regexp this-item)
                              (match-string 2 this-item))))
             (push-this
              (cond
               ((> this-level last-level)
                (textile-organize-lists this-level my-list))
               ((< this-level last-level)
                nil)
               (t
                this-item)))
             (push-length (if (and push-this (listp push-this))
                              (textile-recursive-length push-this)
                            1)))
        (if push-this
            (push push-this new-list))
        (if (< this-level last-level)
            (setq my-list nil)
          (setq my-list (nthcdr push-length my-list)))))
    (reverse new-list)))

(defun textile-list-level (my-tag)
;  (let ((x (length my-tag)))
;    (if (string-match "^#" my-tag)
;        (+ 1000 x)
;      x)))
  (length my-tag))

(defun textile-recursive-length (my-list)
  (let ((i 0))
    (setq my-list (mapcar (function (lambda (x)
                                      (if (listp x)
                                          (textile-recursive-length x)
                                        1))) my-list))
    (dolist (x my-list)
      (setq i (+ i x)))
    i))

(defun textile-all-but-last (my-list)
  "Return everything in the list but the last item."
  (reverse (cdr (reverse my-list))))

(defun textile-block-table (my-string attributes)
  "Handle the table block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this table."
  (if (string-match (textile-this-block-tag-regexp "table") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <table> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (setq attributes (plist-put attributes 'textile-tag "table"))
  (let ((my-row-list (textile-all-but-last
                      (split-string my-string " *| *\\(?:\n\\|$\\)"))))
    (append
     (mapcar 'textile-table-row-process my-row-list) (list attributes))))

(defun textile-table-row-process (this-string)
  (let* ((my-cell-list (split-string this-string " *| *"))
         (row-attributes (textile-attributes " " (car my-cell-list)))
         (my-cell-list (cdr my-cell-list)))
    (setq Textile-in-table t) ; where do I unset it though?
    (setq row-attributes (plist-put row-attributes 'textile-tag "tr"))
    (append (mapcar 'textile-table-cell-process my-cell-list)
          (list row-attributes))))

(defun textile-table-cell-process (this-cell)
  (let* ((cell-attributes (textile-attributes "[.] +"
                                              this-cell))
         (header
;          (or (plist-get row-attributes
;                         'textile-header)
              (plist-get cell-attributes
                         'textile-header)))
;           )
    (if (string-match
         (concat "^" (regexp-quote
                      (plist-get cell-attributes
                                 'textile-attrib-string)))
         this-cell)
        (setq this-cell (replace-match "" nil nil
                                       this-cell)))
    (setq cell-attributes (plist-put cell-attributes
                                     'textile-tag
                                     (if header
                                         "th"
                                       "td")))
    (list (textile-inline-to-list this-cell)
          cell-attributes)))

(defun textile-block-p (my-string attributes)
  "Handle the paragraph block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this paragraph."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <p> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match (textile-this-block-tag-regexp "p") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "p"))
  (list (textile-inline-to-list my-string) attributes))

(defun textile-block-footnote (my-string attributes)
  "Handle the footnote starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this footnote."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended fn? block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match textile-block-tag-regexp my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "p"))
  (list (list (plist-get attributes 'textile-fn-num)
              (plist-put nil 'textile-tag "sup"))
        " " (textile-inline-to-list my-string) attributes))

(defun textile-block-header (my-string attributes)
  "Handle the header block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this header."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <h?> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (let ((my-tag (concat "h" (plist-get attributes 'textile-hlevel))))
    (if (string-match textile-block-tag-regexp my-string)
        (setq my-string (replace-match "" nil nil my-string)))
    (setq attributes (plist-put attributes 'textile-tag my-tag))
    (list (textile-inline-to-list my-string) attributes)))

(defun textile-block-bq (my-string attributes)
  (setq attributes (plist-put attributes 'textile-tag "blockquote"))
  (if (string-match textile-block-tag-regexp my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (list (list (textile-inline-to-list my-string)
              (plist-put nil 'textile-tag "p"))
        attributes))

(defun textile-error (error-message)
  "Break with an error if textile-error-break is t.
Otherwise just flash the error into the Messages buffer and
continue."
  (if textile-error-break
      (error "%s" error-message)
    (message "Textile error: %s" error-message)))

(defun textile-region (start end)
  "Call textile-code-to-blocks on region from point to mark."
  (interactive "r")
  (save-excursion
    (let ((my-string (buffer-substring start end)))
      (delete-region start end)
      (goto-char start)
      (insert (textile-string my-string)))))

(defun textile-buffer ()
  "Call textile-code-to-blocks on the entire buffer."
  (interactive)
  (textile-region (point-min) (point-max)))

(defun textile-string (my-string)
  "Process MY-STRING, return XHTML-encoded string."
  (textile-list-to-blocks (textile-string-to-list my-string)))

(defun textile-generate-attribute-string (my-plist)
  "Generate an attribute string based on MY-PLIST.
Any attributes that start with \"textile-\" will be ignored."
  (if (and (car my-plist) (cadr my-plist))
      (let ((attrib (format "%s" (car my-plist)))
            (value (format "%s" (cadr my-plist))))
        (if (string-match "^textile-" attrib)
            (textile-generate-attribute-string (cddr my-plist))
          (concat " " attrib "=\"" value "\""
                  (textile-generate-attribute-string (cddr my-plist)))))
    (if (cddr my-plist)
        (textile-generate-attribute-string (cddr my-plist))
      "")))

(defun textile-enclose-tag (my-string my-plist)
  "Enclose MY-STRING in a tag generated from information in MY-PLIST."
  (if (plist-get my-plist 'textile-tag)
      (concat "<" (plist-get my-plist 'textile-tag)
              (textile-generate-attribute-string my-plist) ">"
              my-string "</" (plist-get my-plist 'textile-tag) ">")
    my-string))

(provide 'textile)