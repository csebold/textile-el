; textile.el
; $Id$

; Filters will probably have to wait for v2 of this code;
; that looks hard.

; Lots of spurious progns have been introduced to help with debugging;
; those will need to be removed before 1.0.

; Note - in comments, DA = Dean Allen (original author of PHP Textile),
; BC = Brad Choate (implemented Textile v2 in Perl for Movable Type)

; Tue 27 Jul 2004 09:41 - yay!  All BC block tags handled except for
; table at this point (in progress).

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
          tag "+"
          textile-block-tag-regexp-end))

(defvar textile-list-tag-regexp
  ; FIXME - support for nested lists, v2 probably
  "^\\(([^ ]+?)\\|\\)\\([*#]\\)[^ ]* "
  "All list block tags must match this.")

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

(defun textile-code-to-blocks (start end)
  "Block process region from START to END.
This is the primary processing loop in textile.el."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while
          (cond
           ((looking-at "^clear[<>]?\\. *\n")
            (textile-block-clear))
           ((looking-at textile-block-tag-regexp)
            (let* ((tag (match-string 1))
                   (attributes (textile-attributes " " (match-string 2)))
                   (extended (string= (match-string 3) ".."))
                   (my-function
                    (car (read-from-string (concat "textile-block-" tag)))))
              (setq attributes (plist-put attributes 'textile-extended
                                          extended))
              (cond
               ((fboundp my-function)
                (funcall my-function attributes))
               ((string-match "^h[1-6]$" tag)
                (setq attributes (plist-put attributes 'textile-hlevel
                                            (substring tag 1 2)))
                (textile-block-header attributes))
               ((string-match "^fn[0-9]+$" tag)
                (setq attributes (plist-put attributes 'textile-fn-num
                                            (substring tag 2)))
                (setq attributes (plist-put attributes 'class "footnote"))
                (setq attributes (plist-put attributes 'id tag))
                (textile-block-footnote attributes))
               (t (textile-block-p nil)))))
           ((looking-at textile-list-tag-regexp)
            (let* ((tag (match-string 2))
                   (l-attributes (textile-attributes " " (match-string 1))))
              (cond
               ((string= tag "#")
                (textile-block-ol l-attributes))
               ((string= tag "*")
                (textile-block-ul l-attributes))
               (t
                (textile-error "What kind of list is this?")
                (textile-block-p nil)))))
           ((looking-at "^== *\n")
            (textile-block-escape))
           ((looking-at "|")
            (textile-block-table nil))
           (t (textile-block-p nil))))
      (widen))))

(defun textile-attributes (stop-regexp &optional attrib-string)
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
        (valign nil)
        (rowspan nil)
        (colspan nil)
        (textile-header nil)
        (attrib-string (or attrib-string
                           (buffer-substring (point) (point-max)))))
     (makunbound 'Textile-next-block-clear)
     (with-temp-buffer
       (insert 
        (or attrib-string ""))
       (goto-char (point-min))
       (setq my-plist
             (plist-put my-plist 'textile-attrib-string-length (1- (point-max))))
       (while (not (eobp))
         (let ((this-char (char-after)))
           (cond
           ((looking-at stop-regexp)
            (setq my-plist (plist-put my-plist
                                      'textile-attrib-string-length (point)))
            (goto-char (point-max)))
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
            (setq style
                  (concat style 
                          ; if this is a table handle this differently
                          (if (string= stop-regexp "|")
                              "margin-left: auto; margin-right: auto; "
                            "text-align: center; ")))
            (forward-char 1))
           ((equal this-char ?\^ )
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
           ((equal this-char ?\_)
            (setq textile-header t)
            (forward-char 1))
;;         ((equal this-char ?\|)
;;          (forward-char 1)) ; filters - FIXME
            (t (forward-char 1))))))
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
     (dolist (this-variable '(style class id lang valign colspan rowspan
                                    textile-header))
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
      (textile-inline-entities)
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
      (textile-inline-entities)
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
      (textile-inline-generic)
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
      (textile-inline-entities)
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

(defun textile-inline-generic ()
  "Handle most Textile inline processing of tags.
Practically all block formatting (except preformatted blocks)
should call this function to handle inline tags like em, strong,
footnotes, etc."
  (save-excursion
    (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
      (replace-match
       "<sup class=\"footnote\"><a href=\"#fn\\1\">\\1</a></sup>"))))

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
  (save-excursion
    ; FIXME - finish table row processing
    (while (not (eobp))
      (let ((row-attributes (textile-attributes "|")))
        (textile-delete-attributes row-attributes)
        (textile-tag-insert "tr" row-attributes)
        (while (not (looking-at "\n"))
          (let* ((cell-attributes (textile-attributes "[A-Za-z0-9.]"))
                 (header (or (plist-get row-attributes 'textile-header)
                             (plist-get cell-attributes 'textile-header))))
            (textile-delete-attributes cell-attributes)
            (textile-tag-insert
             (if header
                 "th"
               "td")
             cell-attributes)
            (re-search-forward "|" nil t)
            (replace-match "")
            (textile-end-tag-insert (if header
                                        "th"
                                      "td"))))
        (textile-end-tag-insert "tr")
        (re-search-forward "\n" nil t)))))

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
        (let ((attributes (textile-attributes " ")))
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

(defun textile-block-clear ()
  "Pass a \"clear:left|right|both\" style to the next block.
The only valid attributes to include in here are \"<\" or \">\" for clearing
left or right floating, or nothing for the default of \"both\"."
  (if (looking-at "clear\\([<>]?\\)\\. *")
      (let ((attrib-string (match-string 1)))
        (re-search-forward "clear\\([<>]?\\)\\. *\n+" nil t)
        (replace-match "")
        (setq Textile-next-block-clear
              (cond
               ((string= attrib-string "<")
                "clear: left; ")
               ((string= attrib-string ">")
                "clear: right; ")
               (t "clear: both; "))))
    (textile-error "Clear block's attribute string must be <, >, or nothing.")
    (textile-next-paragraph)))

(defun textile-block-dl (attributes)
  "Handle the definition list block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this definition list."
  (if (plist-get attributes 'textile-extended)
      (textile-error "Extended <dl> block doesn't make sense.")
    (textile-delete-tag "dl")
    (textile-tag-insert "dl" attributes)
    (textile-process-definition-block)
    (textile-end-of-paragraph)
    (textile-end-tag-insert "dd")
    (textile-end-tag-insert "dl")
    (textile-next-paragraph)))

(defun textile-block-ol (l-attributes)
  "Handle the ordered list block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this ordered list."
  (delete-region (point)
                 (re-search-forward "#" nil t))
  (textile-tag-insert "ol" l-attributes)
  (insert "\n")
  (let ((attributes (textile-attributes " ")))
    (textile-delete-attributes attributes)
    (textile-tag-insert "li" attributes))
  (textile-process-list-block)
  (textile-end-of-paragraph)
  (insert "</li>\n</ol>")
  (textile-next-paragraph))

(defun textile-block-ul (l-attributes)
  "Handle the unordered list block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this unordered list."
  (delete-region (point)
                 (re-search-forward "\\*" nil t))
  (textile-tag-insert "ul" l-attributes)
  (insert "\n")
  (let ((attributes (textile-attributes " ")))
    (textile-delete-attributes attributes)
    (textile-tag-insert "li" attributes))
  (textile-process-list-block)
  (textile-end-of-paragraph)
  (insert "</li>\n</ul>")
  (textile-next-paragraph))

(defun textile-block-table (attributes)
  "Handle the table block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this table."
  (if (looking-at (textile-this-block-tag-regexp "table"))
      (textile-delete-tag "table"))
  (if (plist-get attributes 'textile-extended)
      (textile-error "Extended <table> block doesn't make sense.")
    (textile-tag-insert "table" attributes)
    (textile-process-table-block)
    (textile-end-of-paragraph)
    (textile-end-tag-insert "table")
    (textile-next-paragraph)))

(defun textile-block-escape ()
  "Handle the escaped block starting at (point).
Finish at the beginning of the next paragraph, having completely
ignored this escaped block."
  (delete-region
   (save-excursion
     (beginning-of-line)
     (point))
   (save-excursion
     (end-of-line)
     (if (looking-at "\n")
         (forward-char 1))
     (point)))
  (re-search-forward "^== *$" nil t)
  (delete-region
   (save-excursion
     (beginning-of-line)
     (point))
   (save-excursion
     (end-of-line)
     (if (looking-at "\n")
         (forward-char 1))
     (point)))
  (textile-next-paragraph))

(defun textile-block-p (attributes)
  "Handle the paragraph block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this paragraph."
  (if (plist-get attributes 'textile-extended)
      (textile-error "Extended <p> block doesn't make sense.")
    (if (looking-at (textile-this-block-tag-regexp "p"))
        (textile-delete-tag "p"))
    (textile-tag-insert "p" attributes)
    (textile-process-block)
    (textile-end-of-paragraph)
    (textile-end-tag-insert "p")
    (textile-next-paragraph)))

(defun textile-block-footnote (attributes)
  "Handle the footnote starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this footnote."
  (if (plist-get attributes 'textile-extended)
      (textile-error "Extended fn? block doesn't make sense.")
    (textile-delete-tag (plist-get attributes 'id))
    (textile-tag-insert "p" attributes)
    (insert "<sup>" (plist-get attributes 'textile-fn-num) "</sup> ")
    (textile-process-block)
    (textile-end-of-paragraph)
    (textile-end-tag-insert "p")
    (textile-next-paragraph)))

(defun textile-block-header (attributes)
  "Handle the header block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this header."
  (if (plist-get attributes 'textile-extended)
      (textile-error "Extended <h?> block doesn't make sense.")
    (let ((my-tag (concat "h" (plist-get attributes 'textile-hlevel))))
      (textile-delete-tag my-tag)
      (textile-tag-insert my-tag attributes)
      (textile-process-block)
      (textile-end-of-paragraph)
      (textile-end-tag-insert my-tag)
      (textile-next-paragraph))))

(defun textile-block-bq (attributes)
  "Handle the blockquote block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this blockquote."
  (textile-delete-tag "bq")
  (textile-tag-insert "blockquote" attributes)
  (if (plist-get attributes 'textile-extended)
      (while
          (progn
            (textile-tag-insert "p" nil)
            (textile-process-block)
            (textile-end-of-paragraph)
            (textile-end-tag-insert "p")
            (when (save-excursion
                    (textile-next-paragraph)
                    (and (not (looking-at textile-any-block-tag-regexp))
                         (not (eobp))))
              (textile-next-paragraph))))
    (textile-tag-insert "p" nil)
    (textile-process-block)
    (textile-end-of-paragraph)
    (textile-end-tag-insert "p"))
  (textile-end-tag-insert "blockquote")
  (textile-next-paragraph))

(defun textile-block-bc (attributes)
  "Handle the blockcode block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this preformatted block of code."
  (textile-delete-tag "bc")
  (textile-tag-insert "pre" attributes)
  (textile-tag-insert "code" nil)
  (if (plist-get attributes 'textile-extended)
      (while
          (progn
            (textile-process-pre-block)
            (textile-end-of-paragraph)
            (when (save-excursion
                    (textile-next-paragraph)
                    (and (not (looking-at textile-any-block-tag-regexp))
                         (not (eobp))))
              (textile-next-paragraph))))
    (textile-process-pre-block)
    (textile-end-of-paragraph))
  (textile-end-tag-insert "code")
  (textile-end-tag-insert "pre")
  (textile-next-paragraph))

(defun textile-delete-tag (&optional tag)
  "Delete the standard textile block tag at (point), or TAG.
TAG is a regular expression."
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
  "Break with an error if textile-error-break is t.
Otherwise just flash the error into the Messages buffer and
continue."
  (if textile-error-break
      (error "%s" error-message)
    (message "Textile error: %s" error-message)))

(defun textile-region (start end)
  "Call textile-code-to-blocks on region from point to mark."
  (interactive "r")
  (textile-code-to-blocks start end))

(defun textile-buffer ()
  "Call textile-code-to-blocks on the entire buffer."
  (interactive)
  (textile-code-to-blocks (point-min) (point-max)))

(defun textile-tag-insert (tag my-plist)
  "Insert HTML tag TAG with attributes based on MY-PLIST.
Any attributes that start with \"textile-\" will be ignored."
  (insert "<" tag (textile-generate-attribute-string my-plist) ">"))

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

(defun textile-end-tag-insert (tag)
  "Close the HTML tag corresponding to TAG."
  (insert (concat "</" tag ">")))

(defun textile-next-paragraph ()
  "Go to the beginning of the next paragraph, as defined by Textile."
  (re-search-forward "^\n+" nil 1))

(defun textile-end-of-paragraph ()
  "Go to the end of this paragraph, as defined by Textile."
  (textile-next-paragraph)
  (backward-char 1)
  (while (looking-at "\n")
    (backward-char 1))
  (forward-char 1))

(defun textile-delete-attributes (attributes)
  "Delete textile-tagged attributes starting at (point)."
  (delete-region (point)
                 (+ (point)
                    (plist-get attributes
                               'textile-attrib-string-length))))

(provide 'textile)