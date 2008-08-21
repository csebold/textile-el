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
          (push "ol" my-list))
         ((= (char-after) ?*)
          (push "ul" my-list)))
        (forward-char 1)))
    my-list)) 

