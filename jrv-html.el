(provide 'jrv-html)

(defun html-double-quote ()
"Insert html entities"
   (interactive)
  	(insert "&ldquo;&rdquo;")
	(backward-char 7)
)

(defun html-single-quote ()
"Insert html entities"
   (interactive)
  	(insert "&rsquo;")
)

(defun html-comment ()
"Insert html comment"
   (interactive)
  	(insert "<!-- -->")
)

(defun a-href ()
"Insert link"
   (interactive)
  	(insert "<a href=\"\">")
)

(defun make-paras ()
"Convert double line break into html paras"
   (interactive)
	(perform-replace "

" "</p>

<p>"  nil t nil)
)

;; Alt+x describe-char was used to find the offending characters         ;
(defun entitify ()
"Convert quotes and dashes into html entities"
	(interactive)
;; first the quotes ;
    (goto-char (point-min))
	(perform-replace "\u2018" "&lsquo;"  nil t nil)
    (goto-char (point-min))
	(perform-replace "\u2019" "&rsquo;"  nil t nil)
    (goto-char (point-min))
	(perform-replace "\u201C" "&ldquo;"  nil t nil)
    (goto-char (point-min))
	(perform-replace "\u201D" "&rdquo;"  nil t nil)
;; now the dashes  ;
    (goto-char (point-min))
	(perform-replace "\u2013" "&ndash;"  nil t nil)
    (goto-char (point-min))
	(perform-replace "\u2014" "&mdash;"  nil t nil)
)

