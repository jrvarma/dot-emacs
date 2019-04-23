;;; text functions: proper case, spelling, space to dash etc
(provide 'jrv-text-functions)

(defun jrv/text-wc (&optional start end)
   "Prints number of lines, words and characters in region or whole buffer."
   ;; taken from http://www.emacswiki.org/emacs/WordCount
   (interactive)
   (let ((n 0)
         (start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max))))
     (save-excursion
       (goto-char start)
       (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
     (message "wc: %3d %3d %3d" (count-lines start end) n (- end start))))

(defun jrv/text-proper-case-region (beginning end)
  "Proper (title) case region. Capitalize 1st letter of words except articles prepositions etc"
  (interactive "r")
;;; 0. Save current state
  (save-excursion
    (text-mode)
;;; 2. Capitalize the whole region (initial letter of each word is capital)
    (capitalize-region beginning end)
;;; 3. set up regexp to match words which must not be capitalized
  (let (regexp) 
    (setq regexp  (concat 
;;; Beginning of word 
		   "\\b\\("                     
;;; Articles and coordinating conjunctions 
		   "A\\|An\\|The\\|And\\|Or\\|For\\|Nor"
;;; Prepositions 
;;;   This is a sublist from the list of 70 prepositions found 
;;;   http://grammar.englishclub.com/prepositions-list.htm
;;;   which itself is a sublist of over 150 prepositions listed in 
;;;   the ebook http://resources.englishclub.com/ebepl.htm
		   "\\|About\\|Above\\|Across\\|After\\|Against\\|Along\\|Among\\|Around\\|As\\|At"
		   "\\|Before\\|Beneath\\|Below\\|Beside\\|Besides\\|Between\\|Beyond\\|But\\|By"
		   "\\|Down\\|During\\|For\\|From\\|In\\|Inside\\|Into\\|Of\\|Off\\|On\\|Onto"
		   "\\|Through\\|To\\|Toward\\|Towards\\|Under\\|Underneath\\|Until\\|Up\\|Upon"
		   "\\|With\\|Within\\|Without"
;;; End of word 
		   "\\)\\b"
		   )
	  )
;;; 4. Run a while loop to downcase all special words
    (goto-char (1+ beginning)) ; do not downcase first letter of buffer
    (while (and (< (point) end)
		(re-search-forward regexp end t))
      (backward-word) 
      (downcase-word 1))
    )
  (normal-mode)
  (goto-char end)
  (deactivate-mark)))

(defun jrv/text-decapitalize-word()
  "Change 1st letter of word to lower case. Useful when autocomplete capitilizes word"
  (interactive)
  (backward-word) 
  (downcase-word 1))


(defun jrv/text-space-to-dash(beg end)
  "Change spaces to dash in region. Useful for example while making file names"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "[[:space:]\n]+" nil t)
        (replace-match "-" nil nil)))))
