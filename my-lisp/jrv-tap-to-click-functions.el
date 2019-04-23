(provide 'jrv-tap-to-click-functions)

;; Disable mousepad tap-to-click inside emacs and enable it outside

(defun tap-to-click-disable()
  "Disable mousepad tap-to-click inside emacs. Add to focus-in hook"
  ;; the magic codes here are for ETPS/2 Elantech Touchpad
  (start-process
   "t2c-disable"
   "*tap-to-click*"
   "xinput"
   "set-prop"
   "ETPS/2 Elantech Touchpad"
   "Synaptics Tap Action"
   "2," "3," "0," "0," "0," "0," "0"))

(defun tap-to-click-enable()
  "Enable mousepad tap-to-click outside emacs. Add to focus-out hook"
  ;; the magic codes here are for ETPS/2 Elantech Touchpad
  (start-process
   "t2c-disable"
   "*tap-to-click*"
   "xinput"
   "set-prop"
   "ETPS/2 Elantech Touchpad"
   "Synaptics Tap Action"
   "2," "3," "0," "0," "1," "3," "2"))
