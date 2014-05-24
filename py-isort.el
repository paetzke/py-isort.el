;;; py-isort.el --- Use isort to sort the imports in a Python buffer

;; Copyright (C) 2014, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: http://paetzke.me/project/py-isort.el
;; Version: 0.2

;;; Commentary:

;; Provides the `py-isort' command, which uses the external "isort"
;; tool to tidy up the imports in the current buffer.

;; To automatically sort imports when saving a python file, use the
;; following code:

;;   (add-hook 'before-save-hook 'py-isort-before-save)

;; To customize the behaviour of "isort" you can set the
;; py-isort-options e.g.

;;   (setq py-isort-options '("--lines=100"))

;;; Code:

(defgroup py-isort nil
  "Use isort to sort the imports in a Python buffer."
  :group 'convenience
  :prefix "py-isort-")


(defcustom py-isort-options nil
  "Options used for isort."
  :group 'py-isort
  :type '(repeat (string :tag "option")))


(defun py-isort-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in py-isort-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (incf line-offset len)
                (kill-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in py-isort-apply-rcs-patch")))))))))


;;;###autoload
(defun py-isort ()
  "Uses the \"isort\" tool to reformat the current buffer."
  (interactive)
  (let ((tmpfile (make-temp-file "isort" nil ".py"))
        (patchbuf (get-buffer-create "*isort patch*"))
        (errbuf (get-buffer-create "*isort Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))
    (write-region nil nil tmpfile)
    (if (zerop (apply 'call-process "isort" nil errbuf nil
                      (append `(" " ,tmpfile) py-isort-options)))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already isorted"))
          (py-isort-apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied isort."))
      (message "Could not apply isort. Check errors for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


;;;###autoload
(defun py-isort-before-save ()
  (interactive)
  (when (eq major-mode 'python-mode) (py-isort)))


(provide 'py-isort)


;;; py-isort.el ends here
