(define-minor-mode remove-trailing-whitespace-mode
  "Toggle remove trailing whitespace on save.
When enabled trailing whitespace is removed before saving."
  :init-value nil
  :global t
  :lighter " W"

  (if remove-trailing-whitespace-mode
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

(defun senny-next-line ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun senny-kill-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun senny-duplicate-line ()
  "duplicate the current line on the line below it."
  (interactive)
  (beginning-of-line)
  (copy-region-as-kill (point) (progn (end-of-line) (point)))
  (senny-next-line)
  (yank)
  (beginning-of-line)
  (indent-according-to-mode))

(defun senny-comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun senny-comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark))
        )
    (senny-comment-or-uncomment-line lines)))

(defun senny-cleanup-buffer ()
  "Perform task such as auto-indent, untabify and delete trailing whitespace
on the current buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
		    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))


(provide 'defuns-cfg)
