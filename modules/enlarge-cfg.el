(defvar senny-enlargement--window-configuration-store nil
  "Hash storing window configurations. persp-name -> list of configs.")

(defvar senny-enlargement--curr-conf-history nil
  "Temporary used list of configs when walking back the history.")

(defun senny-enlargement--create-winconfig-store-if-nil ()
  (if (eq senny-enlargement--window-configuration-store nil)
      (setq senny-enlargement--window-configuration-store (make-hash-table :test 'equal :size 10))))

(defun senny-enlargement--get-winconfigs-of-current-persp ()
  (senny-enlargement--create-winconfig-store-if-nil)

  (let* ((persp (persp-name persp-curr))
         (configs (gethash persp senny-enlargement--window-configuration-store)))
    (if (eq configs nil)
        '()
      configs)))

(defun senny-enlargement--set-winconfigs-of-current-persp (configs)
  (senny-enlargement--create-winconfig-store-if-nil)

  (let ((persp (persp-name persp-curr)))
    (puthash persp configs senny-enlargement--window-configuration-store)))

(defun senny-enlargement--get-current-winconfig ()
  (list (current-frame-configuration) (current-window-configuration)))

(defun senny-enlargement--apply-winconfig (cfg)
  (set-frame-configuration (car cfg))
  (set-window-configuration (car (cdr cfg))))

(defun senny-enlargement-enlarge ()
  (interactive)

  (unless (= (count-windows nil) 1)
    (unless (or (eq last-command 'senny-enlargement-enlarge)
                (eq last-command 'senny-enlargement-restore))
      (senny-enlargement--set-winconfigs-of-current-persp
       (cons (senny-enlargement--get-current-winconfig)
             (senny-enlargement--get-winconfigs-of-current-persp))))
    (delete-other-windows)))

(defun senny-enlargement-restore ()
  (interactive)

  (if (eq last-command 'senny-enlargement-restore)
      (if (not senny-enlargement--curr-conf-history)
          (message "Top reached: no more configurations to restore.")
        (senny-enlargement--apply-winconfig (pop senny-enlargement--curr-conf-history)))

    (let ((configs (senny-enlargement--get-winconfigs-of-current-persp)))
      (setq senny-enlargement--curr-conf-history (cdr configs))
      (senny-enlargement--apply-winconfig (car configs)))))

(provide 'enlarge-cfg)
