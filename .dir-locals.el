;;; Directory Local Variables
;;; Adapted from section 49.2.5 Per-Directory Local Variables
;;; eval the following LISP expression (info "(emacs) Directory Variables")


((nil . ((indent-tabs-mode . t)(fill-column . 40) (tab-width . 4)))
      (f90-mode . ((fill-column . 80) (f90-do-indent . 3) (f90-auto-keyword-case . upcase-word)))
      (prog-mode . ((fill-column . 60)))
      (compile-command . cmake)
      ("prog"
       . ((nil . ((change-log-default-name
                   . "ChangeLog.local")))))
      ("narrow-files" . ((nil . ((fill-column . 20))))))



;;((nil . ((eval . (set (make-local-variable 'my-project-path)
;;                      (file-name-directory
;;                       (let ((d (dir-locals-find-file ".")))
;;                         (if (stringp d) d (car d))))))
;;         (cmake-ide-project-dir . my-project-path)
;;         (eval . (setq cmake-ide-build-dir (concat my-project-path "build")))
;;         )))
