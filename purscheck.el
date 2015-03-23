;;; purscheck --- Purescript flycheck support for emacs

;;; Commentary:
;;; Uses a PureScript cheat mode

;;; Code:

(define-derived-mode purescript-mode haskell-mode "PureScript"
  "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker purs-check
       "Use purscheck to flycheck PureScript code."
       :command ("purscheck" source source-original temporary-file-name)
       :error-patterns
       ((error line-start
               (or (and (zero-or-more " ") "Error at " (file-name)    " line " line ", column " column (zero-or-more " ") (or ":" "-") (zero-or-more not-newline))
                   (and "\""        (file-name) "\" (line " line ", column " column "):"))
               (or (message (one-or-more not-newline))
                   (and "\n"
                        (message
                         (zero-or-more " ") (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (zero-or-more " ")
                                       (one-or-more not-newline)))))
               line-end))
       :modes purescript-mode)
     (add-to-list 'flycheck-checkers 'purs-check)))

(provide 'purscheck)

;;; purscheck ends here
