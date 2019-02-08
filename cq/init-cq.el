(use-package cq
  ;;:require org
  ;;:commands (cq-find-file-hook clearcase-font-lock cq-font-lock)
  :if (eq system-type 'windows-nt)
  :config
  (add-hook 'org-mode-hook 'clearcase-font-lock)
  (add-hook 'org-agenda-mode-hook 'clearcase-font-lock)
  (add-hook 'bibtex-mode-hook 'clearcase-font-lock)
  (add-hook 'prog-mode-hook 'clearcase-font-lock)

  (add-hook 'org-mode-hook 'cq-font-lock)
  (add-hook 'org-agenda-mode-hook 'cq-font-lock)
  (add-hook 'bibtex-mode-hook 'cq-font-lock)
  (add-hook 'prog-mode-hook 'cq-font-lock)

  (add-hook 'find-file-hook 'cq-find-file-hook)
  )

(use-package cqtbl
  :commands (cqtbl-merge-tables)
  )
