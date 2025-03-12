(require 'package)
(add-to-list
 'package-archives
 '("melpa-stable" . "https://melpa.org/packages/") t)
(package-initialize)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin/:" (getenv "PATH")))

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(defun get-default-height ()
       (/ (display-pixel-height)
          (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-add-environments
             '("letter" LaTeX-env-args ["Address"] 0))))

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

(global-unset-key (kbd "C-z"))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(setq mac-option-key-is-meta nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq treesit-extra-load-path
      '("/Users/roffe/Library/Application Support/tree-sitter"))

(mapcar #'frame-width
	(frames-on-display-list))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (let* ((all-frames (frames-on-display-list))
                     (rightmost-left 0))
                (dolist (frame all-frames)
                  (let ((frame-left (frame-parameter frame 'left))
                        (frame-width (frame-pixel-width frame)))
                    (setq rightmost-left
                          (max rightmost-left (+ frame-left frame-width)))))
                (add-to-list 'default-frame-alist `(left . ,rightmost-left))
                (add-to-list 'default-frame-alist '(top . 0)))))

(add-to-list 'default-frame-alist
             `(height . ,(/  (display-pixel-height) (line-pixel-height))))

(dolist (path '("/usr/local/bin" "/opt/homebrew/bin" "/Library/TeX/texbin"))
  (setenv "PATH" (concat path ":" (getenv "PATH"))))

(setenv "LC_ALL" "en_US.UTF-8")

(setq tramp-default-method "sshx")

(defun resolve-jobname-to-actual-filename ()
  "Hvis dokumentet bruker \\jobname i \\addbibresource, erstatt det med faktisk filnavn for RefTeX sin skyld."
  (when (and (buffer-file-name)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward
		"\\\\addbibresource{\\\\jobname\\.bib}" nil t)))
    (let* ((basename (file-name-base (buffer-file-name)))
           (actual-bib (concat basename ".bib"))
           (tex-file (buffer-file-name))
           (bib-dir (file-name-directory tex-file))
           (bib-path (expand-file-name actual-bib bib-dir)))
      (if (file-exists-p bib-path)
          (progn
            (message "RefTeX hack: Mapper \\jobname.bib til %s" bib-path)
            (setq-local reftex-extra-bindings
                        (list (cons "\\jobname.bib" bib-path))))
        (message "Advarsel: Kunne ikke finne bib-fil for \\jobname (%s)"
		 bib-path)))))

(add-hook 'LaTeX-mode-hook #'resolve-jobname-to-actual-filename)

(use-package web-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package polymode
  :ensure t
  :config
  (setq polymode-exporter-output-file-format "%s"
        polymode-weaver-output-file-format "%s"))

(use-package ess
  :ensure t
  :config
  (setq ess-style 'GNU
        inferior-ess-r-program "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/R"))

(defun sync-bibinputs-with-texlive ()
  "Synkroniser Emacs sin BIBINPUTS med TeXLive sin via kpsewhich."
  (let ((bibpath (string-trim (shell-command-to-string "kpsewhich -show-path=bib"))))
    (setenv "BIBINPUTS" bibpath)))

(defun my/LaTeX-setup ()
  "Alt jeg vil gjøre når jeg åpner en LaTeX-fil."
  (turn-on-reftex)
  (sync-bibinputs-with-texlive)
  (resolve-jobname-to-actual-filename)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-format 'biblatex))  ;; siden du bruker addbibresource

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . my/LaTeX-setup)
  :config
  (setq TeX-engine 'xetex
        TeX-tree-roots '("/usr/local/texlive/2024")))

(use-package reftex
  :ensure nil  ;; følger med Emacs
  :after auctex)  ;; ikke nødvendig med mer hook her – vi styrer alt via my/LaTeX-setup

(use-package lua-mode
  :ensure t
  :config
  (setq lua-default-application "/opt/homebrew/bin/lua"))

(use-package python
  :ensure t
  :config
  (setq python-indent-offset 2
        python-interpreter "/opt/homebrew/bin/python3"
        python-shell-interpreter "/opt/homebrew/bin/python3"))

(use-package ace-window
  :ensure t)

(use-package sly
  :ensure t
  :config
  :hook (lambda ()
            (unless (sly-connected-p)
              (save-excursion (sly))))
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(push "/usr/local/src/eplot/" load-path)
(autoload 'eplot "eplot" nil t)
(autoload 'eplot-mode "eplot" nil t)
(unless (assoc "\.plt" auto-mode-alist)
  (setq auto-mode-alist (cons '("\.plt" . eplot-mode) auto-mode-alist)))

(load (expand-file-name "~/.emacs.d/desktop-options"))
(load (expand-file-name "~/.emacs.d/anddo"))

(load custom-file)
