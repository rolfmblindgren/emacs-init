(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

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

(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-engine 'xetex
        TeX-tree-roots '("/usr/local/texlive/2024")))

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
