(let
    ((el (expand-file-name "~/.emacs.d/init.el"))
     (elc (expand-file-name "~/.emacs.d/init.elc")))
  (if (file-newer-than-file-p el elc)
      (byte-compile-file el))
  (load elc))

