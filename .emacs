(setenv "PATH" "/Library/TeX/texbin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
(setq exec-path '("/Library/TeX/texbin" "/opt/homebrew/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"))

(setq epg-gpg-program "/opt/homebrew/bin/gpg")
(setq auth-sources '("~/.authinfo.gpg"))
(setq epa-pinentry-mode 'loopback)

(defun update-init-file-if-newer ()
  "Henter init.el fra WebDAV hvis den er nyere enn den lokale versjonen."
  (let* ((host "www.grendel.no")
         (user "webdav")
         (password (auth-source-pick-first-password :host host :user user))
         (url (format "https://%s:%s@%s/webdav/init.el" user password host))
         (local-file "~/.emacs.d/init.el"))
    (when (file-newer-than-file-p url local-file)
      (url-copy-file url local-file t)
      (message "Hentet ny versjon av init.el fra WebDAV!"))))

(update-init-file-if-newer)


(let
    ((el (expand-file-name "~/.emacs.d/init.el"))
     (elc (expand-file-name "~/.emacs.d/init.elc")))
  (if (file-newer-than-file-p el elc)
      (byte-compile-file el))
  (load elc))


