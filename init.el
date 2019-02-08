;; cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24) (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; appearance
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'morning-star t)
(add-to-list 'load-path "~/.emacs.d/packages/")
(require 'dired+)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(treemacs-icons-dired-mode)
;(setq treemacs-no-png-images t)

;; encoding
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)

(setq load-prefer-newer t)
(setq default-korean-keyboard "2")

(require 'ucs-normalize)
(set-file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; misc
;(setq path "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/go/bin")
(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Make *scratch* buffer blank.
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq gdb-many-windows t)
(setq x-select-enable-clipboard t) ; Share the clipboard with x-window application
(setq make-backup-files nil)	   ; stop creating backup~ files
(setq auto-save-default nil)       ; stop creating #autosave# files
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)
(setq debug-on-error t)

(icomplete-mode)
(which-function-mode)

(put 'set-goal-column 'disabled nil)

(column-number-mode t)			; show column number
(auto-compression-mode t)
(display-time)				; Display time
(tooltip-mode t)			; Use tooltip
(when (functionp 'tool-bar-mode)        ; Don't use toolbar
  (tool-bar-mode -1))
(menu-bar-mode -1)			; Don't use menubar
(if window-system
    (scroll-bar-mode -1))               ; Don't use scrollbar
(setq inhibit-startup-message t)	; Inhibit startup message
(show-paren-mode t)                     ; Show parenthesis match
(transient-mark-mode t)			; Highlight region

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; font
(set-face-bold 'bold nil)
(set-default-font "menlo 18")
(add-to-list 'default-frame-alist
             '(font . "menlo 18"))

(setq mac-allow-anti-aliasing t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
(global-font-lock-mode t)		; Syntax highlight

(put 'narrow-to-region 'disabled nil)

;; keybinding
(when (and (eq system-type 'darwin) window-system)
  (setq ns-use-native-fullscreen nil)
  ;;(setq mac-command-key-is-meta 1)
  (setq mac-command-modifier 'super)
  (global-set-key (kbd "<C-s-268632090>") 'toggle-frame-maximized)
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "s-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-M-h") 'ns-do-hide-others)
  (global-set-key (kbd "s-0") 'treemacs)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(global-set-key "\r" 'newline-and-indent) ; auto indentation
(global-set-key (kbd "C-x t") 'treemacs-select-window)

;(global-set-key "\C-z" 'run-ansi-term)
;(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'undo)

(global-set-key [C-S-iso-lefttab] 'previous-buffer)
(global-set-key [C-tab] 'next-buffer)

;(global-set-key (kbd "M-]") 'next-multiframe-window)
;(global-set-key (kbd "M-[") 'previous-multiframe-window)

(global-set-key (kbd "M-s") 'shell-command)
(global-set-key (kbd "M-c") 'compile)
(global-set-key (kbd "M-a") 'run-ansi-term)

(defun run-ansi-term ()
  (interactive)
  (if (equal "*ansi-term*" (buffer-name))
      (call-interactively 'rename-buffer)
    (if (get-buffer "*ansi-term*")
	(switch-to-buffer "*ansi-term*")
      (ansi-term "/bin/bash"))))

(global-set-key (kbd "C-x g") 'revert-buffer)

;; code
(setq-default truncate-lines t)
(setq-default line-spacing 2)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; objc
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defadvice ff-get-file-name (around ff-get-file-name-framework
				    (search-dirs
				     fname-stub
				     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
	      (header (match-string 2 fname-stub))
	      (fname-stub (concat framework ".framework/Headers/" header)))
	 ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))


(add-hook 'objc-mode-hook (lambda ()
			    (setq c-basic-offset 2)))

(add-hook 'objc-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; rust
(autoload 'rust-mode "rust-mode" nil t)
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; swift
(require 'swift-mode)

;; go
(require 'go-mode)
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 2)
	    (setq standard-indent 2)
	    (setq indent-tabs-mode nil)))

;; java
(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)))

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; python
(add-hook 'python-mode-hook '(lambda ()
			       (setq python-indent 4)))

;; javascript
(setq js-indent-level 2)

;; css
(setq css-indent-offset 2)

;; html
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(put 'erase-buffer 'disabled nil)

;; treemacs
(setq treemacs-is-never-other-window t)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-switch-project-action #'projectile-dired)
