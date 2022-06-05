;;; init.el --- Jay Lee's init.el File For GNU Emacs

;; ===========================================================================
;; Cask
;; ===========================================================================

;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (when (< emacs-major-version 27)
;;   (cask-initialize))
;; (cask-initialize)

;; ===========================================================================
;; Package
;; ===========================================================================

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24) (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq packages-directory-p "~/.emacs.d/.packages")
(if (not (file-directory-p packages-directory-p))
    (make-directory packages-directory-p))
(add-to-list 'load-path packages-directory-p)
(setq package-user-dir packages-directory-p)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(package-initialize)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq byte-compile-warnings '(cl-functions))

;; ===========================================================================
;; Simple Settings
;; ===========================================================================

;; Speed Up startup
;; ----------------
;;
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

;; Appearance
;; ----------
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (window-system)
    (scroll-bar-mode -1)) ; don't use scrollbar in only gui
(tooltip-mode -1)
(display-time-mode -1)
(setq frame-title-format "\n") ;; remove annoying frame title about dimension.
;; (display-time)				      ; Display time
;; (setq display-time-default-load-average nil)
;; (setq display-time-day-and-date t)
;; (setq display-time-format "%a %I:%M %p")
(line-number-mode 1)			  ; line number mode in modeline
(column-number-mode 1)			; show column number
(show-paren-mode t)         ; Show parenthesis match
(transient-mark-mode t)			; Highlight region
;; (global-hl-line-mode 1)
(which-function-mode)
(setq which-func-unknown "?")
(set-face-attribute 'which-func nil :box '(:color "#000000") :bold t :foreground "#c98459")
(setq inhibit-startup-message t)	; Inhibit startup message
;; font
(global-font-lock-mode t)		; Enable syntax highlight
(set-face-bold 'bold nil)
(set-frame-font "menlo 14" nil t)
;; korean font setting
;; (set-frame-font "D2Coding 14" nil t)
;; (set-fontset-font nil 'hangul (font-spec :family "D2Coding" :size 14))
(setq font-lock-maximum-decoration t)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
;; make upper bar to black
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'morning-star t)
;; (set-fringe-mode '(0 . 0)) ;; remove fringe
(setq-default cursor-type 'bar)
(global-visual-line-mode 1)
;; default vertical split
(setq split-height-threshold nil) ;; vertical
(setq split-width-threshold 160)

;; Dired
;; -----
;;
(add-to-list 'load-path "~/.emacs.d/packages/")
(require 'dired+) ;; powerful dired
(use-package dired-filter)
(use-package dired-subtree)
;; reuse dired buffer so that dired buffer is not created more than one.
(diredp-toggle-find-file-reuse-dir 1)
;; show details in dired+.
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
;; hidden dotfiles
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda ()
			     ;; (dired-omit-mode 1)
			     (local-set-key (kbd "C-c o") 'dired-omit-mode)
			     ;; enable dired filter mode (external package)
			     ;; (dired-filter-mode)
			     ))
(setq dired-filter-stack nil)
;; (add-hook 'dired-mode-hook 'auto-revert-mode) ;; enable auto revert mode in dired mode.
(setq dired-dwim-target t) ;; convenient manipulating for files
(setq dired-omit-verbose nil) ;; hide annoying message about omit
;; sort directory first in dired
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-listing-switches "-alh") ;; human readable size in dired
(setq dired-use-ls-dired  nil)
(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-alh --group-directories-first") ;; human readable size in dired
;; dired-subtree (external package)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map "i" 'dired-subtree-insert)
(define-key dired-mode-map ";" 'dired-subtree-remove)
;; dired open file with ace window.
(require 'ace-window)
(defun find-file-ace-window ()
  "Use ace window to select a window for opening a file from dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (> (length (aw-window-list)) 1)
        (aw-select "" (lambda (window)
                        (aw-switch-to-window window)
                        (find-file file)))
      (find-file-other-window file))))
(define-key dired-mode-map "o" 'find-file-ace-window)
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ivy-completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal $sort-by "name") (setq $arg "-alh "))
     ((equal $sort-by "date") (setq $arg "-alh -t"))
     ((equal $sort-by "size") (setq $arg "-alh -S"))
     ;; ((equal $sort-by "dir") (setq $arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))
(define-key dired-mode-map (kbd "s") 'xah-dired-sort)

;; Global Keybindings
;; ------------------
;;
;; Unset Keybindings
(global-unset-key (kbd "C-x C-t"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-L"))
(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "s-'"))
(global-unset-key (kbd "C-c C-v"))
(global-unset-key (kbd "C-c C-p"))
;; Set Keybindings
;; (global-set-key (kbd "<S-SPC>") 'toggle-input-method)
(global-set-key (kbd "\r") 'newline-and-indent) ;; auto indentation
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "<C-S-tab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "C-x C-g") 'revert-buffer)
(global-set-key (kbd "<f5>") 'transpose-frame)
(global-set-key (kbd "<f6>") 'transpose-windows)
(global-set-key (kbd "s-t") 'eyebrowse-create-window-config)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-'") 'swiper)
(global-set-key [wheel-left] 'ignore)
(global-set-key [wheel-right] 'ignore)

;; Encoding
;; --------
;;
(setq load-prefer-newer t)
(setq default-korean-keyboard "2")
(setq default-input-method "korean-hangul")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;; encoding for mac os file system
(if (eq system-type 'darwin)
    (progn
      (require 'ucs-normalize)
      (set-file-name-coding-system 'utf-8-hfs) ;; UTF-8 based coding system for macOS HFS file names.
      (setq locale-coding-system 'utf-8-hfs)
      (prefer-coding-system 'utf-8)
      (set-buffer-file-coding-system 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8-unix)
      (set-next-selection-coding-system 'utf-8-unix)
      (set-selection-coding-system 'utf-8-unix)
      (set-terminal-coding-system 'utf-8-unix)
      (set-clipboard-coding-system 'utf-8-unix)
      ))

;; ===========================================================================
;; Miscellaneous Settings
;; ===========================================================================

;; Mac OS Settings
;; ---------------
;;

;; copy & paste
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
;;
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
;;
;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)
(setq mac-allow-anti-aliasing t)
;; keybinding
(when (and (eq system-type 'darwin) window-system) ;; check if mac os and gui
  (setq ns-use-native-fullscreen t)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  ;; mac style keybinding
  ;; (global-set-key (kbd "<C-s-268632090>") 'toggle-frame-maximized)
  (global-set-key (kbd "<M-return>") 'toggle-frame-maximized)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "s-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-M-h") 'ns-do-hide-others)
  (global-set-key (kbd "s-r") 'revert-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "S-s-w") 'delete-frame)
  (global-set-key (kbd "<s-backspace>") '(lambda () (interactive) (kill-line 0)))
  (global-set-key (kbd "s-w") 'eyebrowse-close-window-config)
  (global-set-key (kbd "<C-S-tab>") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "<C-tab>") 'eyebrowse-next-window-config)
  (global-set-key (kbd "s-{") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "s-}") 'eyebrowse-next-window-config)
  ;; (global-set-key (kbd "s-[") 'xah-previous-user-buffer)
  ;; (global-set-key (kbd "s-]") 'xah-next-user-buffer)
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "s-]") 'next-buffer)
  (global-set-key (kbd "s-`") 'other-frame)
  (global-set-key (kbd "s-~") '(lambda () (interactive) (other-frame -1)))
  (global-set-key (kbd "s-W") 'delete-frame)
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; set defulat frame to maximized.
  (global-set-key (kbd "<s-left>") 'projectile-previous-project-buffer)
  (global-set-key (kbd "<s-right>") 'projectile-next-project-buffer)
  )

;; tmp files
;; ---------
;;
(setq tmp-directory-p "~/.emacs.d/tmp") ;; tmp directory path for emacs
(if (not (file-directory-p tmp-directory-p)) ;; if tmp dir not exist, then create it.
    (make-directory tmp-directory-p))
(setq bookmark-file (expand-file-name "bookmarks" tmp-directory-p)) ;; bookmark
(setq recentf-save-file (expand-file-name "recentf" tmp-directory-p)) ;; recentf
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" tmp-directory-p)) ;; projectile
(setq undo-tree-history-directory-alist `(("." . ,tmp-directory-p))) ;; undo-tree
(setq save-place-file (expand-file-name "places" tmp-directory-p)) ;; save-place-mode
(setq projectile-cache-file (expand-file-name "projectile.cache" tmp-directory-p)) ;; save-place-mode
(setq tramp-persistency-file-name (expand-file-name ".tramp" tmp-directory-p))
(setq org-id-locations-file (expand-file-name ".org-id-locations" tmp-directory-p)) ;; !!!not sure file path
(setq python-environment-directory (expand-file-name ".python-environments" tmp-directory-p))
(setq transient-history-file (expand-file-name "history" tmp-directory-p))
(setq eshell-directory-name (expand-file-name "eshell" tmp-directory-p))
(setq auto-package-update-last-update-day-filename (expand-file-name ".last-package-update-day" tmp-directory-p))

;; scroll setup
;; ------------
;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 200)
(setq scroll-margin 3)
(setq hscroll-step 1)
(setq hscroll-margin 3)
(global-set-key [wheel-right] 'scroll-left)
(global-set-key [wheel-left] 'scroll-right)

;; no pop up frames
;; ----------------
;;
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;; save-desktop-mode in GUI
;; ------------------------
;; restore frame & window sessions
(if (display-graphic-p)
    (progn
      (setq desktop-path (list tmp-directory-p)
	    desktop-dirname tmp-directory-p
	    desktop-restore-eager 5
	    desktop-load-locked-desktop nil
	    desktop-lazy-verbose nil
	    desktop-save t)
      (desktop-save-mode 1)))

;; auto-revert-mode
;; ----------------
;;
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; save-placemode
;; ---------------
;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; delete annoyting buffers
;; ------------------------
;;
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq initial-scratch-message nil) ;; Make *scratch* buffer blank.
(setq inhibit-splash-screen t) ;; Inhibit startup screen.
(setq initial-scratch-message nil)

;; stop creating useless files
;; ---------------------------
;;
(setq make-backup-files nil) ;; stop creating backup~ files
(setq auto-save-default nil) ;; stop creating #autosave# files
(setq auto-save-list-file-prefix nil) ;; stop creating auto save list directory

;; recentf
;; -------
;;
(require 'recentf)
(setq recentf-filename-handlers
      (append '(abbreviate-file-name) recentf-filename-handlers))
(setq recentf-max-menu-items 10)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf (solution of tramp auto connection issue)
(recentf-mode 1)

;; transpose frame & window
;; ------------------------
;;
(require 'transpose-frame)
(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

;; user buffer & emacs buffer
;; --------------------------
;; cycle only user buffer or emacs buffer
(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of 'user buffer'.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (or (string-prefix-p "magit" (prin1-to-string major-mode)) (string-equal major-mode "dired-mode"))
	nil
      t
      )))
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
'user buffer' is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))
(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
'user buffer' is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))
(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
'emacs buffer' here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))
(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
'emacs buffer' here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))
(global-set-key (kbd "<s-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<s-f12>") 'xah-next-emacs-buffer)

;; Uniquify
;; --------
;; readability of buffer file name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
;; (setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)    ;; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

;; Others
;; ------
;;
(defalias 'yes-or-no-p 'y-or-n-p) ;; yes, no -> y, n when confriming something
(setq ad-redefinition-action 'accept) ;; omit warning about redefining function
(setq echo-keystrokes 0.001) ;; remove delay when using keystroke
(auto-compression-mode t)
(setq ring-bell-function 'ignore) ;; Turn off alarm
(setq debug-on-error nil) ;; disable debugging message
(setq select-enable-clipboard t) ;; Share the clipboard with x-window application
(setq disabled-command-function nil) ;; Enable all disabled command
(setq load-prefer-newer t) ;; you're never accidentally using outdated compiled files.
(setq ispell-program-name "/usr/local/bin/ispell")
(flyspell-mode) ;; check spelling
(delete-selection-mode 1) ;; replacing highlight text
(require 'tramp)
(setq tramp-default-method "ssh") ;; set tramp (Transparent Remote Access) default method to ssh
;; disable prompt when killing buffers running process.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))
(setq-default blink-cursor-delay 0) ;; solve disappearing cursor using ace-window or moving cursor fast.
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; ===========================================================================
;; Code For Development
;; ===========================================================================

;; settings
(setq-default truncate-lines t)
(setq-default line-spacing 2)
(add-hook 'before-save-hook #'delete-trailing-whitespace) ;; delete whitespace when file was saved.

;; sh
;; --
;;
(setq sh-basic-offset 2)

;; objc
;; ----
;;
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))
;;
(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
;;
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
;;
(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))
;;
(add-hook 'objc-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; rust
;; ----
;;
(autoload 'rust-mode "rust-mode" nil t)
(use-package rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; swift
;; -----
;;
(use-package swift-mode)

;; go
;; --
;;
(use-package go-mode)
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode 1)
	    (setq tab-width 2)))
(add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mode))

;; java
;; ----
;;
(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))
;;
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)))
;;
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; python
;; ------
;;
(add-hook 'python-mode-hook (lambda ()
			      (setq python-indent-offset 2)
			      (local-unset-key (kbd "C-c C-s"))
			      (local-unset-key (kbd "C-c C-v"))))

;; javascript
;; ----------
;;
(setq js-indent-level 2)

;; css
;; ---
;;
(setq css-indent-offset 2)

;; web
;; ---
;;
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ;; You can also edit plain js, jsx, css, scss, xml files.
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2) ;; javascript, php
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-css-colorization t)
(add-hook 'web-mode-hook (lambda () (local-unset-key (kbd "C-c C-s"))))

;; markdown
;; --------
;;
(use-package markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "/usr/local/bin/pandoc")

;; yaml
;; ----
;;
(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; org
;; ---
;;
(use-package org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-goto-auto-isearch nil) ;; disable auto search in org goto
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; Default directoris for my notes and website.
(setq org-log-done t) ;; log timestamps when todo is done.
(setq org-directory (expand-file-name "~/org"))
(setq org-default-notes-file (concat org-directory "/mygtd.org"))
(setq org-agenda-files '("~/org"))
;; todo
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "BACKLOG(!)" "READY(!)" "PROGRESS(!)" "|" "DONE")
        ))
(setq org-todo-keyword-faces
      '(("BACKLOG" . (:background "#CF5D50" :foreground "#1a1a1a" :weight bold :box '(:line-width -1 :color "#000000")))
	("READY" . (:background "#D2903A" :foreground "#1a1a1a" :weight bold :box '(:line-width -1 :color "#000000")))
	("PROGRESS" . (:background "#D0BA49" :foreground "#1a1a1a" :weight bold :box '(:line-width -1 :color "#000000")))
	)) ;; inspired by trello
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda ()
			   (setq-local global-hl-line-mode nil)
			   (org-bullets-mode 1)
			   (set-face-bold 'bold 1) ;; set face to bold in only org mode.
			   (local-unset-key (kbd "C-c C-v"))
			   (local-set-key (kbd "C-'") 'swiper)
			   ))
;; (use-package org-tempo) ;; org template expansion using tab
;; org-babel
(org-babel-do-load-languages ;; add programming languages to org babel list
 'org-babel-load-languages
 '((python . t)))
(setq org-reverse-note-order t)
;; capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/mygtd.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("k" "Kanban" entry (file+headline "~/org/mygtd.org" "Tasks")
         "* BACKLOG %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ))

;; nginx
;; -----
;; nginx config mode
(use-package nginx-mode)

;; dockerfile
;; ----------
;; dockerfile mode
(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ssh-config-mode
;; ---------------
;; autoload
(use-package ssh-config-mode)

;; pip-requirements
;; ----------------
;; autoload
(use-package pip-requirements)

;; groovy-mode
;; -----------
;;
(use-package groovy-mode)
(setq groovy-indent-offset 2)

;; ============================================================================
;; external Packages
;; ============================================================================

;; magit
;; -----
;;
(use-package magit)
(setq auto-revert-check-vc-info t) ;; update branch name when reverting magit buffer
(setq vc-handled-backends '(Git))
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
;;(setq vc-handled-backends nil) ;; for performance
;;(setq magit-refresh-status-buffer nil)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; projectile
;; ----------
;;
(use-package projectile)
(projectile-mode +1)
(setq projectile-use-git-grep t)
(setq projectile-completion-system 'ivy)
(setq projectile-switch-project-action #'projectile-commander)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c p K") 'projectile-remove-known-project)
(global-set-key (kbd "C-c p h") 'projectile-previous-project-buffer)
(add-to-list 'projectile-other-file-alist '("html" "js" "css" "go"))
(add-to-list 'projectile-other-file-alist '("js" "html" "css" "go"))
(add-to-list 'projectile-other-file-alist '("css" "html" "js" "go"))
(add-to-list 'projectile-other-file-alist '("go" "html" "js" "css"))
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'hybrid)
(setq projectile-sort-order 'recently-active)

;; eyebrowse
;; ---------
;;
(use-package eyebrowse)
(eyebrowse-mode t)
(setq eyebrowse-new-workspace t
      eyebrowse-wrap-around t)
(set-face-attribute 'eyebrowse-mode-line-active nil :box '(:color "#000000") :bold t :foreground "#c98459") ;; #000 is black2 #c98 is orange1 in morning star
(global-set-key (kbd "s-'") 'eyebrowse-last-window-config)
(global-set-key (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "s-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "s-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "s-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "s-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "s-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "s-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "s-9") 'eyebrowse-switch-to-window-config-9)
(setq eyebrowse-mode-line-style 'current)

;; ibuffer-projectile
;; ------------------
;;
(use-package ibuffer-projectile)
(setq ibuffer-expert t) ;; disable prompt when deleting modified buffer.
(define-key ibuffer-mode-map (kbd "M-o") 'ace-window) ;; override ibuffer mode map for M-o
(add-hook 'ibuffer-hook (lambda ()
			  (ibuffer-auto-mode 1) ;; keeps ibuffer list up to date
			  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*") ;; disable to show asterisk buffer
			  ;; (add-to-list 'ibuffer-never-show-predicates "magit*")
			  (setq ibuffer-show-empty-filter-groups nil) ;; don't show empty group
			  ;; (ibuffer-vc-set-filter-groups-by-vc-root)
			  (ibuffer-projectile-set-filter-groups)
			  (ibuffer-do-sort-by-recency)
			  (local-set-key (kbd "<tab>") 'toggle-ibuffer-all-filter-groups)
			  (visual-line-mode -1)))
;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
    (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name."
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
;; hide dired buffer in ibuffer.
(defun my-dired-mode-buffer-p (buf)
  "Non-nil if buffer BUF is in `dired-mode'."
  (with-current-buffer buf
    (derived-mode-p 'dired-mode)))
;; (with-eval-after-load "ibuffer"
;;   (add-to-list 'ibuffer-never-show-predicates #'my-dired-mode-buffer-p))
(defface my-ibuffer-filter-group-name-face
  '((t :foreground "#f2777a"
       ))
  "Face for bifufer-filter-group-name."
  )
(setq ibuffer-filter-group-name-face 'my-ibuffer-filter-group-name-face)
;; ibuffer collaps all & expand
(defun ibuffer-collapse-all-filter-groups ()
  "Collapse all filter groups at once"
  (interactive)
  (setq ibuffer-hidden-filter-groups
        (mapcar #'car (ibuffer-current-filter-groups-with-position)))
  (ibuffer-update nil t))
(defun ibuffer-expand-all-filter-groups ()
  "Expand all filter groups at once"
  (interactive)
  (setq ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))
;; toggle ibuffer filter groups
(defun toggle-ibuffer-all-filter-groups ()
  "Toggle background color between seashell and honeydew.
URL `http://ergoemacs.org/emacs/elisp_toggle_command.html'
Version 2015-12-17"
  (interactive)
  ;; use a property 'state'. Value is t or nil
  (if (get 'toggle-ibuffer-all-filter-groups 'state)
      (progn
	(call-interactively 'ibuffer-collapse-all-filter-groups)
        (put 'toggle-ibuffer-all-filter-groups 'state nil))
    (progn
      (call-interactively 'ibuffer-expand-all-filter-groups)
      (put 'toggle-ibuffer-all-filter-groups 'state t))))

;; ace-window
;; ----------
;;
(use-package ace-window)
(when (fboundp 'winner-mode) ;; if winner mode exist, enable it.
  (winner-mode 1))
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
      aw-dispatch-always t
      aw-dispatch-alist
      '((?x aw-delete-window "Delete Window")
        (?c aw-swap-window "Swap Window")
        (?n aw-flip-window)
        (?v aw-split-window-vert "Split Vert Window")
        (?h aw-split-window-horz "Split Horz Window")
        (?m delete-other-windows "Maximize Other Window")
        (?g delete-other-windows)
        (?b balance-windows)
        (?u (lambda ()
              (progn
                (winner-undo)
                (setq this-command 'winner-undo))))
        (?r winner-redo)
	(?? aw-show-dispatch-help)))

;; ivy & swiper & counsel
;; ----------------------
;;
(use-package ivy)
(ivy-mode 1)
(setq ivy-wrap t)
(setq ivy-use-ignore-default 'always)
;; (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
;; ;; hide buffers by major mode.
;; (defun my-ivy-ignore-buffers (buf)
;;   (when (get-buffer buf)
;;     (with-current-buffer buf
;;       (when (or (eq major-mode 'dired-mode) (string-prefix-p "magit" (prin1-to-string major-mode)))
;;         t))))
;; (add-hook 'ivy-ignore-buffers 'my-ivy-ignore-buffers)
(setq ivy-use-virtual-buffers t) ;; add recnet file to switch buffer.
(setq ivy-virtual-abbreviate 'abbreviate)
(setq ivy-count-format "(%d/%d) ")
;; (setq enable-recursive-minibuffers t)
;; (minibuffer-depth-indicate-mode)
(global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-c C-s") 'swiper)
;; (global-set-key (kbd "C-c C-b") 'swiper-all)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-.") 'counsel-imenu)
(use-package counsel-projectile)
(counsel-projectile-mode 1)
(use-package ivy-hydra)
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy))) ;; fuzzy maching
;; (setq ivy-initial-inputs-alist nil) ;; (optional) fuzzy maching
(use-package ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)
(setq ivy--highlight-function 'hl-line)
(setq ivy-fixed-height-minibuffer 1)
(setq ivy-height 15)

;; which-key
;; ---------
;; cheatsheet
(use-package which-key)
(which-key-mode)

;; multi-term
;; ----------
;;
(use-package multi-term)
(setq multi-term-program "/bin/zsh")
;;(setq multi-term-program-switches "--login")
(global-set-key (kbd "C-c v") 'multi-term)
(global-set-key (kbd "C-c C-v") 'multi-term)
(global-set-key (kbd "C-c y") 'multi-term-dedicated-toggle)
(global-set-key (kbd "C-c C-y") 'multi-term-dedicated-toggle)
(global-set-key (kbd "s-Y") 'multi-term-dedicated-toggle)
(add-hook 'term-mode-hook (lambda ()
			    (setq-local scroll-margin 0)
			    (setq-local hscroll-margin 0)
			    (setq multi-term-scroll-to-bottom-on-output t)
			    (setq multi-term-scroll-show-maximum-output t)
			    (setq-local global-hl-line-mode nil)))
(defun term-send-undo () ;; undo for multi-term
  (interactive)
  (term-send-raw-string "\C-_"))
;; keybinding for multi-term.
(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)            ; default
        ("C-c C-e" . term-send-esc)                    ; default
        ("C-c C-j" . term-line-mode)
        ("C-c C-k" . term-char-mode)
	("C-l"     . comint-clear-buffer)
        ("C-b"     . term-send-left)
        ("C-f"     . term-send-right)
        ("C-m"     . term-send-return)                 ; default
        ("C-y"     . term-paste)                       ; default
        ("M-f"     . term-send-forward-word)           ; default
        ("M-b"     . term-send-backward-word)          ; default
        ("M-o"     . term-send-backspace)              ; default
        ("M-p"     . term-send-up)                     ; default
        ("M-n"     . term-send-down)                   ; default
        ("M-M"     . term-send-forward-kill-word)   ; default
        ("M-d"     . term-send-forward-kill-word)
        ("M-N"     . term-send-backward-kill-word)  ; default
        ("M-DEL"   . term-send-backward-kill-word)
        ("M-r"     . term-send-reverse-search-history) ; default
        ("M-,"     . term-send-raw)                    ; default
        ("M-."     . comint-dynamic-complete)
	("s-z"     . term-send-undo)
	("C--"     . term-send-undo)
	("M-o"     . ace-window)
	("<s-backspace>" . term-send-raw)))

;; Exec-path-from-shell
;; --------------------
;; Get path env using shell.
;;

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; wgrep
;; -----
;; edit grep buffer
(use-package wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

;; ripgrep
;; --
;; ripgrep
(use-package ripgrep)

;; Undo-tree
;; ---------
;;
(use-package undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
;; (setq undo-tree-auto-save-history t)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'redo)

;; minions
;; -------
;; make modeline more simpler
(use-package minions)
(minions-mode)

;; beacon
;; ------
;; make it easier to find cursor
(use-package beacon)
(if (display-graphic-p)
    (progn
      (beacon-mode 1)
      (setq beacon-color "#f2777a"))) ;; red 1 color

;; flycheck
;; --------
;; syntax check
(use-package flycheck)
(add-hook 'python-mode-hook #'flycheck-mode)

;; yasnippet
;; ---------
;; template system
(use-package yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)
(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)

;; company-mode
;; ------------
;; auto complete
;; (use-package compnay)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-show-numbers t)
;; (setq company-idle-delay 0)
;; (defun try-flyspell (arg) ;; check spelling for only comment
;;   (if (nth 4 (syntax-ppss))
;;       (call-interactively 'ispell-word)
;;   nil))
;; (setq hippie-expand-try-functions-list ;; auto complte sequence 1. try-flyspell (comment only) 2. yasnippet 3. dabbrev 4. company
;;       '(try-flyspell
;;         yas-hippie-try-expand
;;         try-expand-dabbrev-visible
;;         (lambda (arg) (call-interactively 'company-complete))
;;         ))
;; (global-set-key (kbd "<escape>") 'hippie-expand)
;; (global-set-key (kbd "M-/") 'hippie-expand)

;; company-jedi
;; ------------
;; backend for company-mode
;; (use-package compnay-jedi)
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; iedit
;; -----
;; edit equal text at same time
(use-package iedit)

;; prodigy
;; -------
;; process manager
(use-package prodigy)
;; (global-set-key (kbd "C-c r") 'prodigy)
;; (global-set-key (kbd "C-c C-r") 'prodigy)
(if (file-exists-p "~/.emacs.d/packages/prodigy-conf.el")
    (use-package prodigy-conf))

;; realgud
;; -------
;; debugger
(use-package realgud)
(setq realgud:pdb-command-name "python -m pdb")
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "<f8>") #'realgud:ipdb)))
(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)

;; dumb-jump
;; ---------
;; go to definition
(use-package dumb-jump)
(dumb-jump-mode)

(setq dumb-jump-selector 'ivy)
(define-key dumb-jump-mode-map (kbd "C-M-p") nil)
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "C-c C-j") 'dumb-jump-go)
(global-set-key (kbd "C-c k") 'dumb-jump-back)
(global-set-key (kbd "C-c C-k") 'dumb-jump-back)

;; git gutter
;; ---------
;; To see git diff
(use-package git-gutter)
(global-git-gutter-mode +1)
(setq git-gutter:window-width 1)
(setq git-gutter:added-sign "|"
      git-gutter:modified-sign "|"
      git-gutter:deleted-sign "|")

;; avy
;; ---
;;
(use-package avy)
(avy-setup-default)
(setq avy-all-windows nil)
(global-set-key (kbd "C-c SPC") 'avy-goto-line)

;; hydra
;; -----
;;
(require 'my-hydra)
;; (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
;; (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
;; (define-key dired-mode-map "." 'hydra-dired/body)
;; (add-hook 'dired-initial-position-hook #'hydra-dired/body)
;; (global-set-key (kbd "M-g") 'hydra-avy/body)
(global-set-key (kbd "C-M-o") 'hydra-window/body)

;; livedown
;; --------
;; markdown preview
(add-to-list 'load-path "~/.emacs.d/packages/emacs-livedown")
(require 'livedown)

;; ;; ===========================================================================
;; ;; Unknown
;; ;; ===========================================================================
