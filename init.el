;;; init.el --- Jay Lee's init.el File For GNU Emacs

;; ===========================================================================
;; Cask
;; ===========================================================================

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; ===========================================================================
;; Package
;; ===========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(when (< emacs-major-version 24) (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; ===========================================================================
;; Simple Settings
;; ===========================================================================

;; Appearance
;; ----------
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (window-system)
    (scroll-bar-mode -1)) ; don't use scrollbar in only gui
(tooltip-mode -1)
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%a %I:%M %p")
(setq frame-title-format nil)
(display-time)				; Display time
(column-number-mode t)			; show column number
(show-paren-mode t)                     ; Show parenthesis match
(transient-mark-mode t)			; Highlight region
(which-function-mode)
(setq which-func-unknown "?")
(set-face-attribute 'which-func nil :box '(:color "#000000") :bold t :foreground "#c98459")
(setq inhibit-startup-message t)	; Inhibit startup message
;; font
(global-font-lock-mode t)		; Enable syntax highlight
(set-face-bold 'bold nil)
;;(set-fontset-font t 'hangul (font-spec :name "NanumGothic")
(set-frame-font "menlo 18" nil t)
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
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

;; Dired
;; -----
;;
(add-to-list 'load-path "~/.emacs.d/packages/")
(require 'dired+) ;; powerful dired
;; show details in dired+.
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(treemacs-icons-dired-mode) ;; dired png icons
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto revert in dired
;; hidden dotfiles
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;;
(setq dired-dwim-target t) ;; convenient manipulating for files
(setq dired-omit-verbose nil) ;; hidden annoying message about omit
;; sort directory first in dired
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

;; Global Keybindings
;; ------------------
;;
;; Unset Keybindings
(global-unset-key (kbd "C-x C-t"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-L"))
(global-unset-key (kbd "C-;"))
;; Set Keybindings
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
(global-set-key (kbd "C-c o") 'dired-omit-mode)
(global-set-key (kbd "\r") 'newline-and-indent) ; auto indentation
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "<C-S-tab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "C-x C-g") 'revert-buffer)
(global-set-key (kbd "<f5>") 'transpose-frame)
(global-set-key (kbd "<f6>") 'transpose-windows)
(global-set-key (kbd "s-t") 'eyebrowse-create-window-config)

;; Encoding
;; --------
;;
(set-language-environment 'utf-8)
;; korean input setting
(setq default-input-method "korean-hangul")
(setq default-korean-keyboard "2")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;; encoding for mac os file system
(if (eq system-type 'darwin)
    (progn
      (require 'ucs-normalize)
      (set-file-name-coding-system 'utf-8-hfs) ;; UTF-8 based coding system for macOS HFS file names.
      (setq locale-coding-system 'utf-8-hfs)
      (prefer-coding-system 'utf-8-unix)
      (set-terminal-coding-system 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8-unix)
      (set-selection-coding-system 'utf-8-unix)
      (prefer-coding-system 'utf-8-unix)
      (set-terminal-coding-system 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8-unix)
      (set-selection-coding-system 'utf-8-unix)
      (set-buffer-file-coding-system 'utf-8-unix)
      (set-clipboard-coding-system 'utf-8-unix)
      ))
;; (defun activate-default-input-method ()
;;   (interactive)
;;   (activate-input-method default-input-method))
;;(add-hook 'org-mode-hook 'activate-default-input-method)

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
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(setq mac-allow-anti-aliasing t)
;; keybinding
(when (and (eq system-type 'darwin) window-system) ;; check if mac os and gui
  (setq ns-use-native-fullscreen nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  ;; mac style keybinding
  (global-set-key (kbd "<C-s-268632090>") 'toggle-frame-maximized)
  (global-set-key (kbd "<M-return>") 'toggle-frame-maximized)
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "s-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "s-M-h") 'ns-do-hide-others)
  (global-set-key (kbd "s-r") 'revert-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "S-s-w") 'delete-frame)
  (global-set-key (kbd "<s-backspace>") '(lambda () (interactive) (kill-line 0)))
  (global-set-key (kbd "s-w") 'eyebrowse-close-window-config)
  (global-set-key (kbd "s-}") 'eyebrowse-next-window-config)
  (global-set-key (kbd "s-{") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "s-]") 'next-buffer)
  (global-set-key (kbd "s-`") 'other-frame)
  (global-set-key (kbd "s-~") '(lambda () (interactive) (other-frame -1)))
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-L") 'treemacs)
  (global-set-key (kbd "s-0") 'treemacs)
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; set defulat frame to maximized.
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

;; scroll setup
;; ------------
;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)
(setq scroll-conservatively 200)
(setq scroll-margin 3)

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
	    desktop-load-locked-desktop nil)
            (desktop-save-mode 1)))

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
(setq recentf-max-menu-items 10)
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

;; ===========================================================================
;; Code For Development
;; ===========================================================================

;; settings
(setq tab-width 2) ;; set tab width
(setq-default truncate-lines t)
(setq-default line-spacing 2)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; swift
;; -----
;;
(require 'swift-mode)

;; go
;; --
;;
(require 'go-mode)
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 2)
	    (setq standard-indent 2)
	    (setq indent-tabs-mode nil)))

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
(add-hook 'python-mode-hook '(lambda ()
			       (setq python-indent 4)))

;; javascript
;; ----------
;;
(setq js-indent-level 2)

;; css
;; ---
;;
(setq css-indent-offset 2)

;; html
;; ----
;;
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; markdown
;; --------
;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml
;; ----
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; org
;; ---
;;
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "BACKLOG" "READY" "PROGRESS" "|" "DONE")
        ))
(setq org-log-done t) ;; log timestamps when todo is done.
;; (setq org-todo-keyword-faces
;;       '(("BACKLOG" . (:foreground "#E5C07B" :weight bold))
;; 	("READY" . (:foreground "#C678DD" :weight bold))
;; 	("PROGRESS" . (:foreground "#E06C75" :weight bold))
;; 	("DONE" . (:foreground "#98C379" :weight bold))))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'git-auto-commit-mode)

;; ============================================================================
;; External Packages
;; ============================================================================

;; magit
;; -----
;;
(require 'magit)
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
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c p K") 'projectile-remove-known-project)
;; (setq projectile-switch-project-action #'projectile-dired)

;; eyebrowse
;; ---------
;;
(eyebrowse-mode t)
(set-face-attribute 'eyebrowse-mode-line-active nil :box '(:color "#000000") :bold t :foreground "#c98459") ;; #000 is black2 #c98 is orange1 in morning star
(global-set-key (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "s-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "s-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "s-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "s-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "s-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "s-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "s-9") 'eyebrowse-switch-to-window-config-9)

;; ibuffer-vc
;; ----------
;;
(require 'ibuffer-vc)
(add-hook 'ibuffer-hook (lambda ()
			  (ibuffer-vc-set-filter-groups-by-vc-root)
			  (ibuffer-do-sort-by-recency)))

;; ace-window
;; ----------
;;
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; ivy & swiper & counsel
;; ----------------------
;;
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c C-s") 'swiper)
(global-set-key (kbd "C-c C-b") 'swiper-all)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(counsel-projectile-mode 1)

;; which-key
;; ---------
;;
(which-key-mode)

;; multi-term
;; ----------
;;
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "C-c C-t") 'multi-term)
(global-set-key (kbd "C-c C-y") 'multi-term-dedicated-toggle)
(global-set-key (kbd "s-Y") 'multi-term-dedicated-toggle)
(defun term-send-undo () ;; undo for multi-term
  (interactive)
  (term-send-raw-string "\C-_"))
(setq term-bind-key-alist
          '(("C-c C-c" . term-interrupt-subjob)            ; default
            ("C-c C-e" . term-send-esc)                    ; default
            ("C-c C-j" . term-line-mode)
            ("C-c C-k" . term-char-mode)
            ("C-a"     . term-bol)
            ("C-b"     . term-send-left)
            ("C-f"     . term-send-right)
            ("C-p"     . previous-line)                    ; default
            ("C-n"     . next-line)                        ; default
            ("C-s"     . isearch-forward)                  ; default
            ("C-r"     . isearch-backward)                 ; default
            ("C-m"     . term-send-return)                 ; default
            ("C-y"     . term-paste)                       ; default
            ("M-f"     . term-send-forward-word)           ; default
            ("M-b"     . term-send-backward-word)          ; default
            ("M-o"     . term-send-backspace)              ; default
            ("M-p"     . term-send-up)                     ; default
            ("M-n"     . term-send-down)                   ; default
            ;; ("M-M"     . term-send-forward-kill-word)   ; default
            ("M-d"     . term-send-forward-kill-word)
            ;; ("M-N"     . term-send-backward-kill-word)  ; default
            ("M-DEL"   . term-send-backward-kill-word)
            ("M-r"     . term-send-reverse-search-history) ; default
            ("M-,"     . term-send-raw)                    ; default
            ("M-."     . comint-dynamic-complete)
	    ("s-z"     . term-send-undo)
	    ("C--"     . term-send-undo)
	    ("M-o"     . ace-windowb)
	    ("<s-backspace>" . term-send-raw)))

;; treemacs
;; --------
;;
(setq treemacs-is-never-other-window t)
(setq treemacs-show-hidden-files nil)
(global-set-key (kbd "C-x t") 'treemacs-select-window)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

;; Exec-path-from-shell
;; --------------------
;; Get path env using shell.
;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Undo-tree
;; ---------
;;
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(setq undo-tree-auto-save-history t)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'redo)

;; minions
;; -------
;; make modeline more simpler
(minions-mode)

;; beacon
;; ------
;; make it easier to find cursor
(if (display-graphic-p)
    (progn
      (beacon-mode 1)
      (setq beacon-color "#f2777a"))) ;; red 1 color

;; flycheck
;; --------
;; syntax check
(add-hook 'python-mode-hook #'flycheck-mode)

;; git-auto-commit-mode
;; --------------------
;; commit and push automatically everytime a file is saved.
(setq gac-automatically-push-p t) ;; settting about automatic push
(setq gac-ask-for-summary-p nil) ;; if t, always ask commit message everytime a file is saved.

;; yasnippet
;; ---------
;; template system
(yas-global-mode 1)
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

;; company-mode
;; ------------
;; auto complete
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(defun try-flyspell (arg) ;; check spelling for only comment
  (if (nth 4 (syntax-ppss))
      (call-interactively 'ispell-word)
  nil))
(setq hippie-expand-try-functions-list ;; auto complte sequence 1. try-flyspell (comment only) 2. yasnippet 3. dabbrev 4. company
      '(try-flyspell
        yas-hippie-try-expand
        try-expand-dabbrev-visible
        (lambda (arg) (call-interactively 'company-complete))
        ))
(global-set-key (kbd "<escape>") 'hippie-expand)
(global-set-key (kbd "M-/") 'hippie-expand)

;; company-jedi
;; ------------
;; backend for company-mode
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; prodigy
;; -------
;; process manager
(prodigy-define-service
  :name "Django"
  :command "python"
  :args '("-m" "django" "runserver" "8888")
  :cwd "~/Desktop/hanjaelee/"
  :tags '(work)
  :env '(("DJANGO_SETTINGS_MODULE" "hanjaelee.settings"))
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

;; ===========================================================================
;; Unknown
;; ===========================================================================
