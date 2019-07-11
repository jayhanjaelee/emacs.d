(provide 'my-hydra)
;;   "
;;  ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
;; -^----------^-+-^----^--------+-^-------^--------+-^----^-------
;;   _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
;;  _h_ _l_      | _u_: unmark   | _S_: save        | _s_: sort
;;   _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
;; -^----------^-+-^----^--------+-^-------^--------+-^----^-------
;; "

;; hydra-ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
 "
^ ^ ^ ^ ^ ^ | ^Mark^        | ^Actions^        | ^View^
^-^-^-^-^-^-+-^----^--------+-^-------^--------+-^----^-------
^ ^ _k_ ^ ^ | _m_: mark     | _D_: delete      | _g_: refresh
_h_ ^+^ _l_ | _u_: unmark   | _S_: save        | _s_: sort
^ ^ _j_ ^ ^ | _*_: specific | _a_: all actions | _/_: filter
^ ^ ^ ^ ^ ^ | _%_: regexp   | ^       ^        |
^-^-^-^-^-^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
	("h" ibuffer-backward-filter-group)
	("l" ibuffer-forward-filter-group)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
	("%" hydra-ibuffer-regexp/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-regexp (:color teal :columns 5
																			 :after-exit (hydra-ibuffer-main/body))
	"Regexp"
  ("n" ibuffer-mark-by-name-regexp "name")
	("m" ibuffer-mark-by-mode-regexp "mode")
	("f" ibuffer-mark-by-file-name-regexp "filename")
	("g" ibuffer-mark-by-content-regexp "content")
	("L" ibuffer-mark-by-locked "lock")
	("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

;; hydra-dired
(defhydra hydra-dired (:hint nil :color pink)
  "
^ ^ ^ ^ ^ ^ | ^Mark^         | ^Actions^        | ^View^
^-^-^-^-^-^-+-^----^---------+-^-------^--------+-^----^-------
^ ^ _k_ ^ ^ | _m_: mark      | _F_: find marked | _g_: refresh
_h_ ^+^ _l_ |	_u_: unmark		 | _D_:	delete			| _s_: sort
^ ^ _j_ ^ ^ |	_U_: unmark All| _v_: view				| _?_: summary
^-^-^-^-^-^-+-^----^---------+------------------+--------------
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ;; ("l" dired-do-redisplay)   ;; relist the marked or singel directory
	("h" dired-prev-dirline)
	("j" dired-next-line)
	("k" dired-previous-line)
	("l" dired-next-dirline)
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window "otehr window" :color blue)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" quit-window "quit" :color blue)
  ("." nil :color blue))

;; hydra-avy
(defhydra hydra-avy (:columns 4 :exit t)
	"avy"
	("g" goto-line "goto-line")
	("j" avy-goto-line-below "goto-line-below")
  ("k" avy-goto-line-above "goto-line-above")
	("l" avy-goto-line "goto-line")
	("m" avy-resume "resume")
	("q" nil "quit"))

;;* Helpers
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; hydra-window
(defhydra hydra-window (:color amaranth)
  "window"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
	("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horz")
  ("t" transpose-frame "tr")
  ("o" delete-other-windows "one" :color blue)
  ("a" ace-window "ace")
  ("c" ace-swap-window "swap")
  ("d" ace-delete-window "del")
  ("i" ace-maximize-window "ace-one" :color blue)
  ("b" ivy-switch-buffer "buf")
  ("m" bookmark-jump "bmk")
  ("q" nil "cancel"))
