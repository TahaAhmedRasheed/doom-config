;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC EMACS CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doom-theme 'doom-gruvbox)

(setq display-line-numbers-type t)

(display-time-mode 1)

;; An integral `:size' is interpreted as pixels. Floating-point numbers are
;; interpreted as points.
(setq doom-font (font-spec :name "Source Code Pro" :size 10.9 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq confirm-kill-emacs nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; DIRED
;; Present a cleaner directory view by default
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;;;; EVIL

;; TODO Use win32yank.exe ... somehow

;; Set d, c, x to use the black hole register by default
(defadvice! evil-delete-to-black-hole
  (fn beg end &optional type register yank-handler)
  "Advise `evil-delete' to set default REGISTER to the black hole register."
  :around #'evil-delete
  (unless register (setq register ?_))
  (funcall fn beg end type register yank-handler))

(defadvice! evil-change-to-black-hole
  (fn beg end &optional type register yank-handler delete-func)
  "Advise `evil-change' to set default REGISTER to the black hole register."
  :around #'evil-change
  (unless register (setq register ?_))
  (funcall fn beg end type register yank-handler delete-func))

(defadvice! evil-delete-char-to-black-hole
  (fn beg end &optional type register)
  "Advise `evil-delete-char' to set default REGISTER to the black hole register."
  :around #'evil-delete-char
  (unless register (setq register ?_))
  (funcall fn beg end type register))

;; Let h,j,k,l wrap through lines
(setq evil-cross-lines t)

;; Allow f,F,t,T to search beyond the current line
(setq evil-snipe-scope 'buffer)

;; More granular changes inside an insert mode session
(setq evil-want-fine-undo t)

;;;;; SPELL
;; TODO Don't have spellchecking toggled on by default



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set M-j, M-k to mirror M-DownArrown M-UpArrow
;; Overwrites default-indent-new-line and kill-sentence
(map! "M-j" #'drag-stuff-down
      "M-k" #'drag-stuff-up)
