;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC EMACS CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doom-theme 'doom-gruvbox)

(setq display-line-numbers-type t)

(display-time-mode 1)

(setq blink-cursor-blinks -1) ; Keep cursor blinking
(blink-cursor-mode 1)

(menu-bar-mode 1) ; Invaluable learning tool, if a little ugly

;; An integral `:size' is interpreted as pixels. Floating-point numbers are
;; interpreted as points.
(setq doom-font (font-spec :name "SauceCodePro NF" :size 10.0 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13))

;; (setq fancy-splash-image (concat doom-user-dir "cacomon.png"))

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq confirm-kill-emacs nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; DIRED
;; Present a cleaner directory view by default
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;;;; EVIL

;; TODO Use win32yank.exe if needed

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


;;;;; SPELL (DOOM MODULE)
;; TODO Don't have spellchecking toggled on by default when using the :spell module
;; (after! spell-fu
;;   (remove-hook! '(text-mode yaml-mode conf-mode prog-mode) #'spell-fu-mode))


;;;;;; SPELL
(setq ispell-program-name "C:\\msys64\\mingw64\\bin\\hunspell.exe")
;; (setq ispell-dictionary "en_US")


;;;;; OLIVETTI

;; Why does after! olivetti work, but after! olivetti-mode seemingly doesn't?
(after! olivetti
  (setq olivetti-body-width 130))


;;;;; SHELL
(setq explicit-shell-file-name "c:\\Program Files\\PowerShell\\7\\pwsh.exe")


;;;;; LSP

;; When there is only one available code action, by default
;; `lsp-execute-code-action' will automatically execute the action without
;; asking for confirmation first, this is problematic since often I just want to
;; see what code actions are available on a line, without necessarily executing
;; them.
(after! lsp-mode
  (setq lsp-auto-execute-action nil))

;;;;; EMMET

(defun emmet-insert-at-next-edit-point (count)
  (interactive "^p")
  (emmet-next-edit-point count)
  (evil-insert 1))

(defun emmet-insert-at-prev-edit-point (count)
  (interactive "^p")
  (emmet-prev-edit-point count)
  (evil-insert 1))


;;;;; COPILOT
(use-package! copilot
  :defer t
  :hook (prog-mode . copilot-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See https://discourse.doomemacs.org/t/how-to-re-bind-keys/

;; Set M-j, M-k to mirror M-DownArrow M-UpArrow
;; Overrides default-indent-new-line and kill-sentence
(map! "M-j" #'drag-stuff-down
      "M-k" #'drag-stuff-up)

;; DEL deletes a character in normal mode. So should backspace then.
(map! :map evil-normal-state-map
      "<backspace>" #'evil-delete-backward-char)

(map! :leader :desc "Center text" "t C" #'olivetti-mode)

(map! :leader :desc "Menu bar" "t M" #'menu-bar-mode)

;; Choice of keybinds is definitely TBF
;; Currently breaks org-mode links for some reason (and possibly more)
(map! "M-i" #'er/contract-region
      "M-o" #'er/expand-region)

(map! :after emmet-mode
      :map emmet-mode-keymap
      :vieomrg "C-M-<right>" #'emmet-next-edit-point
      :vieomrg "C-M-<left>"  #'emmet-prev-edit-point
      :n       "C-M-<right>" #'emmet-insert-at-next-edit-point
      :n       "C-M-<left>"  #'emmet-insert-at-prev-edit-point)

(map! :after copilot
      :map copilot-completion-map
      "<right>"   #'copilot-accept-completion
      "C-f"       #'copilot-accept-completion
      "C-<right>" #'copilot-accept-completion-by-word
      "M-f"       #'copilot-accept-completion-by-word
      "C-e"       #'copilot-accept-completion-by-line
      "<end>"     #'copilot-accept-completion-by-line)
