;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :name "Fira Code" :size 13 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq confirm-kill-emacs nil)

(setq evil-want-fine-undo t)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Remove the icons on the dashboard
;;(dolist (section +doom-dashboard-menu-sections)
;;  (let ((section-plist (cdr section)))
;;    (plist-put section-plist :icon nil)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(defun open-wt-here ()
  "Open Windows Terminal in current directory."
  (interactive)
  (call-process "wt" nil 0 nil "-d" (windows-path (expand-file-name default-directory))))

;; Passes a command of the form `cmd "/c wt -d C:\path\to\dir"' to `powershell start'
;; -Verb runAs lets wt run with administrator with privileges
;; It's unclear why powershell has to ask cmd to open wt; asking powershell to open
;; wt directly doesn't seem to work for some reason
(defun open-admin-wt-here ()
  "Open Windows Terminal with administrator privileges in current directory."
  (interactive)
  (call-process
   "powershell"
   nil
   0
   nil
   "start"
   "cmd"
   (concat "\"/c wt -d " (windows-path (expand-file-name default-directory)) "\"")
   "-Verb"
   "runAs"))

(defun windows-path (str)
  (replace-regexp-in-string "\/" "\\\\" str))
