;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(if (eq system-type 'gnu/linux)
    (setq default-directory "/mnt/c/Users/HAIER"))

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

;; An integral `:size' is interpreted as pixels. Floating-point numbers are
;; interpreted as points.

(setq fira-spec (font-spec :name "Fira Code" :size 13 :weight 'semi-light))

(setq jetbrains-spec (font-spec :name "JetBrains Mono" :size 13 :weight 'light))

(setq source-spec (font-spec :name "Source Code Pro" :size 10.2 :weight 'regular))

(setq doom-font source-spec
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

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq confirm-kill-emacs nil)

(setq evil-want-fine-undo t)

(setq evil-snipe-scope 'buffer)

(setq evil-cross-lines t)

(after! org
  (plist-put org-format-latex-options :scale 1.3))

;; This overrides org-mode's `+org/shift-return'
(defun open-below-without-insert (count)
  "Insert a new line below point without switching to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-open-below count)
  (evil-normal-state)
  (message ""))

(map! :map evil-normal-state-map "<S-return>" #'open-below-without-insert)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Remove the icons on the dashboard
;;(dolist (section +doom-dashboard-menu-sections)
;;  (let ((section-plist (cdr section)))
;;    (plist-put section-plist :icon nil)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(beacon-mode 1)
(setq beacon-blink-delay 0.3
      beacon-blink-duration 0.2)

;; (setq langtool-language-tool-server-jar
;;       "C:\ProgramData\chocolatey\lib\languagetool\tools\LanguageTool-5.1\languagetool-server.jar")

(after! ox-latex
  (add-to-list
   'org-latex-classes
   `("deeparticle"
     ,(concat
      "\\documentclass[11pt]{article}\n"
      "\\usepackage{enumitem}\n"
      "\\newenvironment{deepsection}[1]{\\begin{enumerate}[label={}] \\item \\textbf{#1} \\newline}{\\end{enumerate}}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}")
     ("\\begin{deepsection}{%s}" "\\end{deepsection}" "\\begin{deepsection}{%s}" "\\end{deepsection}"))))

(defun open-wt-here ()
  "Open Windows Terminal in current directory."
  (interactive)
  (call-process "cmd" nil 0 nil "/c" "wt" "-d" (expand-file-name default-directory)))

;; Passes a command of the form `cmd "/c wt -d C:\path\to\dir"' to `powershell start'
;; -Verb runAs lets wt run with administrator privileges
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
   (concat "\"/c wt -d " (expand-file-name default-directory) "\"")
   "-Verb"
   "runAs"))

(map!
 :leader
 "ot" #'open-wt-here
 "oT" #'open-admin-wt-here)

;; Prevents evil delete commands from copying text in addition to removing it.
;; It does this by making each delete command copy text to the "black hole"
;; register (_), which leaves the system clipboard and other registers untouched.
;; Use the " register explicitly (e.g. ""diw) if the old behaviour is desired.
(defadvice! my-evil-delete-default-to-black-hole-a
  (fn beg end &optional type register yank-handler)
  "Advise `evil-delete' to set default REGISTER to the black hole register."
  :around #'evil-delete
  (unless register (setq register ?_))
  (funcall fn beg end type register yank-handler))
