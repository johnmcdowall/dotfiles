;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
                          '(ns-appearance . dark))

(menu-bar-mode -1) 
(tool-bar-mode -1)
(toggle-scroll-bar -1)



;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John McDowall"
      user-mail-address "john@mcdowall.info")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 32))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-nord)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(defun wsl-copy (start end)
    (interactive "r")
      (shell-command-on-region start end "clip.exe"))

(global-set-key
   (kbd "C-c C-c")
    'wsl-copy)

(defun wsl-paste ()
    (interactive)
      (let ((wslbuffername "wsl-temp-buffer"))
      (get-buffer-create wslbuffername)
      (with-current-buffer wslbuffername
        (insert (let ((coding-system-for-read 'dos))
        (shell-command "powershell.exe -command 'Get-Clipboard' 2> /dev/null" wslbuffername nil))))
      (insert-buffer wslbuffername)
      (kill-buffer wslbuffername)))

(global-set-key
   (kbd "C-c C-v")
    'wsl-paste)
