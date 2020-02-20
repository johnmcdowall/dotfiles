;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
                          '(ns-appearance . dark))

;(menu-bar-mode -1)
;(tool-bar-mode -1)
(toggle-scroll-bar -1)
;;
;; platform
;;

;; Define some platform-specific variables.
(setq is-terminal (equal window-system nil))
(setq is-gui (equal window-system 'ns))
(setq is-xserve (equal window-system 'x))
(setq is-osx (equal system-type 'darwin))
(setq is-wsl (string-match "[Mm]icrosoft" operating-system-release))
(setq is-gnu (memq system-type '(gnu gnu/linux gnu/kfreebsd)))



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
(setq doom-font (font-spec :family "Delugia Nerd Font" :size 16))

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

;;;
;; copy / paste
;; JMD - Lifted from https://github.com/wusticality/emacs/blob/master/.emacs.d/init.el
;;

;; Copy / paste on mac.
(when (and is-osx is-terminal)
  ;; Copy from the clipboard.
  (defun mac-copy ()
    (shell-command-to-string "pbpaste"))

  ;; Paste from the clipboard.
  (defun mac-paste (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; Install the commands.
  (setq interprogram-paste-function 'mac-copy)
  (setq interprogram-cut-function 'mac-paste))

;; Copy / paste on wsl.
;;
;; For this I had help from the following sources:
;;
;; https://github.com/redguardtoo/emacs.d
;; http://blog.binchen.org/posts/copypaste-in-emacs.html
;;
;; For things to work on wsl, you need to have "clip.exe"
;; and Powershell ("powershell.exe") in your path. For Powershell,
;; you also need to have the following extension installed:
;;
;; https://github.com/mklement0/ClipboardText

(when (or is-wsl is-terminal is-xserve)
  (defun my-clipboard-set (value)
    "Sets the clipboard value."
    (let ((program (executable-find "clip.exe")))
      (with-temp-buffer
        (insert value)
        (cond
         (program
          (call-process-region (point-min) (point-max) program))
         (t
          (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input"))))))

  (defun my-clipboard-get ()
    "Gets the clipboard value."
    (let ((program (executable-find "powershell.exe")))
      (cond
       (program
        (substring
         (with-output-to-string
           (with-current-buffer standard-output
             (let ((coding-system-for-read 'dos))
               (call-process program nil t nil "-command" "Get-Clipboard"))))
         0 -1))
       (t
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "xsel" nil t nil "--clipboard" "--output")))))))

  (defun my-clipboard-copy (start end)
    "Copy to the clipboard."
    (interactive "r")
    (my-clipboard-set (buffer-substring-no-properties start end)))

  (defun my-clipboard-paste ()
    "Paste from the clipboard."
    (interactive)
    (insert (my-clipboard-get)))

  ;; Keybindings for clipboard copy / paste.
  (global-set-key (kbd "C-c C-c") 'my-clipboard-copy)
  (global-set-key (kbd "C-c C-v") 'my-clipboard-paste))

;; Set initial frame size and position
(defun my/set-initial-frame ()
  (let* ((base-factor 0.70)
	(a-width (* (display-pixel-width) base-factor))
        (a-height (* (display-pixel-height) base-factor))
        (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
	(a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
(setq frame-resize-pixelwise t)
(my/set-initial-frame)
