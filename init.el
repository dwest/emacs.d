;; init.el -- emacs configuration file

;; Author: Daniel West <westwolf24@gmail.com>
;; Version: 0x003
;; Tags: emacs, config

;; General Settings

; Load path as understood by bash
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

; Disable the splash screen and area message it's really annoying
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "daniel")

; Don't assume the scratch buffer is an emacs lisp file please
(setq initial-major-mode 'fundamental-mode)

; Disable "id10t modes"
(put 'set-goal-column 'disabled nil)

; Path settings

; Skip the default init, prevent the site install from doing "dumb things"
(setq inhibit-default-init t)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

; Backup files
;     Keep backup files out of the normal file system tree!  Backup files are really annoying (except when you absolutely need them)!  Note that unlike the wiki article (http://emacswiki.org/emacs/AutoSave) suggests we do not store backup files in the system /tmp directory for security reasons...
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/backup/")))

; Don't ask about trailing eol, just put one there!
(setq next-line-add-newlines nil)
(setq require-final-newline t)

;; Mode settings

; Major modes

; Use fundamental mode for everything, unless I say otherwise.  Stop trying to be helpful!
(setq auto-mode-alist
      '(("\\.el$" . lisp-mode)
        ("\\.js$" . js2-mode)
        ("\\.coffee$" . coffee-mode)
        ("\\.html$" . sgml-mode)
        ("\\.py$" . python-mode)
        ("\\.org$" . org-mode)
        ("\\.css$" . css-mode)
        ("\\.php$" . php-mode)
        ("*" 'fundamental-mode)))

; Minor modes

; Tab settings
(setq-default indent-tabs-mode nil)     ;; don't put tab characters in my files
(setq-default tab-width 4)              ;; tabs inserted by others are shown as 4 spaces long
(setq-default c-basic-offset 4)         ;; 4 spaces to indent in c based languages
(setq indent-line-function 'insert-tab) ;; insert tab... oops spaces (awful... just awful)

; Disable all vc-* modes.  They are awful.
(setq vc-handled-backends nil)

;; Display Settings

; Always show line numbers, if I didn't want them I'd use less instead of emacs...
(global-linum-mode t)

; Column numbers too
(setq column-number-mode t)

; Theme
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'solarized-theme t)

; Minibuffer enhancement
;(require 'icicles)
;(icy-mode 1)

;; Key Bindings

; Logical pair for C-x o -- C-x p means go to previous window
(global-set-key
 (kbd "C-x p") 
    (lambda()
     (interactive) (other-window -1)))

(global-set-key (kbd "C-z") 'eshell)

; show recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; package management

; Get a copy of the latest clojure packages
(require 'package)

; package repositories
(mapcar (lambda (repo) 
          (add-to-list 'package-archives repo 'APPEND))

      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; Python Stuff
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--profile=dev")

;; clojure stuff

; function argument preview
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
; hide special buffers (space toggles in buff-list)
(setq nrepl-hide-special-buffers t)
; tab is indent!
(setq nrepl-tab-command 'indent-for-tab-command)
; only show stack traces in repl
(setq nrepl-popup-stacktraces nil)
; only popup error buffer in repl... apparently a separate option
(setq nrepl-popup-stacktraces-in-repl t)
; handy if running multiple projects at once
(setq nrepl-popup-stacktraces-in-repl t)
; bring nrepl buffer to current window with "C-c C-z"
(add-to-list 'same-window-buffer-names "*nrepl*")

;; nodejs stuff

;; auto-complete mode
(require 'auto-complete-config)
(ac-config-default)

;; Machine-generated cruft
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("12c51dc1e3c1cd5d6c4df302e1d0e07126b22e44c777f5a60189073697fa5b1d" "4cd7eda69f59b3cc97c8a561ac809d82ce6e39b8d0b78aaad8eb6ab58a546d97" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

