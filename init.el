;; init.el -- emacs configuration file

;; Author: Daniel West <westwolf24@gmail.com>
;; Version: 0x004
;; Tags: emacs, config

;;;; General Settings

;; Load path as understood by bash
(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Disable the splash screen and area message it's really annoying
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "daniel")

;; Disable the graphical menu bar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; enable time display
(display-time-mode 1)
(setq display-time-24hr-format 1)

;; Don't assume the scratch buffer is an emacs lisp file please
(setq initial-major-mode 'fundamental-mode)

;; Disable "id10t modes"
(put 'set-goal-column 'disabled nil)

;; Path settings

;; Skip the default init, prevent the site install from doing "dumb things"
(setq inhibit-default-init t)

;; custom lisp code
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; elpa lisp code
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; doc view settings
(setq doc-view-ghostscript-program "/usr/bin/gs")

;;;; Backup files

;; Keep backup files out of the normal file system tree!  Backup files
;; are really annoying (except when you absolutely need them)!  Note
;; that unlike the wiki article (http://emacswiki.org/emacs/AutoSave)
;; suggests we do not store backup files in the system /tmp directory
;; for security reasons...
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/backup/")))

;; Don't ask about trailing eol, just put one there!
(setq next-line-add-newlines nil)
(setq require-final-newline t)

;;;; Mode settings

;;;; Mode hooks

;; non-comprehensive list of mode hooks
(add-hook 'doc-view-mode-hook
          (lambda ()
            (linum-mode -1)))

;;;; Major modes

;; Use fundamental mode for everything, unless I say otherwise.  Stop
;; trying to be helpful!
(setq auto-mode-alist
      '(("\\.el$" . lisp-mode)
        ("\\.c$" . c-mode)
        ("\\.js$" . js2-mode)
        ("\\.coffee$" . coffee-mode)
        ("\\.html$" . sgml-mode)
        ("\\.py$" . python-mode)
        ("\\.go$" . go-mode)
        ("\\.org$" . org-mode)
        ("\\.css$" . css-mode)
        ("\\.php$" . php-mode)
        ("\\.sh$" . sh-mode)
        ("\\.boot" . clojure-mode)
        ("\\.clj\\(c\\|s\\)?$" . clojure-mode)
        ("^Makefile$" . makefile-mode)
        ("\\.clj" . clojure-mode)
        ("\\.pl" . perl-mode)
        ("\\.yml" . yaml-mode)
        ;; N.B. dov-view-mode requires xpdf, gs, etc.
        ("\\.p\\(s\\|df\\)$" . doc-view-mode)
        ("\\.tex$" . tex-mode)
        ("*" 'fundamental-mode)))

;; Minor modes
(show-paren-mode)

;;Tab settings
(setq-default indent-tabs-mode nil)     ;; don't put tab characters in my files
(setq-default tab-width 4)              ;; tabs inserted by others are shown as 4 spaces long
(setq-default c-basic-offset 4)         ;; 4 spaces to indent in c based languages
(setq indent-line-function 'insert-tab) ;; insert tab... oops spaces (awful... just awful)

;;Disable all vc-* modes.  They are awful.
(setq vc-handled-backends nil)

;; Display Settings

;;Always show line numbers, if I didn't want them I'd use less instead of emacs...
(global-linum-mode t)

;;Column numbers too
(setq column-number-mode t)

;;Minibuffer enhancement
;(require 'icicles)
;(icy-mode 1)

;;Theme
;(setq custom-theme-load-path '("~/emacs.d/themes/"))
;(load-theme sanityinc-solarized-dark t)

;; Key Bindings

;; speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)


;;Logical pair for C-x o -- C-x p means go to previous window
(global-set-key
 (kbd "C-x p") 
 (lambda()
   (interactive) (other-window -1)))

(global-set-key
 (kbd "C-x C-;")
 (lambda(beg end)
   (interactive "r") (comment-region beg end)))

(global-set-key
 (kbd "C-x C--")
 (lambda(beg end)
   (interactive "r") (uncomment-region beg end)))

(global-set-key
 (kbd "C-x _")
 (lambda(beg end)
   (interactive "r") (align-regexp beg end "\\(\\s-*\\)[\\s+-*/.]?=")))

(global-set-key (kbd "C-z") 'eshell)

(global-set-key
 (kbd "C-x <f9>")
 (lambda()
   (interactive)
   (let ((my-width (if (= tab-width 2) 4 2)))
     (setq tab-width my-width)
     (setq c-basic-offset my-width))))

;;show recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;package management

;;Get a copy of the latest clojure packages
(require 'package)

;;package repositories
(mapcar (lambda (repo) 
          (add-to-list 'package-archives repo 'APPEND))

      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; Python Stuff
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--profile=dev")

;; clojure stuff

;;function argument preview
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
;;hide special buffers (space toggles in buff-list)
(setq nrepl-hide-special-buffers t)
;;tab is indent!
(setq nrepl-tab-command 'indent-for-tab-command)
;;only show stack traces in repl
(setq nrepl-popup-stacktraces nil)
;;only popup error buffer in repl... apparently a separate option
(setq nrepl-popup-stacktraces-in-repl t)
;;handy if running multiple projects at once
(setq nrepl-popup-stacktraces-in-repl t)
;;bring nrepl buffer to current window with "C-c C-z"
(add-to-list 'same-window-buffer-names "*nrepl*")

;; nodejs stuff

;; auto-complete mode
;(require 'auto-complete-config)
;(ac-config-default)

;; company mode
;;(add-hook 'after-init-hook 'global-company-mode) ;slows down ssh

(when (display-graphic-p)
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ;;("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;;("\\(x\\)"                     #Xe16b)
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)
)
;; Machine-generated cruft
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "12c51dc1e3c1cd5d6c4df302e1d0e07126b22e44c777f5a60189073697fa5b1d" "4cd7eda69f59b3cc97c8a561ac809d82ce6e39b8d0b78aaad8eb6ab58a546d97" default)))
 '(tex-run-command "xelatex"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

