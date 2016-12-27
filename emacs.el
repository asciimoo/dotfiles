;;; .emacs --- CONFIG
(require 'cl)
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar required-packages
  '(
    evil
    go-mode

    helm
    helm-projectile

    molokai-theme
    relative-line-numbers
    smooth-scrolling
    spaceline-config
    yasnippet
   )
)

;define package installer
(defun install-packages ()
  (interactive)
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;load required packages
(loop for p in required-packages
  do (require p))

; UI
(load-theme 'molokai t)
(set-background-color "black")
(setq-default show-trailing-whitespace t)
(set-face-attribute 'default nil :height 80)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode t)
(scroll-bar-mode -1)
(global-relative-line-numbers-mode)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(smooth-scrolling-mode 1)
(setq scroll-preserve-screen-position t)

; CUSTOMISATIONS
;use 4 spaces instead of tabs
(setq-default tab-width 4 indent-tabs-mode nil)
;don't create backup files
(setq make-backup-files nil)
;copy/paste
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(setq x-select-enable-primary t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

; KEY BINDINGS
(global-set-key [f8] 'global-relative-line-numbers-mode)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

; PACKAGE SPECIFIC SETTINGS
; EVIL MODE
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

; FLYCHECK
;(global-flycheck-mode)

; SPACELINE
(spaceline-spacemacs-theme)

; YASNIPPET
(yas-global-mode 1)

; HELM
(require 'helm-config)
(setq helm-split-window-in-side-p           nil ; Open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                                        ;      helm-echo-input-in-header-line        t
      helm-display-header-line              nil
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      fit-window-to-buffer-horizontally     1
      helm-recentf-fuzzy-match              t)

(setq helm-autoresize-max-height 50)
(setq helm-autoresize-min-height 20)
(set-face-attribute 'helm-source-header nil :height 0.1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-o") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(helm-autoresize-mode 1)
(helm-projectile-on)
(helm-mode 1)

; GO-MODE
(add-hook 'before-save-hook 'gofmt-before-save)


(provide '.emacs)
;;; .emacs ends here
