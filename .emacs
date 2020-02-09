(setq user-full-name "Horia Ivan"
      user-mail-address "horia.ivan@bbc.co.uk")

(setq default-frame-alist '((width . 200) (height . 65)))

(desktop-save-mode 1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; GC tuning
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 30sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 30 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)
;; Set garbage collection to 20% of heap
(setq gc-cons-percentage 0.2)

;; Soft line/word wrap
(set-default 'truncate-lines t)
(setq-default word-wrap t)
(global-visual-line-mode t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-hl-line-mode +1)
(blink-cursor-mode -1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Hash key on Mac
(define-key key-translation-map (kbd "M-3") (kbd "#"))

(use-package grandshell-theme
  :ensure t
  :config
  (load-theme 'grandshell t))

(use-package smart-mode-line
  :config
  (add-hook 'after-init-hook 'sml/setup))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move point when splitting
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; TAB
(setq-default indent-tabs-mode nil) ;; disable tab indent
(setq-default tab-width 2)
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(set-frame-font "Menlo 12")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package diminish)

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package expand-region
  :bind ("M-m" . er/expand-region))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package avy
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

(use-package crux
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line))

(use-package magit
  :bind (("C-M-g" . magit-status)))

(use-package projectile
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1))

(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package helm
  :defer 2
  :diminish helm-mode
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click))

(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook #'fci-mode)
  (setq fci-rule-column 80))

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook #'aggressive-indent-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-parentheses
  :config
  (add-hook 'after-init-hook #'global-highlight-parentheses-mode))

(use-package highlight-symbol
  :bind
  (("C-<f2>" . highlight-symbol)
   ("<f2>" . highlight-symbol-next)
   ("S-<f2>" . highlight-symbol-prev)
   ("M-<f2>" . highlight-symbol-query-replace))
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package dockerfile-mode)

(use-package json-mode)

(use-package yaml-mode)

(use-package groovy-mode)

(use-package beacon
  :config
  (beacon-mode 1))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package god-mode
  :config
  (global-set-key (kbd "<escape>") 'god-local-mode))

(use-package windmove
  :ensure nil
  :commands (windmove-up crosshairs)
  :init
  (defmacro defwindmove (name fn)
    `(defun ,name ()
       (interactive)
       (ignore-errors
         (,fn)
         (crosshairs-flash))))
  (defwindmove windowmove-left windmove-left)
  (defwindmove windowmove-right windmove-right)
  (defwindmove windowmove-up windmove-up)
  (defwindmove windowmove-down windmove-down)
  :bind
  (("S-<left>"  .  windowmove-left)
   ("S-<right>" .  windowmove-right)
   ("S-<up>"    .  windowmove-up)
   ("S-<down>"  .  windowmove-down)))

(use-package dimmer
  :config
  (dimmer-activate))

(use-package color-identifiers-mode
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(use-package goto-line-preview
  :bind
  ("C-c g" . goto-line-preview))

(use-package smart-hungry-delete
  :bind
  (("<backspace>" . smart-hungry-delete-backward-char)
   ("C-d" . smart-hungry-delete-forward-char))
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))
