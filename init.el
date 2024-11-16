(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))  
(add-to-list 'load-path "~/.emacs.d/ergoemacs-mode")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;BASIC UI CONFIGURATION
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(setq blink-cursor-blinks 0)

;; tabs
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;;colors CURSOR
(set-frame-parameter nil 'cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(cursor-color . "#ffffff"))  ;; Disable line numbers for some modes


(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;FONT
(set-face-attribute 'default nil :font "terminus-14")

;;set "gnu" style for c
(setq c-deafault-style "linux"
      c-basic-offset 4)

;;auto pairnn
(electric-pair-mode 1)

;;Ivy and Counsel
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mapping
;;bufers
(global-set-key (kbd "<f2>") 'bs-show)
(global-set-key (kbd "C-M-i") 'windmove-up)
(global-set-key (kbd "C-M-k") 'windmove-down)
(global-set-key (kbd "C-M-j") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)

;;bufer-move
(use-package buffer-move
  :ensure t)

(global-set-key (kbd "C-I") 'buf-move-up)
(global-set-key (kbd "C-K") 'buf-move-down)
(global-set-key (kbd "C-J") 'buf-move-left)
(global-set-key (kbd "C-L") 'buf-move-right)
;;search ivy

(global-set-key (kbd "C-f") 'swiper)

;;resize window
(defun enlarge-vert ()
  (interactive)
  (enlarge-window 2))

(defun shrink-vert ()
  (interactive)
  (enlarge-window -2))

(defun enlarge-horz ()					
  (interactive)
  (enlarge-window-horizontally 2))

(defun shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))

(define-prefix-command 'my-mapping)
(define-key my-mapping (kbd "C-c k") 'shrink-vert)
(define-key my-mapping (kbd "C-c i") 'enlarge-vert)
(define-key my-mapping (kbd "C-c j") 'shrink-horz)
(define-key my-mapping (kbd "C-c l") 'enlarge-horz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LSP SERVERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;c/C++ MODE
(use-package lsp-mode
  :ensure t
  :hook
  (c++-mode . lsp) 
  (java-mode . lsp)
  (c-mode . lsp))

;;python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred


;;UI LSP WIH CURSOR
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook 'global-company-mode)

;;IMPROVED CANDIDATE SORTING WITH PRESCIENT.EL 
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
										;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;;HELPFUL HELP COMMANDS
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

;;;;;;;;;ORGmode;;;;;;;;;;;
(setq org-directory "~/Docs/org")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(keymap-global-set "C-c a" 'org-agenda)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" default))
 '(initial-scratch-message nil)
 '(package-selected-packages
   '(buffer-move org-tempo gruber-darker-theme company lsp-java forge magit helpful ivy-prescient flycheck lsp-ui lsp-pyright lsp-mode counsel ivy-rich ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
