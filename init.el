(tab-bar-mode 1)
(setq inhibit-startup-message t) ;Disable open first message startup
(scroll-bar-mode -1)             ;Disable scroll bar
(tool-bar-mode -1)               ;Disable toolbar
(tooltip-mode -1)                ;Disable tooltip
(menu-bar-mode -1)
(set-fringe-mode 0)              ;Border frame

;TABBBBBBBB
(setq-default tab-width 4)
(setq-default tab-always-indent t)
(defun my-insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
(global-set-key (kbd "TAB") 'my-insert-tab-char)


(global-display-line-numbers-mode) ;Line number mode

;disable line numbers for some modes
(dolist  (mode '(org-mode-hook
		 term-mode-hook
		 shell-mode-hook
		 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;disable backup files,autosaving
(setq make-backup-files   nil)
(setq auto-safe-list-file-name    nil)
(setq auto-safe-default    nil)

;Disable beep signal
(setq visible-bell t)

;Set default theme
;doom-ir-black, doom-homage-black, 
(load-theme 'doom-homage-black t)


;global exit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) 

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
(package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind(("C-s" . swiper)
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

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;all icons
(use-package all-the-icons
  :if (display-graphic-p))
;icons for directory
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;airline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
	("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history)))

(use-package doom-themes)

;(use-package general
;	:config
;	(general-create-definer rune/leader-keys
;   	:keymaps '(normal insert visual emacs)
;   	:prefix "SPC"
;   	:global-prefix "C-SPC")

;	(rune/leader-keys
;		"t" '(:ignore t :which-key "toggles")
;		"tt" '(counsel-load-theme :which-key "choose theme")))

;EVIL MODE
(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
				  eshell-mode
				  git-rebase-mode
				  erc-mode
				  circe-server-mode
				  circe-chat-mode
				  circe-query-mode
				  sauron-mode
				  term-modeE))
	(add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state) ;fast exit in normal mode
  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state)

  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;managment project 
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Src/")
	(setq projectile-project-search-path '("~/Src/")))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;lsp configuration
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
  		(python-mode . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-select-next)
	      ("backtab" . company-select-previous))
  :custom
  (company-minimum-prefix-lenght 1)
  (company-idle-delay 0.01))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(initial-scratch-message "")
 '(package-selected-packages
   '(lsp-pyright flycheck company lsp-mode counsel-projectile evil-collection evil general doom-themes ivy-rich which-key rainbow-delimiters doom-modeline counsel ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
