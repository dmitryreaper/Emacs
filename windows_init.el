(require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;maximize screen
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;;default directory
(setq default-directory "~/") 

;;inhibit splash screen
(setq inhibit-splash-screen t)

;;change the font
(set-face-attribute 'default nil :font "Hack Nerd Font-12")

;;disable menu bar
(menu-bar-mode -1)

;;disable tool bar
(tool-bar-mode -1)

;;cursor-blinks forever
(setq blink-cursor-blinks 0)

;;disable scrol bar
(scroll-bar-mode -1)

;;set "gnu" style for c
(setq c-deafault-style "linux"
      c-basic-offset 4)

;;doom themes
(use-package doom-themes
  :init (load-theme 'doom-ir-black t))

;;auto pairnn
(electric-pair-mode 1)

;;transparent
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 .50)))

;;cursor color
(set-frame-parameter nil 'cursor-color "#ff0000")
(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

;;evil-mode for vim layout
(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-themes use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
