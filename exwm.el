(require 'package)
(add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;maximize screen
;;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;;default directory
(setq default-directory "~/") 

;;inhibit splash screen
(setq inhibit-splash-screen t)

;;visible bell

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
(setq c-deafault-style "bsd"
      c-basic-offset 4)

;;doom themes
;;(use-package doom-themes
;;  :init (load-theme 'doom-ir-black t))

;;config exwm
(use-package exwm
  :ensure t)

(require 'exwm)
;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name.
(add-hook 'exwm-update-class-hook
  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
;; Global keybindings.
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
        ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
        ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
                     (interactive (list (read-shell-command "Enter: ")))
                     (start-process-shell-command cmd nil cmd)))
        ;; s-N: Switch to certain workspace.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))
;; Enable EXWM
(exwm-enable)

;;configuration for ivy
(use-package ivy
    :diminish
    :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :after ivy
  :custom (counsel-mode 1))

;;desctop
(use-package desktop-environment
  :ensure t
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  
  (desktop-environment-volume-decrement "5%-")
  (desktop-environment-volume-increment "5%+"))

;; mode line
(display-battery-mode 1)
(setq display-time-day-and-date t)
(display-time-mode 1)

;;start programm EXWM
(exwm-input-set-key (kbd "C-s-<return>") 'counsel-linux-app)

;;auto pairnn
(electric-pair-mode 1)

;;transparent
;;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (85 .50)))

;;cursor color
;;(set-frame-parameter nil 'cursor-color "#ff0000")
;;(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

;;evil-mode for vim layout
;;(use-package evil
;;:ensure t
;;  :init (evil-mode 1)
;;:config
;;  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(package-selected-packages
   '(counsel desktop-environment exwm-modeline exwm graphql-mode haskell-mode ivy swiper ivy-rich ivy-avy doom-themes use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
