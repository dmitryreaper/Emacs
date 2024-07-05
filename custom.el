(set-frame-parameter (selected-frame) 'alpha '(85 . 50))

(set-frame-parameter nil 'cursor-color "#ff0000")

(set-face-attribute 'default nil :font "Hack Nerd Font-11")

(define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state)
;;lsp
(load-theme 'doom-ir-black t)

(put 'customize-themes 'disabled nil)

(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
        (c-mode . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
