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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(lsp-pyright)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
