;;;;;;;;;;;;;;;
;; Packages 
;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit dashboard flyspell-correct-ivy all-the-icons-ivy counsel-projectile ivy-gitlab ivy-xref ivy-yasnippet company company-web use-package xclip latex-pretty-symbols latex-preview-pane dockerfile-mode haskell-mode rust-mode evil bash-completion markdown-mode markdown-preview-mode ansible ansible-doc bind-key iedit switch-buffer-functions neotree flycheck-tip eclim flycheck doom-themes ample-theme projectile exec-path-from-shell ssh-config-mode rainbow-delimiters k8s-mode erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;
;; Emacs
;;;;;;;;;;;;;;;

(make-directory "~/.emacs.d/auto-save" "~/.emacs.d")
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save/" t)))

(setq make-backup-files nil)
(setq create-lockfiles nil)

(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setenv "SHELL" "/usr/local/bin/zsh")
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;
;; Copy-paste
;;;;;;;;;;;;;;;

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(use-package xclip
  :if (memq window-system '(mac ns))
  :init
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(use-package xclip
  :unless (memq window-system '(mac ns))
  :init
  (xclip-mode 1))

;;;;;;;;;;;;;;;
;; mac OS iTerm2 compability
;;;;;;;;;;;;;;;

(progn
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
	       function-key-map)))
    (define-key map "\e[1;P10" (kbd "C-M-SPC"))
    (define-key map "\e[1;P24" (kbd "s-p"))
    ))

(setq mac-command-modifier 'meta)

(setq mac-option-modifier 'super)

;;;;;;;;;;;;;;;
;; Useful
;;;;;;;;;;;;;;;

(use-package ivy
  :requires (swiper)
  :bind ("C-s" . swiper)
  :init
  (ivy-mode +1))

(use-package counsel
  :init
  (counsel-mode +1)
  (counsel-projectile-mode +1))

(use-package iedit
  :bind (("C-c C-r a" . iedit-mode-toggle-on-function)
         ("C-c C-r :" . iedit-mode)))

(use-package neotree
  :bind ([f8] . neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :bind-keymap
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1))

(use-package evil-lion
  :bind (("C-c C-a l" . evil-lion-left)
         ("C-c C-a r" . evil-lion-right)))

(use-package company
  :custom ((company-idle-delay 0))
  :bind ("<backtab>" . company-complete-common)
  :init
  (abbrev-mode)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package org
  :bind (:map org-mode-map
              ("C-k l" . org-store-link)
              ("C-k a" . org-agenda)
              ("C-k c" . org-capture)))

(global-auto-revert-mode +1)

;;;;;;;;;;;;;;;
;; Langs 
;;;;;;;;;;;;;;;

(load "~/.emacs.d/langs/erlang.el")

;;;;;;;;;;;;;;;
;; Fancy stuff 
;;;;;;;;;;;;;;;

(load "~/.emacs.d/firacode.el")

(defun greek-transliteration ()
  (interactive)
  (let ((key (read-key "Enter symbol to translite them into Greek: ")))
    (cond ((eq key ?a) (insert "α"))
          ((eq key ?b) (insert "β"))
          ((eq key ?g) (insert "γ"))
          ((eq key ?d) (insert "δ"))
          ((eq key ?e) (insert "ε"))
          ((eq key ?z) (insert "ζ"))
          ((eq key ?h) (insert "η"))
          ((eq key ?o) (insert "ω"))
          ((eq key ?i) (insert "ι"))
          ((eq key ?k) (insert "κ"))
          ((eq key ?l) (insert "λ"))
          ((eq key ?m) (insert "μ"))
          ((eq key ?n) (insert "ν"))
          ((eq key ?x) (insert "ξ"))
          ((eq key ?p) (insert "π"))
          ((eq key ?r) (insert "ρ"))
          ((eq key ?s) (insert "σ"))
          ((eq key ?t) (insert "τ"))
          ((eq key ?f) (insert "φ")))))

(define-key global-map (kbd "M-g s") 'greek-transliteration)
