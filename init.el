;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (nix-mode doom-modeline elnode docker-tramp gitlab-ci-mode racer flycheck-rust lsp-scala lsp-ui cargo auto-org-md ensime json-mode php-mode evil-lion code-stats counsel ivy ivy-erlang-complete magit magit-popup dashboard flyspell-correct-ivy all-the-icons-ivy counsel-projectile ivy-gitlab ivy-xref ivy-yasnippet company company-web use-package xclip latex-pretty-symbols latex-preview-pane dockerfile-mode haskell-mode rust-mode evil bash-completion markdown-mode markdown-preview-mode ansible ansible-doc bind-key iedit switch-buffer-functions neotree flycheck-tip eclim flycheck doom-themes ample-theme projectile exec-path-from-shell ssh-config-mode rainbow-delimiters k8s-mode erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "#000000")))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install-selected-packages))

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

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setenv "SHELL" (executable-find "zsh")))

(use-package prog-mode
  :custom
  (prettify-symbols-unprettify-at-point t)
  :init
  (global-prettify-symbols-mode +1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
  :if (executable-find "xclip")
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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package ivy
  :bind ("C-s" . swiper)
  :init
  (ivy-mode +1))

(use-package counsel
  :init
  (counsel-mode +1)
  (counsel-projectile-mode +1))

(define-key counsel-find-file-map (kbd "C-x C-f") 'counsel-find-file-fallback-command)

(defun counsel-find-file-fallback-command ()
  "Fallback to non-counsel version of current command."
  (interactive)
  (when (bound-and-true-p ivy-mode)
    (ivy-mode -1)
    (add-hook 'minibuffer-setup-hook
              'counsel-find-file-fallback-command--enable-ivy))
  (ivy-set-action
   (lambda (current-path)
     (let ((old-default-directory default-directory))
       (let ((i (length current-path)))
         (while (> i 0)
           (push (aref current-path (setq i (1- i))) unread-command-events)))
       (let ((default-directory "")) (call-interactively 'find-file))
       (setq default-directory old-default-directory))))
  (ivy-done))

(defun counsel-find-file-fallback-command--enable-ivy ()
  (remove-hook 'minibuffer-setup-hook
               'counsel-find-file-fallback-command--enable-ivy)
  (ivy-mode t))

(define-key ivy-minibuffer-map (kbd "C-x C-f") 'minibuffer-ivy-fallback)

(defun minibuffer-ivy-fallback ()
  "Fallback to non ivy version of current command."
  (interactive)
  (when (bound-and-true-p ivy-mode)
    (ivy-mode -1)
    (add-hook 'minibuffer-setup-hook
              'minibuffer-ivy-fallback--enable-ivy))
  (ivy-set-action
   (lambda (current-path)
     (let ((old-default-directory default-directory))
       (when (not (member last-command '(
                                         dired-create-directory
                                         dired-do-copy
                                         dired-do-rename
                                         )))
         (let ((i (length current-path)))
           (while (> i 0)
             (push (aref current-path (setq i (1- i))) unread-command-events))))
       (let ((default-directory "")) (call-interactively last-command))
       (setq default-directory old-default-directory))))
  (ivy-done))

(defun minibuffer-ivy-fallback--enable-ivy  ()
  (remove-hook 'minibuffer-setup-hook
               'minibuffer-ivy-fallback--enable-ivy )
  (ivy-mode t))

(use-package iedit
  :bind (("C-x C-a ;" . iedit-mode-toggle-on-function)
         ("C-x C-a :" . iedit-mode)))

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
  :bind (("C-x C-a l" . evil-lion-left)
         ("C-x C-a r" . evil-lion-right)))

(use-package company
  :custom ((company-idle-delay 0))
  :bind ("<backtab>" . company-complete-common)
  :init
  (abbrev-mode)
  (add-hook 'after-init-hook 'global-company-mode))

;(use-package org
;  :bind (:map org-mode-map
;              ("C-k l" . org-store-link)
;              ("C-k a" . org-agenda)
;              ("C-k c" . org-capture)))

(global-auto-revert-mode +1)

(use-package yasnippet
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :init
  (yas-global-mode +1))

(use-package gitlab
  :if (getenv "GITLAB_HOST")
  :custom
  (gitlab-host (getenv "GITLAB_HOST"))
  (gitlab-token-id (getenv "GITLAB_TOKEN")))

;;;;;;;;;;;;;;;
;; Langs
;;;;;;;;;;;;;;;

(load "~/.emacs.d/langs/erlang.el")
(load "~/.emacs.d/langs/rust.el")
(add-to-list 'load-path "~/.emacs.d/git-packages/kubernetes-el")
(load "kubernetes")

;;;;;;;;;;;;;;;
;; Fancy stuff
;;;;;;;;;;;;;;;

(use-package code-stats
  :if (getenv "CODESTATS_TOKEN")
	:config
	(setq code-stats-token (getenv "CODESTATS_TOKEN"))
	:hook
	((prog-mode . code-stats-mode)
	 (yaml-mode . code-stats-mode)
	 (kill-emacs .(lambda () (code-stats-sync :wait))))
	:init
	(run-with-idle-timer 30 t #'code-stats-sync))

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

(use-package ansi-color
  :hook ((compilation-filter . colorize-compilation))
  :init
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))
