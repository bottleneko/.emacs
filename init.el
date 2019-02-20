(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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

;;;;;;;;;;;;;;;
;; Encoding
;;;;;;;;;;;;;;;

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;
;; PATH env settings
;;;;;;;;;;;;;;;
(setenv "SHELL" "/usr/local/bin/zsh")

(when (memq window-system '(mac ns x))
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

(if (memq window-system '(mac ns x))
    (progn
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
  (progn
    (require 'xclip)
    (xclip-mode 1)))


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

(global-set-key "\M-\\" 'comint-dynamic-complete-filename)

;;;;;;;;;;;;;;;
;; Package list 
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
    (xclip rust-mode evil bash-completion markdown-mode markdown-preview-mode 0blayout ansible ansible-doc bind-key iedit switch-buffer-functions neotree flycheck-tip eclim flycheck doom-themes ample-theme projectile exec-path-from-shell ssh-config-mode rainbow-delimiters k8s-mode erlang auto-complete-distel ac-helm ac-etags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;
;; AC
;;;;;;;;;;;;;;;
(require 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(global-auto-complete-mode 1)

;;;;;;;;;;;;;;;
;; Iedit
;;;;;;;;;;;;;;;
(require 'iedit)

(define-key global-map (kbd "C-c ;") 'iedit-mode-toggle-on-function)
(define-key global-map (kbd "C-c :") 'iedit-mode)

;;;;;;;;;;;;;;;
(require 'neotree)

(global-set-key [f8] 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;
(require 'projectile)

(projectile-mode +1)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;;;;;;;;;;;;;;;
;; Langs 
;;;;;;;;;;;;;;;
(load "~/.emacs.d/langs/erlang.el")

;;;;;;;;;;;;;;;
;; Emacs temporary files 
;;;;;;;;;;;;;;;
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq create-lockfiles nil)

;;;;;;;;;;;;;;;
;; Fira Code font 
;;;;;;;;;;;;;;;

(if (display-graphic-p)
    (progn
      ;; if graphic
      (defun fira-code-mode--make-alist (list)
	"Generate prettify-symbols alist from LIST."
	(let ((idx -1))
	  (mapcar
	   (lambda (s)
	     (setq idx (1+ idx))
	     (let* ((code (+ #Xe100 idx))
		    (width (string-width s))
		    (prefix ())
		    (suffix '(?\s (Br . Br)))
		    (n 1))
	       (while (< n width)
		 (setq prefix (append prefix '(?\s (Br . Bl))))
		 (setq n (1+ n)))
	       (cons s (append prefix suffix (list (decode-char 'ucs code))))))
	   list)))

      (defconst fira-code-mode--ligatures
	'("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
	  "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
	  "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
	  "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
	  ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
	  "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
	  "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
	  "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
	  ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
	  "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
	  "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
	  "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
	  "x" ":" "+" "+" "*"))

      (defvar fira-code-mode--old-prettify-alist)

      (defun fira-code-mode--enable ()
	"Enable Fira Code ligatures in current buffer."
	(setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
	(setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
	(prettify-symbols-mode t))

      (defun fira-code-mode--disable ()
	"Disable Fira Code ligatures in current buffer."
	(setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
	(prettify-symbols-mode -1))

      (define-minor-mode fira-code-mode
	"Fira Code ligatures minor mode"
	:lighter " Fira Code"
	(setq-local prettify-symbols-unprettify-at-point 'right-edge)
	(if fira-code-mode
	    (fira-code-mode--enable)
	  (fira-code-mode--disable)))

      (defun fira-code-mode--setup ()
	"Setup Fira Code Symbols"
	(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

      (provide 'fira-code-mode)
      )
  ;; else (optional)
  ())
