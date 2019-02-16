(setq erlang-root-dir "~/.kerl/builds/21.2/release_21.2/")
(add-to-list 'exec-path "~/.kerl/installations/21.2/erts-10.2/bin")
(add-to-list 'load-path "~/.emacs.d/git-packages/distel/elisp")
; (add-to-list 'load-path "~/.emacs.d/lib/erlang/lib/wrangler-1.2.0/elisp")


; (setq erlang-xemacs-p nil)
; (setq wrangler-erl-node "test@localhost")

(require 'auto-complete)
(require 'auto-complete-distel)
(require 'erlang-start)
(require 'distel)
(require 'flycheck)
(require 'subr-x)
(require 'evil)
(require 'evil-lion)
; (require 'wrangler)

;;;;;;;;;;;;;;;
;; Erlang 
;;;;;;;;;;;;;;;
(distel-setup)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;; Костыль чтобы при окончании компиляции distel буффер *compilation*
;; в случае успешной компиляции закрывался сам
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        (progn
          (run-at-time
           "0 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-to-list 'ac-sources 'auto-complete-distel)

(flycheck-define-checker erlang-otp
  "An Erlang syntax checker using the Erlang interpreter."
  :command ("erlc"
	    "-Wall"
	    "-o" temporary-directory
            "-I" "../include"
	    "-I" "../../include"
            "-I" "../../../include"
	    "-I" "./"
	    "-I" "../"
	    "-I" "../../"
	    "-I" "../../.."
	    source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes (erlang-mode))

;; При переключении на distel ie session erlang-otp (FlyCheck checker)
;; падает с кодом ошибки 1 при каждом изменении буффера что немного
;; нервирует, посему при переходе во все подобные буффера flycheck-mode
;; будет отключаться 
(add-hook 'switch-buffer-functions
	  (lambda (prev cur)
	    (unless (and (eq (string-match "ie session" (format "%S" cur)) nil)
			 (eq (string-match ".hrl" (format "%S" cur))       nil)
			 (eq (string-match ".app.src" (format "%S" cur))   nil)
			 (eq (string-match ".src" (format "%S" cur))       nil)
			 (eq (string-match ".config" (format "%S" cur))    nil))
	      (flycheck-mode -1))))

(add-hook 'erlang-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq erlang-indent-level 2)
	    (setq erlang-tab-always-indent nil)
            (flycheck-select-checker 'erlang-otp)
            (flycheck-mode)))

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(defun erlang/wrap-word-into-binary ()
  (interactive)
  (let ((beg (region-beginning))
	(end (region-end)))
    (save-excursion (kill-region beg end)
		    (goto-char beg)
		    (let ((line-beg (line-beginning-position)))
		      (progn (insert "<<\"") (yank) (insert "\">>")
			     (goto-char end)
			     (erlang-indent-region line-beg (line-end-position))))
		    )))



(defun erlang/wrap-multiline-into-binary ()
  (interactive)
  (let* ((beg   (region-beginning))
	 (end   (region-end))
	 (terms (buffer-substring-no-properties beg end))
	 (seq   (split-string terms "\n"))
	 (string-seq (mapcar (lambda (elem)
			       (concat "\"" (string-trim elem) "\""))
			     seq))
	 (multiline-string (string-join string-seq ",\n"))
	 (multiline-binary (concat "<<" multiline-string ">>")))
    (save-excursion (kill-region beg end)
		    (goto-char beg)
		    (insert multiline-binary)
		    (goto-char end)
		    (move-end-of-line end)
		    (erlang-indent-region beg (point)))))

(bind-keys :map erlang-mode-map
	   :prefix "C-c"
           :prefix-map mode-specific-map
           ("b"   . erlang/wrap-word-into-binary)
	   ("m b" . erlang/wrap-multiline-into-binary)
	   ("a"   . evil-lion-left)
	   ("A"   . evil-lion-right))
