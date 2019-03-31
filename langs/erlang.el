; (add-to-list 'load-path "~/.emacs.d/lib/erlang/lib/wrangler-1.2.0/elisp")

; (require 'wrangler)

;;;;;;;;;;;;;;;
;; Major mode
;;;;;;;;;;;;;;;

(use-package erlang
  :mode (("\\.erl\\'"     . erlang-mode)
         ("\\.hrl\\'"     . erlang-mode)
         ("\\.xrl\\'"     . erlang-mode)
         ("\\.yrl\\'"     . erlang-mode)
         ("\\.rel\\'"     . erlang-mode)
         ("\\.app\\'"     . erlang-mode)
         ("\\.app.src\\'" . erlang-mode)
         ("\\.config\\'"  . erlang-mode)
         ("\\.es\\'"      . erlang-mode))
  :bind (:map erlang-mode-map
              (("C-c b"   . erlang/wrap-word-into-binary)
	             ("C-c m b" . erlang/wrap-multiline-into-binary)
               ("C-c ;"   . ivy-erlang-complete)))
  :hook ((after-save  . ivy-erlang-complete-reparse)
         (erlang-mode . ivy-erlang-complete-init)
         (erlang-mode . flycheck-mode))
  :custom ((ivy-erlang-complete-erlang-root "~/.kerl/installations/21.2/")
           (ivy-erlang-complete-ignore-dirs '(".git"))
           (erlang-root-dir                 "~/.kerl/builds/21.2/release_21.2/")
           (indent-tabs-mode                nil)
           (erlang-indent-level             2)
	         (erlang-tab-always-indent        nil))
  :init
  (add-to-list 'exec-path "~/.kerl/builds/21.2/release_21.2/bin"))

;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;

(use-package flycheck
  :init
  (require 'flycheck)
  (flycheck-define-checker erlang-otp
    "Erang/OTP syntax checker"
    :command ("~/.emacs.d/git-packages/syntaxerl/syntaxerl" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes (erlang-mode)))

;; При переключении на distel ie session erlang-otp (FlyCheck checker)
;; падает с кодом ошибки 1 при каждом изменении буффера что немного
;; нервирует, посему при переходе во все подобные буффера flycheck-mode
;; будет отключаться 
;(add-hook 'switch-buffer-functions
;	  (lambda (prev cur)
;	    (unless (and
;               (eq (string-match "ie session" (format "%S" cur)) nil)
;			          (eq (string-match ".hrl"       (format "%S" cur)) nil)
;			          (eq (string-match ".app.src"   (format "%S" cur)) nil)
;			          (eq (string-match ".src"       (format "%S" cur)) nil)
;			          (eq (string-match ".config"    (format "%S" cur)) nil))
;	      (flycheck-mode -1))))

;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;

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
