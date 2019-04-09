(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui)

(use-package rust-mode
  :custom (rust-format-on-save t)
  :hook ((rust-mode . lsp)
         (rust-mode . racer-mode)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
