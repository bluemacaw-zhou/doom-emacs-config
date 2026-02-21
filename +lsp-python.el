;;; +lsp-python.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; LSP Python 配置：lsp-pyright

(message "[+lsp-python] 开始加载...")

(after! lsp-pyright
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-typechecking-mode "basic"))  ;; 可选：off / basic / strict

(message "[+lsp-python] ✓ 加载完成")
