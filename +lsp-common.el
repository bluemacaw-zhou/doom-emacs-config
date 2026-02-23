;;; +lsp-common.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; LSP 通用配置：lsp-mode、lsp-ui、lsp-ui-peek、consult-lsp、lsp-treemacs
;; 与具体语言无关，所有语言共用。

(message "[+lsp-common] 开始加载...")

;; ============================================================================
;; lsp-mode 通用设置
;; ============================================================================

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(project file symbols)
        lsp-headerline-breadcrumb-icons-enable t
        read-process-output-max (* 3 1024 1024)   ;; LSP 通信缓冲区 3MB
        lsp-idle-delay 0.500
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 5000
        lsp-keep-workspace-alive nil               ;; 关闭 buffer 后释放 workspace
        lsp-response-timeout 5))

;; ============================================================================
;; lsp-ui 设置
;; ============================================================================

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-peek-peek-height 25
        lsp-ui-peek-list-width 60
        lsp-ui-peek-fontify 'on-demand
        lsp-ui-peek-always-show t))

;; g d / g D → peek 窗口查找定义/引用（按 q 退出，不会因鼠标移动消失）
(after! lsp-mode
  (map! :map lsp-mode-map
        :n "g D" #'lsp-ui-peek-find-references
        :n "g d" #'lsp-ui-peek-find-definitions))

;; ============================================================================
;; lsp-ui-peek 修复
;; ============================================================================

(after! lsp-ui-peek
  ;; 修复：鼠标移动导致 peek 窗口关闭
  ;; lsp-ui-peek 使用 set-transient-map，keymap 中未定义的事件会触发关闭
  (when (boundp 'lsp-ui-peek-mode-map)
    (define-key lsp-ui-peek-mode-map [mouse-movement]    #'ignore)
    (define-key lsp-ui-peek-mode-map [scroll-bar-movement] #'ignore)
    (define-key lsp-ui-peek-mode-map [drag-mouse-1]      #'ignore)
    (define-key lsp-ui-peek-mode-map [drag-mouse-2]      #'ignore)
    (define-key lsp-ui-peek-mode-map [drag-mouse-3]      #'ignore)
    (define-key lsp-ui-peek-mode-map [wheel-up]          #'ignore)
    (define-key lsp-ui-peek-mode-map [wheel-down]        #'ignore))

  ;; 修复：peek 窗口中文乱码
  ;; insert-file-contents-literally 按字节读取，不做编码转换
  ;; 替换为 insert-file-contents，让 Emacs 自动检测 UTF-8
  (setq lsp-ui-peek-fontify 'always)

  (defadvice! +lsp-ui-peek--fix-chinese-encoding-a (fn file)
    "修复 lsp-ui-peek 中文乱码。"
    :around #'lsp-ui-peek--get-xrefs-in-file
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (filename &optional visit beg end replace)
                 (insert-file-contents filename visit beg end replace))))
      (funcall fn file))))

;; ============================================================================
;; consult-lsp 修复
;; ============================================================================

(after! consult-lsp
  ;; 修复：diagnostics 候选项行号被截断
  ;; 原始 transformer 用 "%-60.60s" 截断，同文件多个诊断时显示文本相同
  ;; 改为 短文件名:行号 格式，每条唯一
  (defadvice! +my/fix-consult-lsp-transformer-a (fn file diag)
    "修复 consult-lsp transformer：使用短文件名，确保行号不被截断。"
    :around #'consult-lsp--diagnostics--transformer
    (let* ((line (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line))))
           (short-file (file-name-nondirectory file))
           (display-text (format "%-25s:%4d  " short-file line)))
      (propertize display-text
                  'consult--candidate (cons file diag)
                  'consult--type (consult-lsp--diagnostics--severity-to-type diag))))

  ;; 预览时确保显示行号
  (defadvice! +my/consult-lsp-marker-show-line-numbers-a (fn buffer line column)
    "在创建 marker 时确保 buffer 显示行号。"
    :around #'consult-lsp--marker-from-line-column
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (display-line-numbers-mode 1)))
    (funcall fn buffer line column)))

(message "[+lsp-common] ✓ 加载完成")
