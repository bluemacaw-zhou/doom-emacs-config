;;; +lsp-config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; LSP 配置 - 使用环境变量和动态解析
;; =========================================
;;
;; 这个文件包含了所有 LSP 相关的配置，包括：
;; 1. 从环境变量获取 Java 路径
;; 2. 从 Maven settings.xml 获取本地仓库路径
;; 3. 从项目 pom.xml 获取 Lombok 版本
;; 4. 自动启动 Java 项目的 LSP 分析
;;
;; 使用方法：在 config.el 中添加 (load! "+lsp-config")

(message "[+lsp-config] 开始加载 LSP 配置文件...")


;; ============================================================================
;; LSP 配置 - 通用设置
;; ============================================================================
(after! lsp-mode
  (message "[+lsp-config] 配置 LSP 通用设置...")
  ;; LSP 通用配置
  (setq lsp-headerline-breadcrumb-enable t          ;; 启用顶部面包屑导航
        lsp-headerline-breadcrumb-segments '(project file symbols)  ;; 显示：项目 > 文件 > 符号
        lsp-headerline-breadcrumb-icons-enable t     ;; 显示图标
        read-process-output-max (* 3 1024 1024)     ;; 提高 LSP 通信缓冲区大小（3MB）
        lsp-idle-delay 0.500                         ;; LSP 触发延迟（秒）

        ;; Windows 下禁用文件监视，避免性能问题
        lsp-enable-file-watchers nil                ;; 禁用文件变化监视
        lsp-file-watch-threshold 5000)             ;; 文件监视阈值
  (message "[+lsp-config] ✓ LSP 通用设置配置完成"))

(after! lsp-ui
  (message "[+lsp-config] 配置 LSP UI 增强...")

  ;; LSP UI Doc - 悬浮文档显示
  (setq lsp-ui-doc-enable t                        ;; 启用悬浮文档显示
        lsp-ui-doc-show-with-cursor t              ;; 光标移动时显示文档
        lsp-ui-doc-delay 0.5)                      ;; 文档显示延迟（秒）

  ;; LSP UI Sideline - 侧边栏信息显示
  (setq lsp-ui-sideline-enable t                   ;; 启用侧边栏信息显示
        lsp-ui-sideline-show-code-actions t        ;; 显示代码操作提示
        lsp-ui-sideline-show-diagnostics t)        ;; 显示诊断信息

  ;; LSP UI Peek - 引用/定义查找窗口（g D 使用）
  (setq lsp-ui-peek-enable t                       ;; 启用 peek 功能
        lsp-ui-peek-show-directory t               ;; 显示文件目录
        lsp-ui-peek-peek-height 25                 ;; peek 窗口高度
        lsp-ui-peek-list-width 60                  ;; 列表宽度
        lsp-ui-peek-fontify 'on-demand             ;; 按需语法高亮
        lsp-ui-peek-always-show t)                 ;; 即使只有一个结果也显示 peek

  (message "[+lsp-config] ✓ LSP UI 配置完成"))

;; 绑定 g D 到 lsp-ui-peek-find-references（稳定的引用查找窗口）
;; 这样查找引用时不会因为鼠标移动而消失，需要按 q 或 ESC 退出
(after! lsp-mode
  (map! :map lsp-mode-map
        :n "g D" #'lsp-ui-peek-find-references     ;; 查找引用（peek 窗口）
        :n "g d" #'lsp-ui-peek-find-definitions))  ;; 查找定义（peek 窗口）

;; 修复：防止鼠标移动导致 lsp-ui-peek 窗口关闭
;; 原理：lsp-ui-peek 使用 set-transient-map，当事件不在 keymap 中时会关闭窗口
;; 解决：在 keymap 中添加鼠标移动事件绑定，让它被忽略
(after! lsp-ui-peek
  (message "[+lsp-config] 配置 lsp-ui-peek...")

  ;; 在 lsp-ui-peek-mode-map 中添加鼠标移动事件绑定
  ;; 这样鼠标移动不会导致 transient-map 失效
  (when (boundp 'lsp-ui-peek-mode-map)
    ;; 忽略所有鼠标移动事件
    (define-key lsp-ui-peek-mode-map [mouse-movement] #'ignore)
    (define-key lsp-ui-peek-mode-map [scroll-bar-movement] #'ignore)
    ;; 忽略拖拽事件
    (define-key lsp-ui-peek-mode-map [drag-mouse-1] #'ignore)
    (define-key lsp-ui-peek-mode-map [drag-mouse-2] #'ignore)
    (define-key lsp-ui-peek-mode-map [drag-mouse-3] #'ignore)
    ;; 忽略滚轮（但可能需要保留滚轮功能，先添加看看）
    (define-key lsp-ui-peek-mode-map [wheel-up] #'ignore)
    (define-key lsp-ui-peek-mode-map [wheel-down] #'ignore)
    (message "[+lsp-config] ✓ lsp-ui-peek 鼠标事件已绑定到 ignore"))

  ;; 修复中文乱码：设置 lsp-ui-peek 读取文件时使用 UTF-8 编码
  (setq lsp-ui-peek-fontify 'always)

  (message "[+lsp-config] ✓ lsp-ui-peek 配置完成"))

;; 修复 lsp-ui-peek 中文乱码问题
;; 原因：lsp-ui-peek--get-xrefs-in-file 使用 insert-file-contents-literally
;;       这个函数按字节读取文件，不进行编码转换，导致中文乱码
;; 解决：用 advice 替换为 insert-file-contents，让 Emacs 自动检测编码
(after! lsp-ui-peek
  (defadvice! +lsp-ui-peek--fix-chinese-encoding-a (fn file)
    "修复 lsp-ui-peek 中文乱码：使用 insert-file-contents 替代 literally 版本"
    :around #'lsp-ui-peek--get-xrefs-in-file
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (filename &optional visit beg end replace)
                 (insert-file-contents filename visit beg end replace))))
      (funcall fn file)))

  (message "[+lsp-config] ✓ lsp-ui-peek 中文编码修复已应用"))


;; ============================================================================
;; LSP Java 配置 - 使用环境变量和动态解析
;; ============================================================================


;; 加载辅助函数（纯 Elisp，不依赖 Doom 宏）
;; 这些函数也可以被单元测试文件直接加载测试
(message "[+lsp-config] 加载 LSP 辅助函数...")
(load! "+lsp-helper")
(message "[+lsp-config] ✓ LSP 辅助函数加载完成")

;; ---------------------------------------------------------------------------
;; LSP Java 主配置
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; LSP Java 主配置
;; ---------------------------------------------------------------------------

(after! lsp-java
  (message "[+lsp-config] 配置 LSP Java...")

  ;; 动态获取 Java 路径（从环境变量 JAVA_HOME）
  (let ((java-path (+my-lsp-java-get-java-path)))
    (if java-path
        (progn
          (setq lsp-java-java-path java-path)
          (message "[+lsp-config] ✓ Java 路径 = %s" java-path))
      (message "[+lsp-config] ✗ 警告 - 未能从环境变量获取 Java 路径，可能影响 LSP Java 功能")))

  ;; 动态构建 JVM 参数（包含 Lombok）
  (let ((vmargs (+my-lsp-java-build-vmargs)))
    (setq lsp-java-vmargs vmargs)
    (message "[+lsp-config] ✓ JVM 参数已配置（%d 项参数）" (length vmargs)))

  ;; 其他 LSP Java 设置
  (setq lsp-java-workspace-dir +my-jdtls-workspace-dir  ;; JDT LS 工作空间目录（存储项目索引等）
        lsp-java-references-code-lens-enabled t       ;; 启用引用代码透镜（显示方法/类的引用数量）
        lsp-java-implementations-code-lens-enabled t)  ;; 启用实现代码透镜（显示接口实现的快速导航）
  (message "[+lsp-config] ✓ LSP Java 配置完成"))

;; ---------------------------------------------------------------------------
;; 自动启动 LSP（针对 Java 项目）
;; ---------------------------------------------------------------------------

(defvar +my-java-lsp-started-projects nil
  "已启动 LSP 的项目根目录列表，避免重复执行。")

(defun +my-java-auto-start-lsp ()
  "当检测到当前是 Maven Java 项目且该项目尚未启动 LSP 时，自动启动。
同一项目只在第一个 Java 文件打开时执行一次。"
  (let ((project-root (and (fboundp 'projectile-project-root)
                           (projectile-project-root))))
    ;; 已启动过的项目直接跳过
    (when (and project-root
               (not (member project-root +my-java-lsp-started-projects))
               (file-exists-p (expand-file-name "pom.xml" project-root))
               (not (bound-and-true-p lsp-mode)))
      (push project-root +my-java-lsp-started-projects)
      (message "[LSP 自动启动] 检测到 Maven 项目: %s，1秒后启动..." project-root)
      (run-at-time 1 nil
                   (lambda ()
                     (condition-case err
                         (progn
                           (lsp!)
                           (message "[LSP 自动启动] ✓ LSP 已启动，正在分析代码..."))
                       (error
                        (message "[LSP 自动启动] ✗ 启动失败: %s" err))))))))

;; 注册钩子（同时支持 java-mode 和 java-ts-mode）
(add-hook 'java-mode-hook #'+my-java-auto-start-lsp)
(add-hook 'java-ts-mode-hook #'+my-java-auto-start-lsp)
(message "[+lsp-config] ✓ Java LSP 自动启动钩子已注册")


;; ============================================================================
;; 修复 consult-lsp-diagnostics 候选项显示问题
;; ============================================================================
;; 问题：原始 transformer 使用 (format "%-60.60s" ...) 截断显示文本，
;;       导致长路径的文件名后面的行号被截断。当同一文件有多个诊断时，
;;       它们的显示文本可能相同（因为行号被截断），导致 consult 选择错误的诊断。
;; 解决：使用短文件名 + 行号格式，确保每个候选项有唯一的显示文本。
;; ============================================================================
(after! consult-lsp
  (defadvice! +my/fix-consult-lsp-transformer-a (fn file diag)
    "修复 consult-lsp transformer：使用短文件名，确保行号不被截断"
    :around #'consult-lsp--diagnostics--transformer
    (let* ((line (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line))))
           (short-file (file-name-nondirectory file))
           ;; 使用短文件名 + 行号作为显示文本，末尾加空格分隔消息
           (display-text (format "%-25s:%4d  " short-file line)))
      (propertize display-text
                  'consult--candidate (cons file diag)
                  'consult--type (consult-lsp--diagnostics--severity-to-type diag)))))

  ;; 确保预览时显示行号
  (defadvice! +my/consult-lsp-marker-show-line-numbers-a (fn buffer line column)
    "在创建 marker 时确保 buffer 显示行号"
    :around #'consult-lsp--marker-from-line-column
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (display-line-numbers-mode 1)))
    (funcall fn buffer line column))


;; ============================================================================
;; LSP Pyright 配置（Python）
;; ============================================================================

(after! lsp-pyright
  (setq lsp-pyright-auto-import-completions t    ;; 自动导入补全
        lsp-pyright-typechecking-mode "basic"))   ;; 基本类型检查模式（可选：off/basic/strict）

;; ============================================================================
;; LSP Treemacs 配置
;; ============================================================================

(after! lsp-treemacs
  (lsp-treemacs-sync-mode 1))  ;; 启用 Treemacs 与 LSP 的同步显示

;; ============================================================================
;; 诊断和调试命令
;; ============================================================================

;;;###autoload
(defun +my-lsp-java-diagnostic ()
  "显示 LSP Java 配置的诊断信息。

这个命令会输出以下信息，帮助排查配置问题：
- Java 路径（来自 JAVA_HOME）
- Maven 主目录（来自 MAVEN_HOME）
- Maven 本地仓库路径
- Lombok 版本和 JAR 路径
- 当前 JVM 参数

使用方法：M-x +my-lsp-java-diagnostic"
  (interactive)
  (message "========================================")
  (message "LSP Java 配置诊断")
  (message "========================================")
  (message "")
  (message "[Java 路径]")
  (message "  JAVA_HOME: %s" (getenv "JAVA_HOME"))
  (message "  可执行文件: %s" (+my-lsp-java-get-java-path))
  (message "")
  (message "[Maven 配置]")
  (message "  MAVEN_HOME: %s" (+my-lsp-java-get-maven-home))
  (message "  本地仓库: %s" (+my-lsp-java-get-maven-local-repo))
  (message "")
  (message "[Lombok 配置]")
  (let ((version (+my-lsp-java-get-lombok-version-from-pom)))
    (if version
        (message "  项目版本: %s" version)
      (message "  项目版本: 未找到（将使用默认版本 1.18.20）")))
  (message "  JAR 路径: %s" (+my-lsp-java-get-lombok-jar-path))
  (message "")
  (message "[JVM 参数]")
  (dolist (arg (+my-lsp-java-build-vmargs))
    (message "  %s" arg))
  (message "")
  (message "========================================")
  (message "诊断完成！")
  (message "========================================"))

;; 在 *Messages* buffer 中也显示详细信息
(with-current-buffer "*Messages*"
  (message ""))
