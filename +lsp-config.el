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
  ;; LSP UI 增强 - 显示文档、诊断信息等
  (setq lsp-ui-doc-enable t                        ;; 启用悬浮文档显示
        lsp-ui-doc-show-with-cursor t              ;; 光标移动时显示文档
        lsp-ui-doc-delay 0.5                       ;; 文档显示延迟（秒）
        lsp-ui-sideline-enable t                   ;; 启用侧边栏信息显示
        lsp-ui-sideline-show-code-actions t        ;; 显示代码操作提示
        lsp-ui-sideline-show-diagnostics t)       ;; 显示诊断信息
  (message "[+lsp-config] ✓ LSP UI 配置完成"))


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
  (setq lsp-java-workspace-dir "C:/jdtls-workspace"  ;; JDT LS 工作空间目录（存储项目索引等）
        lsp-java-references-code-lens-enabled t       ;; 启用引用代码透镜（显示方法/类的引用数量）
        lsp-java-implementations-code-lens-enabled t)  ;; 启用实现代码透镜（显示接口实现的快速导航）
  (message "[+lsp-config] ✓ LSP Java 配置完成"))

;; ---------------------------------------------------------------------------
;; 自动启动 LSP（针对 Java 项目）
;; ---------------------------------------------------------------------------

;; 定义测试函数
(defun +my-test-java-mode-hook ()
  "测试钩子是否被调用"
  (message "")
  (message "========================================")
  (message "[测试] java-mode-hook 被触发了！")
  (message "[测试] 当前 buffer: %s" (buffer-name))
  (message "[测试] 当前 mode: %s" major-mode)
  (message "[测试] 文件路径: %s" (buffer-file-name))
  (message "========================================"))

;; 定义自动启动函数
(defun +my-java-auto-start-lsp ()
  "当检测到当前是 Java 项目（存在 pom.xml）时，自动启动 LSP。

工作原理：
1. 检测当前文件是否在 Projectile 项目中
2. 检查项目根目录是否存在 pom.xml
3. 如果是 Maven 项目，延迟 1 秒后启动 LSP
4. LSP 启动后会自动进行代码分析和索引

延迟原因：避免影响 Emacs 启动速度，让用户先打开文件。

注意：LSP 启动后可能需要几秒到几十秒才能完成初始分析，
这段时间内代码补全和跳转功能可能不完整。"
  ;; ===== 日志：开始检查 =====
  (message "")
  (message "========================================")
  (message "[LSP 自动启动] 开始检查 Java 项目")
  (message "========================================")
  (message "[当前文件] %s" (or (buffer-file-name) (buffer-name)))
  (message "[当前模式] %s" major-mode)

  ;; 获取项目根目录（projectile-project-root 是函数，不是变量）
  (let ((project-root (and (fboundp 'projectile-project-root)
                           (projectile-project-root))))

    ;; 检查 1：Projectile 是否可用
    (message "[检查 1/4] Projectile 是否可用")
    (if (fboundp 'projectile-project-root)
        (message "  ✓ Projectile 已加载")
      (message "  ✗ Projectile 未加载"))

    ;; 检查 2：是否在项目中
    (message "[检查 2/4] 当前是否在 Projectile 项目中")
    (if project-root
        (message "  ✓ 是，项目根目录: %s" project-root)
      (message "  ✗ 否，不在任何项目中"))

    ;; 检查 3：是否存在 pom.xml
    (when project-root
      (let ((pom-file (expand-file-name "pom.xml" project-root)))
        (message "[检查 3/4] 项目根目录是否存在 pom.xml")
        (message "  检查路径: %s" pom-file)
        (if (file-exists-p pom-file)
            (message "  ✓ pom.xml 存在")
          (message "  ✗ pom.xml 不存在"))))

    ;; 检查 4：LSP 是否已启动
    (message "[检查 4/4] LSP 是否已启动")
    (if (bound-and-true-p lsp-mode)
        (message "  ✓ LSP 已启动，无需重复启动")
      (message "  ✗ LSP 未启动，准备启动"))

    ;; ===== 判断是否启动 LSP =====
    (if (and project-root
             (file-exists-p (expand-file-name "pom.xml" project-root))
             (not (bound-and-true-p lsp-mode)))
        (progn
          (message "")
          (message "[LSP 自动启动] 满足所有条件，1秒后启动 LSP...")
          ;; 延迟 1 秒启动，避免影响 Emacs 启动速度
          (run-at-time 1 nil
                       (lambda ()
                         (message "[LSP 自动启动] 开始启动 LSP...")
                         (condition-case err
                             (progn
                               (lsp!)
                               (message "[LSP 自动启动] ✓ LSP 启动命令已执行")
                               (message "[LSP 自动启动] 正在分析代码，请稍候...")
                               (message "========================================"))
                           (error
                            (message "[LSP 自动启动] ✗ LSP 启动失败: %s" err)
                            (message "========================================"))))))
      (progn
        (message "")
        (message "[LSP 自动启动] 不满足启动条件，跳过自动启动")
        (message "========================================")))))

;; 直接注册钩子（不使用 after! java，因为 java 不是有效的 feature）
;; 注意：同时支持 java-mode 和 java-ts-mode（tree-sitter 模式）
(message "[+lsp-config] 注册 Java LSP 自动启动钩子...")

;; 传统 java-mode 钩子
(add-hook 'java-mode-hook #'+my-test-java-mode-hook)
(add-hook 'java-mode-hook #'+my-java-auto-start-lsp)
(message "[+lsp-config] ✓ java-mode-hook 已注册")

;; tree-sitter 模式的钩子（当使用 +tree-sitter flag 时）
(add-hook 'java-ts-mode-hook #'+my-test-java-mode-hook)
(add-hook 'java-ts-mode-hook #'+my-java-auto-start-lsp)
(message "[+lsp-config] ✓ java-ts-mode-hook 已注册")

(message "[+lsp-config] ✓ Java LSP 自动启动钩子全部注册完成")
(message "")

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
