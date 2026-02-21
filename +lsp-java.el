;;; +lsp-java.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; LSP Java 配置：jdtls、自动启动、Maven 编译命令、git 后刷新
;; 依赖：+lsp-helper.el（Java路径/Maven路径/Lombok版本辅助函数）

(message "[+lsp-java] 开始加载...")

;; 辅助函数（从环境变量和 pom.xml 解析路径）
(load! "+env-helper")

;; ============================================================================
;; lsp-java 主配置
;; ============================================================================

(after! lsp-java
  ;; 锁定 jdtls 1.38.0（最后一个支持 Java 17 的版本）
  ;; jdtls >= 1.39.0 要求 Java 21 运行时
  (setq lsp-java-jdt-download-url
        "https://download.eclipse.org/jdtls/milestones/1.38.0/jdt-language-server-1.38.0-202408011337.tar.gz")

  ;; ARM Mac：使用 config_mac_arm（默认只识别 config_mac x86_64）
  (when (and (eq system-type 'darwin)
             (string= (car (split-string system-configuration "-")) "aarch64"))
    (let ((arm-config (expand-file-name "config_mac_arm" lsp-java-server-install-dir)))
      (when (file-directory-p arm-config)
        (setq lsp-java-server-config-dir arm-config)
        (message "[+lsp-java] ✓ 使用 ARM 配置: %s" arm-config))))

  ;; Java 可执行文件路径（从 JAVA_HOME 环境变量获取）
  (let ((java-path (+my-lsp-java-get-java-path)))
    (if java-path
        (setq lsp-java-java-path java-path)
      (message "[+lsp-java] ✗ 警告：未能从 JAVA_HOME 获取 Java 路径")))

  ;; JVM 参数（含 Lombok agent）
  (setq lsp-java-vmargs (+my-lsp-java-build-vmargs))

  ;; 其他设置
  (setq lsp-java-workspace-dir +my-jdtls-workspace-dir
        lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t)

  (message "[+lsp-java] ✓ lsp-java 配置完成"))

;; ============================================================================
;; 自动启动 LSP（Maven 项目）
;; ============================================================================

(defvar +my-java-lsp-started-projects nil
  "已启动 LSP 的项目根目录列表，避免同一项目重复启动。")

(defun +my-java-auto-start-lsp ()
  "检测到 Maven 项目时自动启动 LSP，每个项目只触发一次。"
  (let ((project-root (and (fboundp 'projectile-project-root)
                           (projectile-project-root))))
    (when (and project-root
               (not (member project-root +my-java-lsp-started-projects))
               (file-exists-p (expand-file-name "pom.xml" project-root))
               (not (bound-and-true-p lsp-mode)))
      (push project-root +my-java-lsp-started-projects)
      (message "[+lsp-java] 检测到 Maven 项目: %s，1秒后启动 LSP..." project-root)
      (run-at-time 1 nil
                   (lambda ()
                     (condition-case err
                         (progn (lsp!)
                                (message "[+lsp-java] ✓ LSP 已启动"))
                       (error
                        (message "[+lsp-java] ✗ LSP 启动失败: %s" err))))))))

(add-hook 'java-mode-hook    #'+my-java-auto-start-lsp)
(add-hook 'java-ts-mode-hook #'+my-java-auto-start-lsp)

;; ============================================================================
;; Maven 编译命令
;; ============================================================================

(defun +my-java-set-compile-command ()
  "为 Maven 项目设置 compile-command。"
  (when-let ((root (and (fboundp 'projectile-project-root)
                        (projectile-project-root))))
    (when (file-exists-p (expand-file-name "pom.xml" root))
      (setq-local compile-command
                  (concat "cd " (shell-quote-argument root) " && mvn clean compile")))))

(add-hook 'java-mode-hook    #'+my-java-set-compile-command)
(add-hook 'java-ts-mode-hook #'+my-java-set-compile-command)


;; ============================================================================
;; 诊断命令
;; ============================================================================

;;;###autoload
(defun +my-lsp-java-diagnostic ()
  "输出 LSP Java 配置诊断信息（M-x +my-lsp-java-diagnostic）。"
  (interactive)
  (message "========================================")
  (message "LSP Java 配置诊断")
  (message "========================================")
  (message "[Java]  JAVA_HOME: %s" (getenv "JAVA_HOME"))
  (message "[Java]  可执行文件: %s" (+my-lsp-java-get-java-path))
  (message "[Maven] MAVEN_HOME: %s" (+my-lsp-java-get-maven-home))
  (message "[Maven] 本地仓库: %s" (+my-lsp-java-get-maven-local-repo))
  (let ((ver (+my-lsp-java-get-lombok-version-from-pom)))
    (message "[Lombok] 项目版本: %s" (or ver "未找到，使用默认 1.18.20")))
  (message "[Lombok] JAR: %s" (+my-lsp-java-get-lombok-jar-path))
  (message "[JVM 参数]")
  (dolist (arg (+my-lsp-java-build-vmargs))
    (message "  %s" arg))
  (message "========================================"))

(message "[+lsp-java] ✓ 加载完成")
