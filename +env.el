;;; +env.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Emacs 环境变量配置
;; =========================================
;;
;; 此文件定义 Emacs 专用的环境变量，不影响系统环境。
;; 这些变量主要用于 LSP、构建工具等开发环境。
;;

(message "[+env] 开始加载 Emacs 环境变量...")

;; ============================================================================
;; Java 开发环境
;; ============================================================================

;; Java JDK 路径
;; 选项 1: WSL 中使用 Linux 系统的 Java
(setenv "JAVA_HOME" "/usr/lib/jvm/java-21-openjdk-amd64")
;; 选项 2: WSL 中使用 Windows 上的 Java（如需切换，注释掉选项1，取消注释这行）
;; (setenv "JAVA_HOME" "/mnt/d/program/openjdk-17.0.13")

(message "[+env] ✓ JAVA_HOME = %s" (getenv "JAVA_HOME"))

;; ============================================================================
;; Maven 配置
;; ============================================================================

;; 取消从系统继承的 Maven 环境变量，强制使用 ~/.m2/settings.xml
;; 这样可以避免读取系统的 /usr/share/maven/conf/settings.xml
(setenv "MAVEN_HOME" nil)
(setenv "M2_HOME" nil)
(message "[+env] ✓ Maven 配置: 已取消 MAVEN_HOME，使用用户级别 ~/.m2/settings.xml")

;; 如果需要显式设置 Maven，取消下面的注释：
;; (setenv "MAVEN_HOME" "/usr/share/maven")
;; (setenv "M2_HOME" (getenv "MAVEN_HOME"))
;; (message "[+env] ✓ MAVEN_HOME = %s" (getenv "MAVEN_HOME"))

;; ============================================================================
;; PATH 环境变量
;; ============================================================================

;; 将 Java 和 Maven 的 bin 目录添加到 PATH
(let ((java-home (getenv "JAVA_HOME"))
      (maven-home (getenv "MAVEN_HOME")))
  (when java-home
    (let ((java-bin (expand-file-name "bin" java-home)))
      (setenv "PATH" (concat java-bin path-separator (getenv "PATH")))
      (add-to-list 'exec-path java-bin)
      (message "[+env] ✓ 已添加到 PATH: %s" java-bin)))
  (when maven-home
    (let ((maven-bin (expand-file-name "bin" maven-home)))
      (setenv "PATH" (concat maven-bin path-separator (getenv "PATH")))
      (add-to-list 'exec-path maven-bin)
      (message "[+env] ✓ 已添加到 PATH: %s" maven-bin))))

;; ============================================================================
;; jdtls (Java LSP) 工作空间目录
;; ============================================================================

;; jdtls 在此目录存储项目索引、类型信息、依赖分析结果
;; Windows 示例: (defvar +my-jdtls-workspace-dir "C:/jdtls-workspace")
(defvar +my-jdtls-workspace-dir (expand-file-name ".cache/jdtls-workspace" (getenv "HOME"))
  "jdtls workspace 目录，存储项目索引和分析数据。")
(message "[+env] ✓ jdtls workspace = %s" +my-jdtls-workspace-dir)

;; ============================================================================
;; 其他开发环境变量
;; ============================================================================

;; 可以在这里添加其他环境变量
;; 例如：
;; (setenv "GRADLE_HOME" "/path/to/gradle")
;; (setenv "NODE_HOME" "/path/to/node")
;; (setenv "PYTHON_HOME" "/path/to/python")

(message "[+env] ✓ Emacs 环境变量加载完成")
(message "")

(provide '+env)
