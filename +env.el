;;; +env.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Emacs 环境变量配置 - 从系统环境变量读取并验证
;; =========================================
;;
;; 逻辑：读取系统环境变量 → 验证路径是否存在 → 未配置则给出提示。
;; 不做自动检测，依赖用户在 shell profile 中正确设置环境变量。
;;

(message "[+env] 开始加载 Emacs 环境变量...")

;; ============================================================================
;; 辅助函数
;; ============================================================================

(defun +env--validate (name &optional check-bin)
  "读取并验证环境变量 NAME。

如果环境变量已设置且目录存在，返回其值。
CHECK-BIN 为非 nil 时，额外检查 bin/ 子目录是否存在。
未设置或路径无效时打印警告并返回 nil。"
  (let ((value (getenv name)))
    (cond
     ((null value)
      (message "[+env] ✗ %s 未设置，请在 shell profile 中配置" name)
      nil)
     ((not (file-directory-p value))
      (message "[+env] ✗ %s = %s （路径不存在）" name value)
      nil)
     ((and check-bin (not (file-directory-p (expand-file-name "bin" value))))
      (message "[+env] ✗ %s = %s （bin/ 目录不存在）" name value)
      nil)
     (t
      (message "[+env] ✓ %s = %s" name value)
      value))))

;; ============================================================================
;; Java 开发环境
;; ============================================================================

(let ((java-home (+env--validate "JAVA_HOME" t)))
  (when java-home
    (let ((java-bin (expand-file-name "bin" java-home)))
      (setenv "PATH" (concat java-bin path-separator (getenv "PATH")))
      (add-to-list 'exec-path java-bin))))

;; ============================================================================
;; Maven 配置
;; ============================================================================

;; 优先读 MAVEN_HOME，未设置则尝试 M2_HOME
(let ((maven-home (or (+env--validate "MAVEN_HOME")
                      (+env--validate "M2_HOME"))))
  (when maven-home
    (let ((maven-bin (expand-file-name "bin" maven-home)))
      (when (file-directory-p maven-bin)
        (setenv "PATH" (concat maven-bin path-separator (getenv "PATH")))
        (add-to-list 'exec-path maven-bin)))))

;; ============================================================================
;; jdtls (Java LSP) 工作空间目录
;; ============================================================================

(defvar +my-jdtls-workspace-dir (expand-file-name ".cache/jdtls-workspace" (getenv "HOME"))
  "jdtls workspace 目录，存储项目索引和分析数据。")
(message "[+env] ✓ jdtls workspace = %s" +my-jdtls-workspace-dir)

(message "[+env] ✓ Emacs 环境变量加载完成")
(message "")

(provide '+env)
