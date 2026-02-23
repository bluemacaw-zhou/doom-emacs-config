;;; +debug-java.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Java 调试配置：dap-mode + dap-java（与 dape 共存）
;;
;; 说明：
;;   dape（Doom debugger 模块默认）负责 Python/Go 等语言
;;   dap-mode + dap-java 负责 Java（通过 jdtls 内置 java.debug.plugin.jar）
;;
;; 调试流程：
;;   1. SPC d b  → 打/取消断点
;;   2. SPC d d  → 选模板启动调试（launch 模式，从 main 开始）
;;   3. SPC d q  → 停止调试（请用此命令，勿直接关闭 DAP buffer）

(message "[+debug-java] 开始加载...")

;; ============================================================================
;; dap-mode 核心配置
;; ============================================================================

(after! dap-mode
  ;; dap-mode.el 不自动加载 dap-ui，必须显式 require
  (require 'dap-ui)

  ;; 启用 dap-ui-mode：向 dap-breakpoints-changed-hook 注册 overlay 刷新函数
  (dap-ui-mode 1)

  ;; 断点行背景色（fringe bitmap 在某些环境不显示时的兜底方案）
  ;; pending = 暗红（打了断点，调试器尚未确认）
  ;; verified = 暗绿（调试中，调试器已确认）
  (with-eval-after-load 'dap-ui
    (set-face-attribute 'dap-ui-pending-breakpoint-face nil
                        :background "#4a0000" :extend t)
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil
                        :background "#003a00" :extend t))

  ;; 调试启动时不自动弹出程序输出窗口
  (setq dap-auto-show-output nil)

  ;; 在 Java buffer 中启用 dap-mode minor mode
  (add-hook 'java-mode-hook    #'dap-mode)
  (add-hook 'java-ts-mode-hook #'dap-mode)

  (require 'dap-java)

  ;; ── 调试模板 ──────────────────────────────────────────────────────────────
  ;; im-msgsvr Spring Boot
  (dap-register-debug-template
   "im-msgsvr :: Spring Boot"
   (list :type "java"
         :request "launch"
         :name "im-msgsvr :: Spring Boot"
         :mainClass "io.bluemacaw.msgsvr.Application"
         :projectName "im-msgsvr"
         :vmArgs "-Dfile.encoding=UTF-8"
         :args ""))

  (message "[+debug-java] ✓ dap-java 配置完成"))

;; ============================================================================
;; 运行当前文件的 main 方法（算法练习专用）
;; ============================================================================
;; 适用场景：每个文件都有独立 main 方法的项目（算法题、单文件测试等）
;; 自动从当前 buffer 解析 package + class，无需手动填写 mainClass

(defun +my/dap-java-has-test-p ()
  "当前 buffer 是否包含 JUnit @Test 注解。"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\s-*@Test\\b" nil t)))

(defun +my/dap-java-debug-current-file ()
  "智能调试当前 Java 文件：
- 包含 @Test 注解 → 调试光标所在的 @Test 方法（dap-java-debug-test-method）
- 有 main 方法   → 直接 launch（dap-debug with mainClass）"
  (interactive)
  (unless (buffer-file-name)
    (user-error "当前 buffer 不是文件"))
  (if (+my/dap-java-has-test-p)
      ;; JUnit 测试类：调试光标处的 @Test 方法
      ;; dap-java-debug-test-method 需要 port 参数，交互时自动生成
      (dap-java-debug-test-method (dap--find-available-port))
    ;; 普通 main 类：构造 launch 配置
    (let* ((class-name (file-name-sans-extension
                        (file-name-nondirectory (buffer-file-name))))
           (package (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^package\\s-+\\([^;]+\\);" nil t)
                        (string-trim (match-string 1)))))
           (main-class (if package (concat package "." class-name) class-name))
           (pom-dir (locate-dominating-file (buffer-file-name) "pom.xml"))
           (project-name (when pom-dir
                           (file-name-nondirectory
                            (directory-file-name pom-dir)))))
      (dap-debug
       (list :type "java"
             :request "launch"
             :name (format "Run: %s" class-name)
             :mainClass main-class
             :projectName (or project-name ""))))))


;; ============================================================================
;; 快捷键：Java buffer 下覆盖 dape 的 SPC d 绑定
;; Python/Go 等其他语言仍走 dape 默认绑定
;; ============================================================================

(map! :map (java-mode-map java-ts-mode-map)
      :leader
      :prefix "d"
      :desc "breakpoint toggle"  "b" #'dap-breakpoint-toggle
      :desc "debug start"        "d" #'dap-java-debug
      :desc "eval at point"      "e" #'dap-eval-thing-at-point
      :desc "eval expression"    "E" #'dap-eval
      :desc "continue"           "c" #'dap-continue
      :desc "next (step over)"   "n" #'dap-next
      :desc "step in"            "i" #'dap-step-in
      :desc "step out"           "o" #'dap-step-out
      :desc "disconnect"         "q" #'dap-disconnect
      :desc "run/debug smart"    "r" #'+my/dap-java-debug-current-file
      :desc "debug test method"  "t" #'dap-java-debug-test-method
      :desc "debug test class"   "T" #'dap-java-debug-test-class)

(message "[+debug-java] ✓ 加载完成")
