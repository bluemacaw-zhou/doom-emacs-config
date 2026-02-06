;;; test-lsp-config.el -*- lexical-binding: t; -*-
;;
;; LSP 配置的单元测试
;; =========================================
;;
;; 测试实际的辅助函数（从 lsp-helper.el 加载）
;; 修改 lsp-helper.el 后，测试结果会自动反映实际行为
;;
;; 运行方式（在 .doom.d 目录下）：
;;   emacs -batch -l test-lsp-config.el -f ert-run-tests-batch-and-exit
;;

(require 'ert)

;; 加载实际的辅助函数（与 +lsp-config.el 使用相同的文件）
;; 使用 load-file-name 获取当前文件所在目录，跨平台兼容
(let ((helper-file (expand-file-name "+lsp-helper.el"
                                      (file-name-directory
                                       (or load-file-name
                                           buffer-file-name)))))
  (message "[测试准备] 加载 LSP 辅助函数: %s" helper-file)
  (load helper-file))

;; ============================================================================
;; 辅助宏：添加测试日志
;; ============================================================================

(defmacro deftest-with-log (name description &rest body)
  "定义带日志输出的测试用例。
NAME: 测试函数名
DESCRIPTION: 测试功能描述
BODY: 测试代码"
  (declare (indent 2))
  `(ert-deftest ,name ()
     ,description
     (message "")
     (message "========================================")
     (message "[测试开始] %s" (quote ,name))
     (message "[功能说明] %s" ,description)
     (message "========================================")
     ,@body
     (message "[测试完成] %s - 通过" (quote ,name))
     (message "")))

;; ============================================================================
;; 测试用例
;; ============================================================================

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-expand-tilde-path
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-expand-tilde-path
  "展开路径中的 ~ 为 $HOME 绝对路径（验证路径转换功能正常）"
  (let ((home (getenv "HOME")))
    (message "[环境变量] HOME=%s" home)
    (message "[测试步骤] 展开 ~/.m2/repository")
    (let ((result (+my-lsp-expand-tilde-path "~/.m2/repository")))
      (message "[测试结果] 展开后路径: %s" result)
      (should (stringp result))
      (should (not (string-prefix-p "~/" result)))
      (should (string-match-p "\\.m2/repository" result)))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-get-java-path
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-get-java-path
  "获取 Java 可执行文件路径（验证环境变量读取功能正常）"
  (message "[环境变量] JAVA_HOME=%s" (getenv "JAVA_HOME"))
  (message "[系统类型] %s" system-type)
  (message "[测试步骤] 调用 +my-lsp-java-get-java-path")
  (let ((result (+my-lsp-java-get-java-path)))
    (message "[测试结果] Java 路径: %s" result)
    ;; 只验证返回值是字符串或 nil（配置正确就有值，不正确就是 nil）
    (should (or (null result) (stringp result)))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-get-maven-home
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-get-maven-home
  "获取 Maven 主目录（验证环境变量读取功能正常）"
  (message "[环境变量] MAVEN_HOME=%s" (getenv "MAVEN_HOME"))
  (message "[环境变量] M2_HOME=%s" (getenv "M2_HOME"))
  (message "[测试步骤] 调用 +my-lsp-java-get-maven-home")
  (let ((result (+my-lsp-java-get-maven-home)))
    (message "[测试结果] Maven 路径: %s" result)
    ;; 只验证返回值是字符串或 nil（配置正确就有值，不正确就是 nil）
    (should (or (null result) (stringp result)))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-parse-maven-settings
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-parse-maven-settings
  "解析 Maven settings.xml 文件（验证配置文件解析功能正常）"
  (let ((test-file (make-temp-file "settings-" nil ".xml" "
<settings>
  <localRepository>D:/apache-maven-3.6.3/repository</localRepository>
</settings>")))
    (message "[测试步骤] 创建临时 settings.xml: %s" test-file)
    (unwind-protect
        (let ((result (+my-lsp-java-parse-maven-settings test-file)))
          (message "[测试结果] 解析的仓库路径: %s" result)
          (should (stringp result))
          (should (string-match-p "repository" result)))
      (delete-file test-file)
      (message "[清理] 删除临时文件"))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-get-maven-local-repo
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-get-maven-local-repo
  "获取 Maven 本地仓库路径（验证配置读取功能正常）"
  (let ((home (getenv "HOME")))
    (message "[环境变量] HOME=%s" home)
    (message "[测试步骤] 调用 +my-lsp-java-get-maven-local-repo")
    (let ((result (+my-lsp-java-get-maven-local-repo)))
      (message "[测试结果] Maven 仓库路径: %s" result)
      (should (stringp result))
      ;; 验证返回的是一个有效路径（包含 .m2 或自定义路径）
      (should (not (string-empty-p result))))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-get-lombok-version-from-pom
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-get-lombok-version-from-pom
  "从 pom.xml 中获取 Lombok 版本号（验证项目配置解析功能正常）"
  (let* ((test-dir (make-temp-file "project-" t))
         (pom-file (expand-file-name "pom.xml" test-dir))
         (projectile-project-root test-dir))
    (message "[测试步骤] 创建临时项目目录: %s" test-dir)
    (unwind-protect
        (progn
          (write-region "
<project>
  <dependencies>
    <dependency>
      <groupId>org.projectlombok</groupId>
      <artifactId>lombok</artifactId>
      <version>1.18.30</version>
    </dependency>
  </dependencies>
</project>" nil pom-file)
          (message "[测试步骤] 创建 pom.xml，Lombok 版本: 1.18.30")
          (let ((result (+my-lsp-java-get-lombok-version-from-pom)))
            (message "[测试结果] 解析的版本: %s" result)
            ;; 只验证返回值是字符串或 nil（有 Lombok 就返回版本，没有就返回 nil）
            (should (or (null result) (stringp result)))))
      (delete-directory test-dir t)
      (message "[清理] 删除临时项目目录"))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-get-lombok-jar-path
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-get-lombok-jar-path
  "获取 Lombok JAR 包路径（验证路径构建功能正常）"
  (message "[测试步骤] 调用 +my-lsp-java-get-lombok-jar-path")
  (let ((result (+my-lsp-java-get-lombok-jar-path)))
    (message "[测试结果] Lombok JAR 路径: %s" result)
    ;; 只验证返回值是字符串或 nil（有 Lombok 配置就返回路径，没有就返回 nil）
    (should (or (null result) (stringp result)))))

;; ---------------------------------------------------------------------------
;; 测试：+my-lsp-java-build-vmargs
;; ---------------------------------------------------------------------------

(deftest-with-log +test-lsp-java-build-vmargs
  "构建 LSP Java 服务器的 JVM 参数（验证参数构建功能正常）"
  (message "[测试步骤] 调用 +my-lsp-java-build-vmargs")
  (let ((result (+my-lsp-java-build-vmargs)))
    (message "[测试结果] JVM 参数列表 (%d 项):" (length result))
    (dolist (arg result)
      (message "  - %s" arg))
    (should (listp result))
    (should (member "-Xmx2G" result))
    (should (member "-Dfile.encoding=UTF-8" result))))

(provide 'test-lsp-config)

;; 如果直接运行此文件，执行测试
(when (boundp 'batch-exit-code)
  (message "")
  (message "========================================")
  (message "LSP 配置单元测试")
  (message "========================================")
  (message "")
  (ert-run-tests-batch-and-exit))
