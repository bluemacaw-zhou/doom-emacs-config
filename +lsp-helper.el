;;; +lsp-helper.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; LSP 辅助函数 - 纯 Elisp 实现，不依赖 Doom 宏
;; =========================================
;;

;;;###autoload
(defun +my-lsp-expand-tilde-path (path)
  "展开路径中的 ~ 为 HOME 环境变量的绝对路径。

如果 PATH 以 ~ 开头，则使用 $HOME 环境变量替换 ~；
否则原样返回路径。

这个函数比 Emacs 的 expand-file-name 更可靠，因为它显式使用 HOME 环境变量，
避免了某些情况下 ~ 解析失败的问题。

示例：
  输入: \"~/.m2/repository\"
  输出: \"C:/Users/username/.m2/repository\" (Windows)
  输出: \"/home/username/.m2/repository\" (Linux)"
  (if (string-prefix-p "~/" path)
      (let ((home (getenv "HOME")))
        (if home
            ;; 将 ~/xxx 转换为 $HOME/xxx
            (expand-file-name (substring path 2) home)
          ;; 如果 HOME 未设置，回退到 expand-file-name
          (expand-file-name path)))
    ;; 不以 ~/ 开头，直接展开
    (expand-file-name path)))

;;;###autoload
(defun +my-lsp-java-get-java-path ()
  "从环境变量获取 Java 可执行文件路径。

查找顺序：
1. JAVA_HOME 环境变量（推荐）
2. 系统 PATH 中的 java 命令

返回示例：D:/program/openjdk-17.0.13/bin/java.exe

使用前请确保设置了 JAVA_HOME 环境变量，例如：
  Windows: setx JAVA_HOME D:\\program\\openjdk-17.0.13
  Linux/Mac: export JAVA_HOME=/usr/lib/jvm/java-17-openjdk"
  (let ((java-home (getenv "JAVA_HOME")))
    (cond
     ;; 优先使用 JAVA_HOME 环境变量
     ((and java-home (file-exists-p java-home))
      (let* ((java-exe (if (eq system-type 'windows-nt)
                           "bin/java.exe"           ;; Windows 下是 java.exe
                         "bin/java"))              ;; Linux/Mac 下是 java
             (java-path (expand-file-name java-exe java-home)))
        (if (file-exists-p java-path)
            java-path
          (message "Warning: JAVA_HOME 指向的路径不存在: %s" java-path)
          nil)))
     ;; 回退到使用 exec-path 查找 java
     ((executable-find "java")
      (executable-find "java"))
     ;; 都找不到则报错
     (t
      (message "Error: 无法找到 Java，请设置 JAVA_HOME 环境变量")
      nil))))

;;;###autoload
(defun +my-lsp-java-get-maven-home ()
  "从环境变量获取 Maven 主目录。

查找顺序：
1. MAVEN_HOME 环境变量
2. M2_HOME 环境变量（旧版 Maven）

返回示例：D:/apache-maven-3.6.3

如果都未设置则返回 nil。"
  (or (getenv "MAVEN_HOME")
      (getenv "M2_HOME")
      (progn
        (message "Warning: 未设置 MAVEN_HOME 或 M2_HOME 环境变量")
        nil)))

;;;###autoload
(defun +my-lsp-java-parse-maven-settings (settings-file)
  "解析 Maven settings.xml 文件，获取本地仓库路径。

SETTINGS-FILE 是 settings.xml 的文件路径。

返回本地仓库路径，如果解析失败则返回默认路径 ~/.m2/repository。

settings.xml 示例：
  <settings>
    <localRepository>D:/apache-maven-3.6.3/repository</localRepository>
  </settings>"
  (let ((default-repo "~/.m2/repository"))  ;; Maven 默认本地仓库路径
    (if (and settings-file (file-exists-p settings-file))
        (with-temp-buffer
          (insert-file-contents settings-file)
          ;; 使用正则表达式解析 <localRepository> 标签
          (save-excursion
            (if (re-search-forward "<localRepository>\\([^<]+\\)</localRepository>" nil t)
                (let ((repo-path (match-string 1)))
                  ;; 展开路径中的 ~ 为 $HOME 绝对路径（避免解析失败）
                  (+my-lsp-expand-tilde-path repo-path))
              ;; 文件存在但没有 <localRepository> 标签，返回默认路径
              (+my-lsp-expand-tilde-path default-repo))))
      ;; 文件不存在，展开默认路径
      (+my-lsp-expand-tilde-path default-repo))))

;;;###autoload
(defun +my-lsp-java-get-maven-local-repo ()
  "获取 Maven 本地仓库路径。

查找顺序：
1. $MAVEN_HOME/conf/settings.xml 中的 <localRepository>
2. ~/.m2/settings.xml 中的 <localRepository>
3. 默认路径 ~/.m2/repository

返回示例：D:/apache-maven-3.6.3/repository"
  (let* ((maven-home (+my-lsp-java-get-maven-home))
         (global-settings (when maven-home
                            (expand-file-name "conf/settings.xml" maven-home)))
         ;; 先展开 ~ 路径为绝对路径，避免解析失败
         (user-settings (+my-lsp-expand-tilde-path "~/.m2/settings.xml")))
    (cond
     ((and global-settings (file-exists-p global-settings))
      (+my-lsp-java-parse-maven-settings global-settings))
     ((and user-settings (file-exists-p user-settings))
      (+my-lsp-java-parse-maven-settings user-settings))
     (t
      (message "使用默认 Maven 仓库路径")
      ;; 展开 ~ 为绝对路径
      (+my-lsp-expand-tilde-path "~/.m2/repository")))))

;;;###autoload
(defun +my-lsp-java-get-lombok-version-from-pom ()
  "从当前项目的 pom.xml 中获取 Lombok 版本。

返回 Lombok 版本号（如 \"1.18.20\"），如果未找到则返回 nil。

pom.xml 示例 1（直接指定版本）：
  <dependencies>
    <dependency>
      <groupId>org.projectlombok</groupId>
      <artifactId>lombok</artifactId>
      <version>1.18.20</version>
    </dependency>
  </dependencies>

pom.xml 示例 2（使用属性）：
  <properties>
    <lombok.version>1.18.20</lombok.version>
  </properties>
  <dependencies>
    <dependency>
      <groupId>org.projectlombok</groupId>
      <artifactId>lombok</artifactId>
      <version>${lombok.version}</version>
    </dependency>
  </dependencies>"
  (let ((project-root (and (fboundp 'projectile-project-root)
                           (projectile-project-root))))
    (when project-root
      (let ((pom-file (expand-file-name "pom.xml" project-root)))
        (when (file-exists-p pom-file)
          (with-temp-buffer
            (insert-file-contents pom-file)
            ;; 查找 Lombok 依赖的版本号
            ;; 支持两种格式：
            ;; 1. 直接在 dependency 中指定 <version>1.18.20</version>
            ;; 2. 使用属性 <version>${lombok.version}</version>
            (save-excursion
              (cond
               ;; 尝试查找直接的 version 标签
               ((and (re-search-forward "<groupId>org.projectlombok</groupId>\\s-*<artifactId>lombok</artifactId>\\s-*<version>\\([^<]+\\)</version>" nil t)
                     (match-string 1)))
               ;; 尝试查找使用属性的版本
               ((and (re-search-forward "<groupId>org.projectlombok</groupId>\\s-*<artifactId>lombok</artifactId>\\s-*<version>\\$*{\\([^}]+\\)}</version>" nil t)
                     (let* ((property-name (match-string 1))
                            (property-pattern (format "<%s>\\([^<]+\\)</%s>" property-name property-name)))
                       (goto-char (point-min))
                       (when (re-search-forward property-pattern nil t)
                         (match-string 1)))))))))))))

;;;###autoload
(defun +my-lsp-java-get-lombok-jar-path ()
  "获取 Lombok JAR 包的完整路径。

从 Maven 本地仓库中查找 Lombok JAR 文件。
版本号优先从项目 pom.xml 中获取，如果未找到则使用默认版本 1.18.20。

返回示例：D:/apache-maven-3.6.3/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar

如果 JAR 文件不存在则返回 nil。"
  (let* ((local-repo (+my-lsp-java-get-maven-local-repo))
         (version (or (+my-lsp-java-get-lombok-version-from-pom)
                      "1.18.20"))
         (lombok-jar (format "org/projectlombok/lombok/%s/lombok-%s.jar" version version))
         (full-path (expand-file-name lombok-jar local-repo)))
    (if (file-exists-p full-path)
        (progn
          (message "LSP: 使用 Lombok JAR %s (版本: %s)" full-path version)
          full-path)
      (progn
        (message "Warning: Lombok JAR 不存在: %s" full-path)
        nil))))

;;;###autoload
(defun +my-lsp-java-build-vmargs ()
  "构建 LSP Java 服务器的 JVM 参数。

返回一个字符串列表，包含：
- 基本内存设置（-Xmx2G 最大堆内存，-Xms100m 初始堆内存）
- 编码设置（UTF-8）
- Java 17+ 模块系统开放设置（允许 Lombok 和 JDT LS 的反射访问）
- Lombok agent（如果找到 JAR 包）

这些参数会被传递给 eclipse.jdt.ls（Eclipse Java Language Server）。"
  (let ((lombok-jar (+my-lsp-java-get-lombok-jar-path)))
    (append
     (list
      "-Xmx2G"                      ;; 最大堆内存 2GB
      "-Xms100m"                    ;; 初始堆内存 100MB
      "-Dfile.encoding=UTF-8"       ;; 文件编码 UTF-8

      ;; Java 9+ 模块系统：开放必要的包给反射访问
      ;; 这是 Lombok 和 JDT LS 正常工作所必需的
      ;; Lombok 需要在编译时修改 AST，需要反射访问 Java 内部类
      "--add-opens=java.base/java.lang=ALL-UNNAMED"
      "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED"
      "--add-opens=java.base/java.io=ALL-UNNAMED"
      "--add-opens=java.base/java.net=ALL-UNNAMED"
      "--add-opens=java.base/java.nio=ALL-UNNAMED"
      "--add-opens=java.base/java.util=ALL-UNNAMED"
      "--add-opens=java.base/jdk.internal.loader=ALL-UNNAMED")

     ;; 如果找到 Lombok JAR，添加 javaagent 参数
     ;; javaagent 是 Java 的字节码增强机制，Lombok 通过它实现注解处理
     (when lombok-jar
       (list (format "-javaagent:%s" lombok-jar))))))

(provide 'lsp-helper)
