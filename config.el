;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; 字体配置 - 根据需要调整 :size 参数
(setq doom-font (font-spec :family "Consolas" :size 16)
      doom-variable-pitch-font (font-spec :family "Arial" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox)  
(setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-monokai-pro)
;; (setq doom-theme 'doom-tokyo-night)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ============================================================================
;; 编码配置 - 解决中文乱码问题
;; ============================================================================
;; Windows 下需要特别处理进程输出编码，因为：
;; - Emacs 内部使用 UTF-8
;; - Windows 命令行工具（cmd, mvn, javac）默认输出 GBK/CP936
;; - 需要正确配置进程编码解码方式

;; 1. Emacs 内部编码设置
(prefer-coding-system 'utf-8)           ; Emacs 内部优先使用 UTF-8
(set-selection-coding-system 'utf-8)   ; 剪贴板使用 UTF-8

;; 2. 进程输出编码设置（关键！）
(when (eq system-type 'windows-nt)
  ;; Windows 下外部进程（mvn, javac 等）输出是 GBK 编码
  ;; 设置进程输出用 GBK 解码，输入用 GBK 编码
  (setq locale-coding-system 'gbk)
  (setq default-process-coding-system '(gbk . gbk))

  ;; 针对 compilation-mode 专门设置
  (add-hook 'compilation-mode-hook
            (lambda ()
              (setq buffer-file-coding-system 'gbk))))

 ;; ============================================================================
  ;; LSP 配置 - 加载独立配置文件
  ;; ============================================================================
  ;; - 从环境变量获取 Java 路径（JAVA_HOME）
  ;; - 从 Maven settings.xml 获取本地仓库路径（MAVEN_HOME）
  ;; - 从项目 pom.xml 动态获取 Lombok 版本
  ;; - 自动启动 Java 项目的 LSP 分析
  ;; - 诊断命令：M-x +my-lsp-java-diagnostic
  ;;

  (message "")
  (message "========================================")
  (message "[配置加载] 开始加载 LSP 配置...")
  (message "========================================")
  (load! "+lsp-config")
  (message "[配置加载] ✓ LSP 配置加载完成")
  (message "========================================")
  (message "")
