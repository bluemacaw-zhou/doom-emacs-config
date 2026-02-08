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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

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

;; --- Windows 路径空格问题修复 ---
(when (eq system-type 'windows-nt)
  ;; 修复 Projectile 在 Windows 下的路径问题
  ;; 强制使用 native indexing，避免调用外部命令导致的路径空格问题
  (after! projectile
    (setq projectile-indexing-method 'native
          projectile-enable-caching t))

  ;; 使用 cmdproxy 而不是 bash，避免路径空格问题
  ;; 或者注释掉这行，让 Emacs 使用默认的 shell
  ;; (setq shell-file-name "cmdproxy")
  )

;; 全局编码修正 - 确保 UTF-8 编码在所有情况下正确处理
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ============================================================================
;; Emacs 环境变量配置 - 不影响系统环境
;; ============================================================================
;; 在 +env.el 中定义 Emacs 专用的环境变量（JAVA_HOME、MAVEN_HOME 等）
;; 这些变量只在 Emacs 内生效，保持系统环境变量干净
;;
(load! "+env")

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


;; === Buffer 管理 - 类似 VSCode 的面板控制 ===

;; Doom 的 +popup 配置（替代外部 popper 包）
(after! popup
  ;; 配置 popup buffer 的显示规则
  (setq +popup-defaults
        '(;; LSP 相关 buffer 在底部
          ("^\\*lsp-log\\*"               :slot 0 :side bottom :height 0.3)
          ("^\\*jdtls::stderr\\*"         :slot 1 :side bottom :height 0.3)
          ("^\\*lsp-diag\\*"              :slot 2 :side bottom :height 0.3)
          ("^\\*lsp-ui-doc\\*"            :slot 3 :side bottom :height 0.3)

          ;; 编译相关 buffer
          ("^\\*compilation\\*"           :slot 4 :side bottom :height 0.25)
          ("^\\*Compile-Log\\*"           :slot 5 :side bottom :height 0.25)

          ;; Grep 结果
          ("^\\*grep\\*"                  :slot 6 :side bottom :height 0.3)

          ;; 终端
          ("^\\*vterm\\*"                 :slot 7 :side bottom :height 0.4)

          ;; 帮助 buffer 在右侧
          ("^\\*Help\\*"                  :slot 0 :side right :width 80)
          ("^\\*Man\\*"                   :slot 1 :side right :width 80)
          ("^\\*info\\*"                  :slot 2 :side right :width 80)

          ;; Magit 在右侧
          ("^\\*magit"                    :slot 3 :side right :width 0.4)

          ;; 其他常用 buffer
          ("^\\*Messages\\*"              :slot 8 :side bottom :height 0.3)
          ("^\\*Warnings\\*"              :slot 9 :side bottom :height 0.3)
          ("^\\*Buffer List\\*"           :slot 10 :side bottom :height 0.3)
          ("^\\*undo-tree\\*"             :slot 11 :side bottom :height 0.3)
          ("^\\*Dict\\*"                  :slot 12 :side bottom :height 0.3)
          ("^\\*kill-ring\\*"             :slot 13 :side bottom :height 0.3)
          ("^\\*register\\*"              :slot 14 :side bottom :height 0.3)
          ("^\\*debug\\*"                 :slot 15 :side bottom :height 0.3)
          ("^\\*Calendar\\*"              :slot 16 :side bottom :height 0.3)
          ("^\\*Org Agenda\\*"            :slot 17 :side bottom :height 0.3)))

  ;; 配置 popup 参数
  (setq +popup-reference-height 25    ;; popup 默认高度
        +popup-reference-width 80     ;; popup 默认宽度
        +popup-margin-width 1)        ;; popup 边距

  ;; 自动关闭旧的 popup
  (setq +popup-close-old-on-popup t))

;; 配置 display-buffer-alist 作为补充
(after! doom
  (setq display-buffer-alist
        `(;; LSP 相关 buffer 在底部
          ("^\\*lsp-log\\*"               (display-buffer-in-side-window)
                                           (window-height . 0.3)
                                           (side . bottom)
                                           (slot . 0)
                                           (window-parameters . ((no-other-window . t))))

            ("^\\*jdtls::stderr\\*"         (display-buffer-in-side-window)
                                           (window-height . 0.3)
                                           (side . bottom)
                                           (slot . 1)
                                           (window-parameters . ((no-other-window . t))))

            ("^\\*lsp-diag\\*"              (display-buffer-in-side-window)
                                           (window-height . 0.3)
                                           (side . bottom)
                                           (slot . 2)
                                           (window-parameters . ((no-other-window . t))))

            ;; 编译相关 buffer 在底部
            ("^\\*compilation\\*"           (display-buffer-in-side-window)
                                           (window-height . 0.25)
                                           (side . bottom)
                                           (slot . 3))

            ("^\\*Compile-Log\\*"           (display-buffer-in-side-window)
                                           (window-height . 0.25)
                                           (side . bottom)
                                           (slot . 4))

            ;; 帮助 buffer 在右侧
            ("^\\*Help\\*"                  (display-buffer-in-side-window)
                                           (window-width . 80)
                                           (side . right)
                                           (slot . 0))

            ("^\\*Man\\*"                   (display-buffer-in-side-window)
                                           (window-width . 80)
                                           (side . right)
                                           (slot . 1))

            ("^\\*info\\*"                  (display-buffer-in-side-window)
                                           (window-width . 80)
                                           (side . right)
                                           (slot . 2))

            ;; Grep 结果在底部
            ("^\\*grep\\*"                  (display-buffer-in-side-window)
                                           (window-height . 0.3)
                                           (side . bottom)
                                           (slot . 5))

            ;; Magit 在右侧
            ("magit:.*"                     (display-buffer-in-side-window)
                                           (window-width . 0.4)
                                           (side . right)
                                           (slot . 3))

            ;; 终端在底部
            ("^\\*vterm\\*"                 (display-buffer-in-side-window)
                                           (window-height . 0.4)
                                           (side . bottom)
                                           (slot . 6)))))

;; 窗口管理快捷键
(map! :leader
      :desc "Delete other windows" "w D" #'doom/kill-other-buffers-and-windows
      :desc "Switch to last buffer" "b l" #'evil-switch-to-windows-last-buffer
      :desc "Maximize window" "w m" #'doom/window-maximize-buffer
      :desc "Balance windows" "w =" #'balance-windows
      :desc "Rotate windows forward" "w W" #'doom/window-rotate-forward
      :desc "Rotate windows backward" "w R" #'doom/window-rotate-backward)

;; Doom 内置的 popup 快捷键
(map! :leader
      :prefix ("t" . "toggle/popup")
      :desc "Toggle last popup" "p" #'+popup/toggle
      :desc "Cycle popup forward" "n" #'+popup/raise
      :desc "Cycle popup backward" "N" #'+popup/other)

;; 全局快捷键（类似 VSCode）
(map! :map 'global
      "C-`" #'+popup/toggle           ;; Ctrl + ` 切换面板
      "C-<" #'+popup/other            ;; Ctrl + < 切换到上一个
      "C->" #'+popup/raise)            ;; Ctrl + > 切换到下一个
