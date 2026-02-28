;;; +windows.el -*- lexical-binding: t; -*-

;; Windows 专属配置。

;; 内部默认保持 UTF-8，同时为 Windows 工具输出做兼容。
(setq locale-coding-system 'gbk)
(setq default-process-coding-system '(gbk . gbk))

(add-hook 'compilation-mode-hook
          (lambda ()
            (setq buffer-file-coding-system 'gbk)))

;; 对仍依赖外部命令的场景，强制使用 Git 自带 GNU 工具。
(setq find-program "C:/PROGRA~1/Git/usr/bin/find.exe")
(setq grep-program "C:/PROGRA~1/Git/usr/bin/grep.exe")
(setq xargs-program "C:/PROGRA~1/Git/usr/bin/xargs.exe")

(let ((git-usr-bin "C:/PROGRA~1/Git/usr/bin"))
  (unless (member git-usr-bin exec-path)
    (add-to-list 'exec-path git-usr-bin)
    (setenv "PATH" (concat git-usr-bin path-separator (getenv "PATH")))))

;; Git Bash 工具优先 UTF-8；编译场景再局部切回 GBK。
(setq locale-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

(defun +my-use-gbk-for-compilation ()
  "Windows 原生命令（如 mvn/javac）在编译时使用 GBK 进程编码。"
  (set (make-local-variable 'locale-coding-system) 'gbk)
  (set (make-local-variable 'buffer-process-coding-system) '(gbk . gbk)))

(add-hook 'compilation-mode-hook #'+my-use-gbk-for-compilation)

(after! projectile
  (setq projectile-indexing-method 'native
        projectile-enable-caching t))

(defun +my-projectile-rebuild-cache ()
  "清理并重建 Projectile 缓存。"
  (interactive)
  (when (fboundp 'projectile-invalidate-cache)
    (projectile-invalidate-cache nil))
  (when (fboundp 'projectile-cleanup-known-projects)
    (projectile-cleanup-known-projects))
  (when-let (root (ignore-errors (projectile-project-root)))
    (projectile-add-known-project root))
  (message "[Projectile] 缓存已重建，请重试 SPC p p / SPC p f"))

;; 防止误打开 Windows 保留设备名（CON/PRN/AUX/NUL 等）。
(defun +my--windows-device-name-p (path)
  (let ((name (downcase (file-name-nondirectory (directory-file-name path)))))
    (string-match-p
     (rx bos
         (or "con" "prn" "aux" "nul"
             (seq "com" (in "1-9"))
             (seq "lpt" (in "1-9")))
         (opt "." (* any))
         eos)
     name)))

(defun +my--guard-find-file-windows-device (orig-fn filename &rest args)
  (if (+my--windows-device-name-p filename)
      (user-error "已拦截 Windows 保留设备名: %s" filename)
    (apply orig-fn filename args)))

(unless (advice-member-p #'+my--guard-find-file-windows-device #'find-file)
  (advice-add 'find-file :around #'+my--guard-find-file-windows-device))

(message "[配置] Windows 专属配置已加载")
