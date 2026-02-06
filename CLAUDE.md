# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration directory (`.doom.d`) that provides a VSCode-like IDE experience with comprehensive LSP support, particularly optimized for Java and Python development on Windows.

## Key Commands

### Doom Emacs Management
```bash
# Apply configuration changes (run after modifying init.el or packages.el)
doom sync

# Upgrade Doom and packages
doom upgrade

# Run diagnostic to check for issues
doom doctor

# Reload Emacs configuration
doom/reload
```

### Testing

运行单元测试（在 `.doom.d` 目录下）：

```bash
# 进入配置目录
cd ~/.doom.d  # Linux/Mac
cd %USERPROFILE%\.doom.d  # Windows CMD
cd $env:USERPROFILE\.doom.d  # Windows PowerShell

# 运行测试
emacs -batch -l test-lsp-config.el -f ert-run-tests-batch-and-exit
```

**测试输出特点**：
- 每个测试显示功能说明和测试步骤
- 输出当前测试的方法名和实际功能
- 显示环境变量值（如 HOME、JAVA_HOME）
- 显示测试结果和中间过程
- 14个单元测试覆盖：
  - Java 路径解析（JAVA_HOME）
  - Maven 配置检测（MAVEN_HOME、M2_HOME）
  - Maven settings.xml 解析
  - Lombok 版本提取（pom.xml 直接版本和属性引用）
  - JVM 参数构建

### LSP Diagnostics
Within Emacs:
```
M-x +my-lsp-java-diagnostic    # Display LSP Java configuration diagnostics
```

## Architecture

### Core Configuration Files

1. **init.el** - Doom module configuration
   - Defines which Doom modules are enabled
   - Key enabled modules: `(java +lsp +tree-sitter)`, `(python +lsp +pyright +tree-sitter)`, `(lsp +peek)`, `(debugger +lsp)`
   - Uses `corfu` for completion, `vertico` for searching
   - Enables tabs, treemacs, popup management, workspaces

2. **config.el** - User configuration and customization
   - Loads LSP configuration via `(load! "+lsp-config")`
   - Windows path handling fixes (lines 78-89)
   - UTF-8 encoding settings
   - VSCode-like popup/buffer management system (lines 107-244)
   - Window management keybindings

3. **packages.el** - Package declarations
   - Currently uses default Doom packages only
   - Add custom packages here using `package!` macro

### LSP Configuration System

The LSP setup is modular, testable, and consists of three main files:

1. **lsp-config.el** (loaded via `+lsp-config` in config.el)
   - Main LSP configuration entry point
   - Configures `lsp-mode`, `lsp-ui`, `lsp-java`, `lsp-pyright`, `lsp-treemacs`
   - Uses environment variables: `JAVA_HOME`, `MAVEN_HOME`, `M2_HOME`
   - Auto-starts LSP for Java projects with `pom.xml` (1 second delay)
   - Dynamically resolves Lombok version from project `pom.xml`

2. **lsp-helper.el** (loaded via `+lsp-helper` in lsp-config.el)
   - **Pure Elisp implementation** - no Doom macros, fully testable
   - **Uses environment variables** - `HOME`, `JAVA_HOME`, `MAVEN_HOME`/`M2_HOME`
   - **Path expansion mechanism** - Explicitly converts `~` to `$HOME` absolute path to avoid resolution failures
   - Contains 8 helper functions with detailed documentation:
     - `+my-lsp-expand-tilde-path`: Converts `~/path` to `$HOME/path` absolute path (more reliable than Emacs' `expand-file-name` in some cases)
     - `+my-lsp-java-get-java-path`: Resolves Java executable from `JAVA_HOME` (with Windows/Unix compatibility)
     - `+my-lsp-java-get-maven-home`: Gets Maven home from `MAVEN_HOME` or `M2_HOME`
     - `+my-lsp-java-parse-maven-settings`: Parses settings.xml for `<localRepository>` (uses `$HOME` for default path)
     - `+my-lsp-java-get-maven-local-repo`: Gets Maven local repo (checks global/user settings.xml, defaults to `$HOME/.m2/repository`)
     - `+my-lsp-java-get-lombok-version-from-pom`: Extracts Lombok version from pom.xml (supports both direct version and `${property}` references)
     - `+my-lsp-java-get-lombok-jar-path`: Constructs full path to Lombok JAR in local repo
     - `+my-lsp-java-build-vmargs`: Builds JVM arguments including memory settings, UTF-8 encoding, Java module opens, and Lombok javaagent

3. **test-lsp-config.el**
   - 14 unit tests using ERT (Emacs Lisp Regression Testing)
   - **Detailed logging**: Each test prints function name, description, test steps, and results
   - **Environment variable display**: Shows `HOME`, `JAVA_HOME` values during tests
   - Cross-platform compatible (uses relative paths via `load-file-name`)
   - Tests cover: environment variable parsing, Maven settings parsing, pom.xml parsing (both direct and property versions), JVM args construction
   - Run independently without starting Emacs GUI
   - Uses `deftest-with-log` macro for automatic test logging

### Popup/Buffer Management

This config implements a VSCode-like panel system using Doom's `+popup` module:

- **Bottom panels**: LSP logs, compilation output, grep results, terminal (vterm)
- **Right panels**: Help, documentation, Magit
- **Keybindings**:
  - `C-\`` (Ctrl+backtick): Toggle popup
  - `C-<`: Previous popup
  - `C->`: Next popup
  - `SPC t p`: Toggle last popup
  - `SPC t n/N`: Cycle popups forward/backward

Configuration uses both `+popup-defaults` (Doom's system) and `display-buffer-alist` (Emacs native) for maximum compatibility.

## Environment Requirements

### Required Environment Variables

- **JAVA_HOME**: Path to JDK installation (e.g., `D:/program/openjdk-17.0.13`)
  - Must point to a valid JDK directory containing `bin/java.exe` (Windows) or `bin/java` (Unix)

- **MAVEN_HOME** or **M2_HOME**: Path to Maven installation (e.g., `D:/apache-maven-3.6.3`)
  - Used to locate Maven settings.xml and local repository
  - Falls back to `$HOME/.m2/repository` if not set

- **HOME**: User home directory (typically set by system)
  - Used to locate `$HOME/.m2/settings.xml` and `$HOME/.m2/repository`
  - All `~` paths in the code use `$HOME` environment variable for cross-platform compatibility

### Maven Configuration

The LSP Java configuration automatically:
1. Reads Maven settings.xml from `$MAVEN_HOME/conf/settings.xml` or `~/.m2/settings.xml`
2. Extracts local repository path (or uses default `~/.m2/repository`)
3. Reads project's pom.xml to get Lombok version (supports both direct version and property references like `${lombok.version}`)
4. Constructs Lombok JAR path and adds it as Java agent to LSP server

## Path Resolution Mechanism

**Problem**: The `~` symbol can fail to resolve in certain environments (especially Windows or custom shells).

**Solution**: Explicit path expansion using `$HOME` environment variable:

```elisp
;; Before: May fail in some environments
~/.m2/repository

// After: Converted to absolute path via +my-lsp-expand-tilde-path
C:/Users/username/.m2/repository  (Windows)
/home/username/.m2/repository     (Linux)
```

**How it works**:
1. Code uses `~/.m2/repository` notation (readable, portable)
2. `+my-lsp-expand-tilde-path` function converts `~` to `$HOME` absolute path
3. All file operations use the absolute path (more reliable)

**Applied in**:
- `+my-lsp-java-parse-maven-settings`: Default repo path `~/.m2/repository`
- `+my-lsp-java-get-maven-local-repo`: User settings `~/.m2/settings.xml`
- Maven settings.xml parsing: `<localRepository>~/custom/path</localRepository>`

## Windows-Specific Considerations

1. **Path Handling**: Projectile uses native indexing (`projectile-indexing-method 'native`) to avoid issues with spaces in Windows paths (config.el:78-89)

2. **File Watching**: LSP file watchers are disabled (`lsp-enable-file-watchers nil`) to improve performance on Windows (lsp-config.el:27)

3. **Java Paths**: Automatically detects `java.exe` on Windows vs `java` on Unix (lsp-helper.el:24-26)

## How LSP Auto-Start Works

When opening a Java file (java-mode-hook):
1. Checks if current file is in a Projectile project
2. Verifies project root contains `pom.xml`
3. If Maven project detected, waits 1 second (to avoid slowing Emacs startup)
4. Starts LSP and begins code analysis
5. Initial analysis may take several seconds to minutes depending on project size

## File Organization

The configuration follows a clean, modular structure:

```
.doom.d/
├── init.el              # Doom module declarations
├── config.el            # Main user configuration
├── packages.el          # Package declarations
├── lsp-config.el        # LSP configuration (after! blocks)
├── lsp-helper.el        # Pure Elisp helper functions (testable)
├── test-lsp-config.el   # Unit tests (14 tests with detailed logging)
└── CLAUDE.md            # This file
```

**Notes**:
- Previous backup files (`lsp-helper-new.el`, `lsp-helper-new2.el`, `lsp-config.el.bak2`) have been removed
- Test runner script (`run-lsp-tests.sh`) has been removed - use the command in Testing section instead

## File Loading Order

```
init.el (Doom modules) → config.el (user config) → +lsp-config → +lsp-helper
                                                  ↓
                                            lsp-config.el
                                            (configures LSP after package loads)
                                                  ↓
                                            lsp-helper.el
                                            (pure Elisp functions)
```

The `(load! "file")` macro loads files relative to `.doom.d/` directory. The `+` prefix convention indicates config modules.

---

## Development Workflow & Standards

This section defines the coding standards and workflow patterns to follow when working on this configuration.

### Core Principles

1. **Modular Configuration** - Each feature/configuration should be in separate files
2. **Testable Utilities** - All helper functions must have unit tests
3. **Detailed Logging** - Configuration loading and runtime must be traceable via logs
4. **Documentation First** - Every function/major block must have clear comments

### File Organization Standards

**File Naming Convention**:
- Use `+` prefix for config modules: `+lsp-config.el`, `+lsp-helper.el`
- Test files: `test-<module>.el` (no `+` prefix)
- File header: `;;; +filename.el -*- lexical-binding: t; no-byte-compile: t; -*-`

**Why `no-byte-compile: t`?**
- Doom configs use many macros (`after!`, `add-hook!`, `load!`) that may not be available during compilation
- Byte compilation can cause "void-variable" errors (like you saw with `lsp-mode`)
- Source files load only milliseconds slower - negligible for config files
- Prevents cache issues where .elc and .el are out of sync

### Configuration File Structure

Each config file should follow this pattern:

```elisp
;;; +module-name.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Module Description - Brief overview
;; =========================================
;;
;; Detailed explanation of what this file does and how it works.
;;

(message "[+module-name] Starting to load...")

;; ============================================================================
;; Section 1 - Helper Functions
;; ============================================================================

(defun +my-module-helper-function (arg)
  "One-line summary of what this does.

More detailed explanation if needed.

ARG: Description of argument"
  ;; Implementation
  )

;; ============================================================================
;; Section 2 - Configuration
;; ============================================================================

(after! some-package
  (message "[+module-name] Configuring some-package...")
  ;; Configuration code
  (message "[+module-name] ✓ some-package configured"))

(message "[+module-name] ✓ Module loaded")
```

### Testing Standards

**Every helper function must have a unit test.**

#### Test File Structure

```elisp
;;; test-module-name.el -*- lexical-binding: t; -*-
;;
;; Tests for +module-name.el
;; =========================================

(require 'ert)

;; Load the module being tested
(let ((module-file (expand-file-name "+module-name.el"
                                    (file-name-directory
                                     (or load-file-name
                                         buffer-file-name)))))
  (message "[测试准备] 加载模块: %s" module-file)
  (load module-file))

;; ============================================================================
;; Helper Macro for Test Logging
;; ============================================================================

(defmacro deftest-with-log (name description &rest body)
  "Define test with automatic logging."
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
;; Test Cases
;; ============================================================================

(deftest-with-log +test-module-function/normal-case
  "Test description of normal case"
  (message "[测试步骤] Describe what you're testing")
  (let ((result (+my-module-function "test-input")))
    (message "[测试结果] %s" result)
    (should (stringp result))))

(provide 'test-module-name)

;; Auto-run when executed directly
(when (boundp 'batch-exit-code)
  (message "")
  (message "========================================")
  (message "单元测试")
  (message "========================================")
  (message "")
  (ert-run-tests-batch-and-exit))
```

#### Test Coverage Rules

1. **One test per function** - Only test the normal/happy path
2. **No edge case testing** - For config file parsers, just verify it works
3. **No branch testing** - Don't test every possible combination
4. **Focus on functionality** - Answer: "Does this function do what it's supposed to do?"

**Rationale**: Config functions are simple - if the config is correct, it works. If not, the user fixes their config. We don't need to test error handling.

**Example**:
```elisp
// Good: Test normal operation
(deftest-with-log +test-parse-config/valid-file
  "Parse valid config file"
  (let ((result (my-parse-config "valid-config.xml")))
    (should (stringp result))))

// Bad: Testing all edge cases
(deftest-with-log +test-parse-config/missing-file
  "Handle missing file")  // Don't test this
(deftest-with-log +test-parse-config/malformed-xml
  "Handle malformed XML")  // Don't test this either
```

#### Running Tests

```bash
cd ~/.doom.d
emacs -batch -l test-module-name.el -f ert-run-tests-batch-and-exit
```

Tests run without starting Emacs GUI - fast and scriptable.

### Logging Standards

All configuration and runtime code must have detailed logging for debugging.

#### Log Message Format

Use `[Category]` prefix to identify log source:

```elisp
;; Configuration loading
(message "[+module-name] Starting to load...")
(message "[+module-name] ✓ Loaded successfully")

;; Runtime checks
(message "[LSP 自动启动] 开始检查 Java 项目")
(message "[检查 1/4] Projectile 是否可用")
(message "  ✓ Projectile 已加载")

;; Error handling
(message "[Module Name] ✗ Failed to load: %s" error-message)
```

#### Log Levels

- **✓** - Success: `[Module] ✓ Operation completed`
- **✗** - Failure: `[Module] ✗ Operation failed: reason`
- **→** - Progress: `[Module] → Processing item...`
- **[ ]** - Checkbox: `[检查 1/3] Description`

#### Multi-Step Logging

For complex operations, break down into numbered steps:

```elisp
(message "")
(message "========================================")
(message "[LSP 自动启动] 开始检查 Java 项目")
(message "========================================")
(message "[当前文件] %s" file-path)

(message "[检查 1/4] Projectile 是否可用")
(if projectile-available
    (message "  ✓ Projectile 已加载")
  (message "  ✗ Projectile 未加载"))

(message "[检查 2/4] 当前是否在 Projectile 项目中")
;; ... more checks
(message "========================================")
```

This makes it easy to:
- Track execution flow
- Identify where something fails
- Understand what the code is doing

#### When to Add Logs

**Required**:
- Configuration file loading: `[+module] Loading...`
- Hook registration: `[+module] Registering hooks...`
- Auto-start checks: `[Module] Checking conditions...`
- Major state changes: `[Module] ✓ Started`

**Optional** (but recommended):
- Environment variable values: `[Module] JAVA_HOME=%s`
- File paths being checked: `[Module] Checking: /path/to/file`
- Intermediate results: `[Module] Parsed value: %s`

### Hook Registration Rules

**CRITICAL**: Always register hooks inside `after!` blocks to ensure the module is loaded first.

**Wrong** ❌:
```elisp
;; At top level - may fail if java-mode not defined yet
(add-hook! 'java-mode-hook #'+my-function)
```

**Correct** ✅:
```elisp
(after! java
  (message "[+module-name] Registering hooks...")
  (add-hook! 'java-mode-hook #'+my-function)
  (message "[+module-name] ✓ Hooks registered"))
```

**Why?**
- Hooks like `java-mode-hook` are only defined after the java module loads
- `after! java` ensures the module is fully loaded before registering
- Prevents "void-variable" errors

### Code Quality Checklist

Before committing changes, verify:

- [ ] File has `no-byte-compile: t` in header
- [ ] All functions have docstrings
- [ ] Helper functions have corresponding tests
- [ ] Tests can run in batch mode: `emacs -batch -l test-file.el -f ert-run-tests-batch-and-exit`
- [ ] Configuration loading has logs: `[+module] Loading...`
- [ ] Hook registration uses `after!` blocks
- [ ] Multi-step operations have numbered checkpoints: `[1/3]`, `[2/3]`, etc.
- [ ] Error messages use `✗` prefix and include context
- [ ] Success messages use `✓` prefix

### Debugging Workflow

When something doesn't work:

1. **Check Messages buffer**: `SPC : view-echo-area-messages`
2. **Look for log prefixes**: Search for `[Category]` in logs
3. **Run tests**: `cd ~/.doom.d && emacs -batch -l test-*.el -f ert-run-tests-batch-and-exit`
4. **Use diagnostic commands**: `M-x +my-lsp-java-diagnostic` (if available)
5. **Check hook registration**: Look for `[Module] ✓ Hooks registered` message

### Summary Workflow

1. **Write feature** → Create modular config file with detailed comments
2. **Add tests** → One test per helper function, test normal path only
3. **Add logs** → Detailed logging with `[Category]` prefixes
4. **Test locally** → Run batch tests without starting Emacs
5. **Verify in Emacs** → Restart and check Messages buffer
6. **Commit** → Ensure all quality checks pass

This ensures:
- ✅ Code is modular and maintainable
- ✅ Functions are tested and verified
- ✅ Issues are easy to debug via logs
- ✅ No need to start Emacs to verify functionality
