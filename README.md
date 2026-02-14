# Doom Emacs 使用手册

使用范式：**Popup 系统**

核心思想：使用 `SPC o x` 命令打开需要的工具，Doom 自动管理显示位置。

---

## 核心理解：窗口与 Buffer

### 基本概念

- **Window（窗口）**：容器（container），用于显示内容
- **Buffer（缓冲区）**：内容（content），实际的文件、目录、终端等

### 工作模式

```
1. 创建窗口（容器）
   SPC w n

2. 在窗口中显示某个 buffer（内容）
   C-x b        - 切换 buffer
   C-x d        - 打开 dired buffer（目录浏览）
   M-x vterm    - 打开终端 buffer
   SPC f f      - 打开文件 buffer
```

### 关键认知

- 窗口是临时的，buffer 是持久的
- 一个窗口同一时刻只能显示一个 buffer
- 同一个 buffer 可以在多个窗口中显示
- 不需要记住 `SPC f d` 等快捷键，理解底层逻辑即可

---

## 使用场景 → 操作映射

### 查看项目文件目录

```
SPC o p        - 打开 Treemacs（文件目录树）
```

### 打开终端

```
SPC o t        - 打开 vterm（终端）
```

### 查看 Git 状态

```
SPC g g        - 打开 Magit（Git 界面）
```

### 在项目中查找文件

```
SPC p f        - 在项目中查找文件
SPC SPC        - 快速查找文件（同上）
```

### 在项目中搜索内容

```
SPC s p        - 在项目中搜索内容
SPC /          - 快速搜索（同上）
```

### 切换打开的文件

```
SPC b b        - 切换 buffer（快速切换，日常使用）
SPC f r        - 最近打开的文件
```

### 管理 Buffer

```
SPC b i        - 打开 ibuffer（buffer 管理器）
```

**ibuffer 是什么**：
- Buffer 列表管理器，显示所有打开的 buffer
- 可以查看 buffer 状态、大小、mode
- 支持标记、批量操作、过滤

**ibuffer 中的操作**：
```
标记操作：
d              - 标记删除
s              - 标记保存
u              - 取消标记
x              - 执行标记的操作

直接操作：
k              - 直接 kill 当前 buffer
RET/回车       - 切换到该 buffer

过滤：
/ m            - 按 mode 过滤
/ n            - 按名称过滤
```

**最佳实践**：
- 日常切换 buffer：用 `SPC b b`（快速、模糊搜索）
- 删除/管理 buffer：用 `SPC b i`（可视化、批量操作）

### 查看文档

```
SPC h d h      - 查看帮助文档
K              - 查看当前符号的文档（需要 LSP）
```

### 窗口管理

```
C-`            - 切换 popup（Ctrl+反引号）
SPC t p        - 切换上一个 popup
SPC w w        - 在窗口间切换
SPC w c        - 关闭当前窗口
```

---

## 待探索的需求

### 需求 1: 窗口级别的 Buffer 历史切换

**场景描述**：
- 当前窗口显示过多个 buffer（例如：dired → test.txt → notes.txt）
- 想在当前窗口中切换 buffer
- 只想在**这个窗口显示过的 buffer** 中选择
- 不希望看到其他窗口访问过的 buffer 或无关 buffer

**当前限制**：
- `SPC b b` - 显示所有 buffer（包含无关 buffer）
- `SPC b [ ]` - 只能前后切换，无法"选择"
- 缺少"窗口级别的 buffer 历史选择器"

**状态**：待探索 - 不确定需求是否合理，探索中可能发现新的处理角度

### 需求 2: 学习org模式

**场景描述**
- obsidian安装org插件 实现emacs编辑 obsidian多端同步

**当前限制**
- obsidian编写笔记不是很舒服 需要源码模式什么的 不如emacs
- obsidian安装org mode之后 emacs编辑笔记 obsidian + git多端同步
- obsidian整合excalidraw, plantuml这样更好的画图

---

## 后续记录

探索后在此记录：
- 常用操作：
- 需要定制的功能：
