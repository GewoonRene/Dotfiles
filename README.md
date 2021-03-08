# 🟢 - DotFiles
These are my dot-files for programs that I actively use.

### Requirements
* Git
* nvim v0.4.4+
* Emacs 24.5+
* tmux 1.8+
* zsh 5.0.2+ (with oh-my-zsh)

### Emacs

<strong> How to fix the header file issue in c-mode? <strong/> <br/>
First go to the root of the project and create a local directory file:
```
M-x add-dir-local-variable RET c-mode RET RET
```
Then place this code inside the file, with the path to the library:
```
((c-mode . ((eval . (setq flycheck-clang-include-path
    (list
    (expand-file-name "library_dir_here")))))))
```
