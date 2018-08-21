# Lisp notes

## Installation

### On Mac OS X

- Install `sbcl` (Steel Bank Common Lisp)

  ```bash
  $ brew install sbcl
  ```

- Install Common Lisp package manager `Quicklisp`

  ```bash
  $ curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
  $ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
         --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
         --quit
  $ rm /tmp/ql.lisp
  ```

  This will install `Quicklisp` to the `~/.quicklisp` directory. Then create (if not exist) the `rc` file of `sbcl`

  ```bash
  $ touch ~/.sbclrc
  ```

  Finally, let `sbcl` load `Quicklisp` every start time

  ```bash
  $ sbcl
  * (load #p".quicklisp/setup.lisp")
  
  T
  * ( ql:add-to-init-file)
  I will append the following lines to #P"/Users/thorough/.sbclrc":
  
    ;;; The following lines added by ql:add-to-init-file:
    #-quicklisp
    (let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                           (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
  
  Press Enter to continue.
  ```

  where `*` is the command prompt in `sbcl` like `>>>` in `python`. 

## Develop environment

### For vim

I am a vim user, I use [vlime](https://github.com/l04m33/vlime) as REPL(Real Eval Print Loop). But my SBCL+MacVim+vlime runs slowly for REPL. By the way, let's see how to use them.

- I use [Vundle](https://github.com/VundleVim/Vundle.Vim) to manage my plugins, add `Plugin 'l04m33/vlime', {'rtp': 'vim/'}` to the `vimrc` file and type `:PluginInstall` to install the plugin. Then type the command in the terminal to start vlime server.

  ```bash
  $ sbcl --load ~/.vim/bundle/vlime/lisp/start-vlime.lisp
  ```

  For the first time, vlime will download dependents using `Quicklisp`.

- Quick start. 

  - Type `:help vlime-tutor` to see a tutorial.
  - Use vim open a lisp source file.
  - Type `\rr` in normal mode to start `vlime` server. Type `\cs` to list current servers list and switch server. Type `\cd` to disconnect and stop a server.
  - Type `\i` in normal mode to enter the vlime interactive mode to start REPL.
  - Edit a Lisp expression and keep the cursor in the expression. (If another expression is inside this expression, the `vlime` will evaluate the nearest expression.) Then press `<CR>` to send the expression to the server and receive the return value.

## Hello, World!

Setup Common Lisp REPL

```bash
$ sbcl
*
```

Then input

```bash
* (format t "Hello, World!")
```

type `Enter`, you will get

```bash
Hello, World!
NIL
```

To exit the REPL

```bash
* (exit)
```

You can also write a small script and use `sbcl` to run it. Create `hello.lisp` and write down

```lisp
(format t "Hello, World!")
```

To run the script, you can

```bash
$ sbcl --script hello.lisp
Hello, World!
```

