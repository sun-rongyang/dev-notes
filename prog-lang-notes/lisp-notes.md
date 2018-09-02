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



## Basic syntax and operation

### List and Atom

For Lisp, we only have two elementary structures: **list** and **atom**.

- Lists are surrounded by parentheses, like

  ```lisp
  (1 2 3)
  ()
  ((a b) c)
  (+ 1 2)
  ```

- Atoms are separated by whitespace or parentheses, like

  ```lisp
  1	; A number
  a	; A symbol
  b-3
  @foo
  ```

  Note: `b-3` is one atom, not a arithmetic express. Except **number**, other atoms are **symbol**.

### Form and Evaluation

- **Form** can be either an atom or list. 

- **Evaluation** means get its value.

- Form is meant to be evaluated.

  - If a form is an atom, Lisp treat is as a name and try to return its value.

    - **Number** is self-evaluating atom.

        ```lisp
        * 1 ; A number is an atom.
        1	; Number is self-evaluating, i.e. return ifself.
        ```

    - **keyword** is self-evaluating atom. Keyword is a symbol which start from a `:`.

      ```lisp
      * :foo
      :FOO
      ```

    - Except keywords, there are only two symbols which is self-evaluating, true `T` and false `NIL`.

      ```lisp
      * t
      T
      *nil
      NIL
      ```

      

  - If a form is list, treats the form as a function call. The first element is as the name of the function. The remaining elements are _arguments_.

    ```lisp
    * (+ 1 2)
    3
    ```

  - Lisp first evaluates the arguments (from left to right), then evaluates the first element to get its function, then applies the function to the argument. (Special forms and macros can change argument evaluation.)

    ```lisp
    * (+ (- 2 1) (* 3 4))
    ; First: (+ 1 (* 3 4))
    ; Second: (+ 1 12)
    ; Third: 13
    13
    ```