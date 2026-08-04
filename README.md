# paterl - Erlang to Pat transpiler

![Static Badge](https://img.shields.io/badge/version-0.9-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/duncanatt/paterl?color=violet)
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/duncanatt/paterl/build.yml?branch=main&label=tests)


The current type-checking pipeline consists of these stages:

1. `epp:parse_file`: Erlang preprocessor (parsing and macro expansion)
2. `paterl_syntax:module`: Rule out unsupported Erlang syntactic subset (not implemented)
3. `erl_lint:module`: Erlang linting
4. `paterl_ir:module`: Assignment transformation which expands Erlang expressions to match (will implement ANF next)
5. `paterl_types:table`: Extract Erlang `typespec` annotations into a table
6. `paterl_bootstrap:forms`: Modifies the Erlang syntax tree to insert an auxiliary bootstrapping `main` function
7. `paterl_anno:annotate`: Annotates the Erlang syntax tree using the type information in the table obtained from step 5
8. `paterl_trans:module`: Translates the annotated Erlang syntax tree to a Pat syntax tree
9. `pat_prettypr:module`: Prints the Pat syntax tree

## Using from the Erlang shell

```erlang
paterl:compile("src/examples/erlang/codebeam/id_server_demo.erl", [{includes, ["include"]}, {out, "out"}]).
```

## Using from the terminal

```shell
./src/paterl src/examples/erlang/codebeam/id_server_demo.erl -v all -I include
```

---

# Installing the `paterl` toolchain

The following instructions detail how `paterl` can be installed on MacOS, Ubuntu, and Windows.

<!-- Some of the installation steps below are common to different installations and this guide identifies them accordingly by the corresponding operating system icon on which steps should be executed. -->

<!-- <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/apple.svg" width="20" height="20"> <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/ubuntu.svg" width="20" height="20"> <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/windows.svg" width="20" height="20"> -->

## Install the Windows Subsystem for Linux

The simplest and ideal way to install `paterl` on Windows is to use the Windows Subsystem for Linux (WSL).
WSL provides virtualised Linux environment on Hyper-V.
While WSL can access your Windows file system, software installed on the virtualised Linux environment is isolated from and cannot be accessed by Windows, and vice versa.
This means that packages (e.g. Erlang) installed on Windows must also be installed in the virtualised Linux environment to be available to Linux.

1. By default, WSL installs the latest Ubuntu distribution.
   This is done by typing the following on your terminal or PowerShell:

   ```bash
   C:\> wsl --install
   C:\> wsl
   ```

2. After the installation completes, update your Ubuntu virtual environment to the latest packages.

   ```bash
   $ sudo apt update && sudo apt upgrade
   ```

3. You can obtain information about your Ubuntu environment by typing:

   ```bash
   $ lsb_release -a
   ```

### Accessing WSL and Windows directories

WSL can be launched at any point by starting a fresh terminal or PowerShell and typing:

```bash
C:\> wsl
```

This will switch the prompt from `C:\>` to `$:/`. 
To access your Windows directories from a WSL session, change directory to the mounted virtualised Ubuntu partition:

```bash
$ cd /mnt/c/Users/your-username/Desktop
```

## Installing OCaml

We start by installing `opam`, the OCaml package manager.

1. The easiest way to install `opam` on WSL and Ubuntu is via `apt`:

    ```bash
    $ sudo apt install opam
    ```
   
    On macOS, `opam` is installed using Homebrew as follows:

    ```bash
    $ brew install opam
    ```

2. 
   Once `opam` is installed, it needs to be initialised by running the command below as a normal user.
   This will take a few minutes to complete.

   ```bash
   $ opam init -y
   ```
3. Follow the instructions provided at the end of the `opam init` output to complete initialise `opam`.
   Typically, this is done by typing:

   ```bash
   $ eval $(opam env --switch=default)
   ```

   **This step is required whenever you wish to interact with `opam` in a fresh terminal to install or build packages.**

4. Now that `opam` is successfully installed, we can install the common OCaml platform tools.
   These are required to run or develop OCaml applications.

   ```bash
   $ opam install ocaml-lsp-server odoc ocamlformat utop  
   ```

5. Test your OCaml installation by launching `utop`, the universal top-level for OCaml.
   Try typing `21 * 2;;` at the `#` prompt and hit Enter.
   The following output should be displayed:

   ```ocaml
   # 21 * 2;;
   - : int = 42
   ```

   Exit `utop` by typing:

   ```ocaml
   # #quit;;
   ```

## Installing Z3

1. To install Z3 on WSL and Ubuntu, type:

   ```bash
   $ sudo apt install z3
   ```

   On macOS, Z3 is installed using Homebrew as follows:

   ```bash
   $ brew install z3
   ```

2. Test Z3 by invoking it from the terminal.
   It should return the output shown below:

   ```bash
   $ z3
   Error: input file was not specified.
   For usage information: z3 -h
   ```

## Setting up your development directory

Before downloading the necessary components to set up the `paterl` toolchain on your system, create a new directory where all the development source code will be cloned from GitHub.

```bash
$ mkdir Development
$ cd Development
```

Feel free to change the `Development` directory to one that fits your system configuration set-up. 

## Setting up Pat

Pat is the OCaml backend component to `paterl`, implemented by the `pat-lang` project: a type checker 
and interpreter for the Pat programming language. It processes programs written in the Pat language, 
and reports communication errors.
The `paterl` Erlang frontend synthesises Pat programs from Erlang source code and launches the `pat` 
tool in type-checking mode as a shell process, retrieving any errors reported by Pat. These errors are 
post-processed by `paterl` and presented to the user in the form of Erlang errors.

1. `pat-lang` requires OCaml >= 5.2.0.
   Check your active version with `ocaml -version` and, if it is older, create and select a new `opam` switch:

   ```bash
   $ opam switch create 5.2.0
   $ eval $(opam env)
   ```

2. Install the Pat OCaml dependencies.
   These dependencies include the Z3 OCaml bridge, which is used by Pat to solve pattern inclusion constraints to 
   ensure that there are no communication errors in Pat programs.
   The dependencies can be installed via `opam` as shown.

   ```bash
   $ opam install dune menhir ppx_import visitors z3 bag cmdliner eio eio_main
   ```

3. Clone the `pat-lang` GitHub repository:

   ```bash
   $ git clone https://github.com/mailbox-types/pat-lang.git
   ```

4. Build the `pat` tool using the included `Makefile`:

   ```bash
   $ cd pat-lang
   $ make
   ```

   This creates a `pat` symlink in the repository root pointing to the compiled binary.

5. Lastly, test your Pat installation by running one of the many Pat examples:

   ```bash
   $ ./pat -t test/examples/de_liguoro_padovani/future.pat
   ```

   No errors should be reported.
   `pat-lang` also includes an interpreter that runs the program after type checking; the `-t` flag 
   skips interpretation and performs type checking only.

## Installing Erlang/OTP

Our `paterl` frontend requires Erlang/OTP > 26.
The best way to install Erlang/OTP on WSL and Ubuntu is to use the Personal Package Archive (PPA) 
maintained by the RabbitMQ team (the PPA managed by ESL currently seems to be out of date).
The RabbitMQ PPA is valid for Ubuntu 22.04 and 20.04.

1. Add the RabbitMQ PPA to your `apt` installation:

   ```bash
   $ sudo add-apt-repository ppa:rabbitmq/rabbitmq-erlang
   $ sudo apt update
   $ sudo apt install erlang
   ```

   If you have already installed Erlang from the Ubuntu repository, it will be upgraded to the version 
   given in the RabbitMQ PPA.
   
2. Test your installation by launching the Erlang shell:

   ```bash
   $ erl 
   Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

   Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)
   1> 
   ```

3. Quit the shell by typing:

   ```erlang
   1> q().
   ```

### Removing Erlang

In case you need to remove the Erlang version installed from the RabbitMQ PPA and restore it to the version provided 
by the Ubuntu repository, use PPA `purge` as follows:

```bash
$ sudo apt install ppa-purge
$ sudo ppa-purge ppa:rabbitmq/rabbitmq-erlang
```

If you wish to completely remove Erlang, type:

```bash
$ sudo apt remove erlang
```

## Setting up `paterl`

The last step is to set up `paterl`, the Erlang front-end tool used to process Erlang files.

1. Clone the `paterl` Git repository:

   ```bash
   $ git clone https://github.com/duncanatt/paterl.git
   ```

2. Make the `pat` tool visible to `paterl`.
   The `paterl` Erlang frontend invokes the `pat` tool to type check the Pat source code files it synthesises.
   It locates `pat` via the `PATERL_PAT` environment variable when set, and otherwise searches for an executable 
   named `pat` in the directories of `PATH`.
   Point `PATERL_PAT` to the **compiled** `pat` binary (the symlink created by `make` in the `pat-lang` repository root), e.g.

   ```bash
   $ export PATERL_PAT="/path/to/pat-lang/pat"
   ```

   or alternatively add the `pat-lang` repository root to `PATH`.
   The flags `paterl` needs (`-q -j -t`) are supplied automatically: `-q` disables quasilinearity checking and `-j` 
   uses sequential join instead of disjoint combine (both required by the Pat code `paterl` generates), while `-t` 
   performs type checking only, without interpreting the program.

3. Build the `paterl` Erlang front-end using the included `Makefile`:

   ```bash
   $ make
   ```

4. Finally, test your `paterl` installation by running one of the many included examples:

   ```bash
   $ ./src/paterl src/examples/erlang/codebeam/id_server_demo.erl -v all -I include
   ```

   You should see the following output on your terminal:

   ```bash
   [WRITE] Writing temporary Pat file ebin/id_server_demo.
   [PAT] Patt'ing ebin/id_server_demo.
   [PAT] Successfully type-checked ebin/id_server_demo.erl.
   ```

   
## Troubleshooting

- If `pat-lang` fails to build and complains with errors similar to the one below, it means that the `opam` 
environment is not initialised in your active shell.

  ```bash
  $ make
  /bin/sh: 1: dune: not found
  ```

  Reinitialise your `opam` environment using

  ```bash
  $ eval $(opam env --switch=default)
  ```

  and rebuild `pat-lang`.

- If `pat-lang` fails to build with an error like the one below, the EIO dependencies are missing or your `opam` switch uses an OCaml version older than 5.2.0.

  ```bash
  Error: Library "eio_main" not found.
  ```

  Install them with `opam install eio eio_main`; if `opam` refuses because of the OCaml version, create a new switch with `opam switch create 5.2.0` first, re-run `eval $(opam env)`, and reinstall the dependencies listed above.

- If errors like the one below occur when testing `paterl`, it means that the `PATERL_PAT` environment variable points to an incorrect `pat` binary.
  In the excerpt below, the binary is mistyped as `patt`.

  ```bash
  [WRITE] Writing temporary Pat file ebin/id_server_demo.
  [PAT] Patt'ing ebin/id_server_demo.
  Error: sh: /path/to/pat-lang/patt: No such file or directory
  sh: line 0: exec: /path/to/pat-lang/patt: cannot execute: No such file or directory
  ```

  Correct the `PATERL_PAT` environment variable and re-run `paterl`; no rebuild is necessary.
  If instead `paterl` reports that it cannot find the `pat` executable, set `PATERL_PAT` or add the `pat-lang` repository root to `PATH`.

## (Optional) Install and configure VS Code

You may wish to set up VS Code as your default source code editor.
We recommend the following extensions that provide syntax highlighting, debugging, and building support for OCaml and Erlang projects.
- [OCaml Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
- [Erlang/OTP](https://marketplace.visualstudio.com/items?itemName=yuce.erlang-otp)

These extensions work for VS Code on MacOS, Ubuntu, and Windows.

We also recommend the [WSL](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) extension which directly integrates into your Windows WSL set-up.
This extension enables you to load and execute code directly into your virtualised WSL environment and accesses the WSL shell from within VS Code.

Note that code extensions, such as the OCaml Platform and Erlang/OTP extensions, need to be **installed** separately for WSL from VS Code.


