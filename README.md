# paterl - Erlang to Pat transpiler

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

## Using from command line

```shell
./src/paterl src/examples/erlang/codebeam/id_server_demo.erl -v all -I include
```

# Installing the `paterl` toolchain

The following instructions detail how `paterl` can be installed on MacOS, Ubuntu, and Windows.

Some of the installation steps below are common to different installations and this guide identifies them accordingly by the corresponding operating system icon on which steps should be executed.

<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/apple.svg" width="20" height="20"> <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/ubuntu.svg" width="20" height="20"> <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/windows.svg" width="20" height="20">

## Install the Windows Subsystem for Linux

The simplest and ideal way to install `paterl` on Windows is to use the Windows Subsystem for Linux (WSL).
WSL provides virtualised Linux environment on Hyper-V.
While WSL can access your Windows file system, software installed on the virtualised Linux environment is isolated from and cannot be accessed by Windows, and vice versa
This means that packages (e.g. Erlang) installed on Windows must also be installed in the virtualised Linux environment to be available to Linux.

1. By default, WSL installs the latest Ubuntu distribution.
   This is done by typing the following on your terminal or PowerShell:

   ```bash
   wsl --install
   ```

2. After the installation completes, update your Ubuntu environment to the latest packages.

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

1. The easiest way to install on WSL and Ubuntu is via `apt`:

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

   **This step is required whenever you wish to interact with `opam` to install packages.**

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

   ```
   sudo apt install z3
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

## Setting up Pat

Pat is the OCaml backend component to `paterl`.
It processes programs written in the Pat language and uncovers communication errors.
The `paterl` Erlang frontend synthesises Pat programs from Erlang source code and launches the Pat type checker as a shell process and retrieves potential errors generated by Pat.
These Pat-generated errors are processed by `paterl` and presented to the user in the form of Erlang errors.

1. Install the Pat OCaml dependencies.
   These dependencies include the Z3 OCaml bridge, which is used by Pat to solve pattern inclusion constraints to uncover communication errors in Pat programs.
   The dependencies can be installed via `opam` as shown.

   ```bash
   $ opam install ppx_import visitors z3 bag cmdliner
   ```

2. Clone the Pat type checker Git repository:

   ```bash
   git clone https://github.com/SimonJF/mbcheck.git
   ```

3. The `paterl`-Pat integration is currently implemented as an experimental branch, and is required by `paterl`.
   Switch the Pat development branch to `paterl-experiments`

   ```
   $ cd mbcheck
   $ git checkout paterl-experiments
   ```

3. Build the Pat type checker using the included `Makefile`:

   ```bash
   $ make
   ```

4. Finally, test your Pat type checker installation by running one of the many Pat examples:

   ```bash
   $ ./mbcheck test/examples/de_liguoro_padovani/future.pat
   ```

## Installing Erlang/OTP

Our `paterl` frontend requires Erlang/OTP > 26.
The best way to install Erlang/OTP on WSL and Ubuntu is to use the Personal Package Archive (PPA) maintained by the RabbitMQ team (the PPA managed by ESL currently seems to be out of date).
The RabbitMQ PPA is valid for Ubuntu 22.04 and 20.04.

1. Add the RabbitMQ PPA to your `apt` installation:

   ```bash
   $ sudo add-apt-repository ppa:rabbitmq/rabbitmb-erlang
   $ sudo apt update
   $ sudo apt install erlang
   ```

   If you have already installed Erlang from the Ubuntu repository, it will be upgraded to the version given in the PPA.
   
2. Test your installation by launching the Erlang shell:

   ```bash
   $ erl 
   Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

   Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)
   1> 
   ```

3. Quit the shell by typing:

   ```erlang
   q().
   ```

In case you need to remove the Erlang version installed from the RabbitMQ PPA and restore it to the version provided by the Ubuntu repository, use PPA `purge` as follows:

```bash
$ sudo apt install ppa-purge
$ sudo ppa-purge ppa:rabbitmq/rabbitmq-erlang
```

If you wish to completely remove Erlang, type:

```bash
$ sudo apt remove erlang
```


## Set up `paterl`

## Step 1: Clone paterl

```
git clone
```








## Insall and configure VS Code