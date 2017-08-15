# Jayanth R Varma's `.emacs` (init files)

## Organization and Structure

My `.emacs` contains more than 1,200 lines of lisp, and has been split into several files in `.emacs.d` for easier maintainability. The modular design also makes it easy to pick and choose what is needed by simply keeping the desired files and deleting the rest. 

## `my-settings.el`

The behaviour of my `.emacs` can be customized by changing the custom variables in `my-settings.el`. For example, to change the theme, one can simply change the variable `my-theme-name` from `misterioso` to whatever is desired. 

As discussed later,  `my-settings.el` uses the `hostname` command to set some variables to different values on different devices.

## `init.el`

My `init.el` does only three things:

1. Set the load path for `init` files
2. Keep all the `init` files byte-compiled for faster load
3. Load the *real* `init` file `jrv-init.el`

## `jrv-init.el`

This consists largely of a series of `require` commands that load the various `.el` files that do the actual customization. For example, `jrv-auctex-config` contains the configuration of `auctex` for editing and compiling `LaTeX` files, and `jrv-init.el` contains a `require` command that loads `jrv-auctex-config`.

I run `emacs` on many machines, servers and devices ranging from my laptop to my mobile phone. On some of these, I need a minimal `.emacs` that handles only a few modes. Rather than maintain different `.emacs` for each device, I simply delete unneeded `.el` files while copying my `.emacs.d` to less capable devices. When `require` is called with its fourth argument non-nil, `emacs` does not signal an error if the package is missing. This allows me to use the same `jrv-init.el` file in all my devices even if they do not contain many of the `.el` files that `jrv-init.el` tries to load.

However, in my main machine, where all the `.el` files are expected to be present, I do want to know if some package did not load. So the fourth argument to `require` is not hard coded to `t` or `nil`, but is set to the variable `my-minimal` which is set in my customization file `my-settings.el`. (Actually, `my-settings.el` uses the `hostname` command to set `my-minimal` to `t` on some machines and `nil` on other devices.)

## `.el` files for different modes

The following `.el` files deal with configuration of various modes:


* `jrv-auctex-config.el`: editing and compiling `LaTeX` files
* `jrv-company.el`: Completion using `company` mode
* `jrv-cpp.el`: compiling `C++` files
* `jrv-csv.el`: viewing and editing `CSV` files
* `jrv-ess-config.el`: editing and running `R` (also `Rnw`) files
* `jrv-markdown.el`: editing `markdown` files
* `jrv-nxml-config.el`: editing `html` files
* `jrv-html.el`: inserting `html` tags and entities in `html` and `markdown` files
* `jrv-ibuffer.el`: customization of `ibuffer`
* `jrv-notmuch.el`: viewing and composing emails using `notmuch`
* `jrv-org.el`: my `org` files and customization of `org` mode
* `jrv-python-elpy.el`: editing and running `python` files
* `jrv-pweave.el`: weaving `Plw` files (`python` chunks in `markdown` or `LaTeX` files)
* `jrv-pydoc.el`: functions and key bindings for documentation of `python` files (using `sphinx`)
* `jrv-text-functions.el`: functions useful in `text` mode (including spell, word count and proper-case)

## dired mode

* `jrv-dired-config.el`: configuration of `dired` mode
* `jrv-dired-compare.el`: modified version of `dired-compare-directories`
* `jrv-shell-commands.el` contains functions for running shell commands in `dired` (some of these are variants of `dired-do-async-shell-command`)

## `emacs` configuration 

* `jrv-basic-customize.el` sets the theme, hides the menu and tool bars, sets the history configuration and other similar stuff
* `jrv-key-alias.el` defines various keybindings and aliases
* `jrv-buffer-functions.el` contains functions for opening and killing buffers and windows.

## `mypaths.el`

This is where many file names and paths are set. Rather than hard code these file names, I use `symlinks` in the folder `/~/0`. `mypaths.el` scans all files in this folder to find the filenames. It also allows long paths to be referred to in `emacs` by short names by providing a `symlink` in `/~/0`. These functions might be useful to others as well.

## `jrv-finish.el`

This file contains code that is highly specific to my needs and is probably useless to others without substantial modifications. It is included here only in the belief that a small minority of people might find some of the ideas useful for their own needs even if they are very different from mine.

My way of running `emacs` is as follows:

* I run `emacs` as server/daemon immediately after login  
  `emacs --daemon`  
  No `emacs` frame is displayed at this stage; the server runs in the background waiting for `emacsclient` to be run. Starting the server runs all the `init` files, but `jrv-finish.el`, contains only function definitions and no executable code. So nothing in this file is actually run at this stage. The two important functions in this file, `run-jrv-finish` and `after-dropbox-sync` are run later using `emacsclient --eval` as described below.
       
* I use the following command to display the `emacs` frame with my set of windows/buffers/screens. (I can exit `emacsclient` at any time, and then I can run the command again to display the frame again):  
     `nohup emacsclient --create-frame --eval '(run-jrv-finish t t t)' &`  
 `run-jrv-finish` takes three arguments. If the first argument is `t`, a set of my standard buffers and files are opened in two windows in the same frame. If the second argument is `t`, a set of `email` buffers are opened in two windows in the same frame. (If both arguments are `t`, then two screens are created using `escreen` with the two sets of windows in the two screens). When the third argument is `t`, `after-dropbox-sync` is not run immediately (it must be run separately as described below).
 
* Some of my files are in the cloud (`dropbox`). After login, I have to wait for `dropbox` to sync the files from the cloud to the local machine, then I have to process that folder using some python scripts. Only after that, do the files become available for use in `emacs`. To inform `emacs` that the files are ready to be opened, the python script runs the command:  
  `emacsclient --eval '(after-dropbox-sync)'`  
  When `after-dropbox-sync` is run, it opens the files from the cloud that are now available.

## Redaction and modifications

The files posted here are mildly redacted and slightly modified versions of my actual files. Personal information like `email` addresses have been redacted. Some file names have been modified to `\path\to\file\name` or something similar.
