# purescript-erumu

Virtual DOM for PureScript, inspired by Elm.

## Working on Erumu

### Do an initial build

```bash
./scripts/build
```

This is the script you should run to make sure everything is ship-shape before
creating a PR, and can be useful to run after pulling to ensure everything is
up to date.

### Start a build loop

Once you're ready to write some code, you can start up a shell by running the
following script:

```bash
./scripts/shell
```

Once inside the shell, you can run the following command to start automatic
recompilation:

```bash
grunt dev
```

# Git Blame Ignore Revs

To ignore certain large commits for git blame purposes, such as changing automatic
formatting, you can add a line to .git-blame-ignore-revs and run

`git config --local blame.ignoreRevsFile .git-blame-ignore-revs`

to set git blame to ignore the SHAs listed in this file.
