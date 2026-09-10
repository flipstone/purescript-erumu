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

### Update the spago package set

The package set is `workspace.packageSet.registry` in `spago.yaml`. Dependabot
has no PureScript ecosystem, so moving it is a manual step. From a
`./scripts/shell`:

```bash
spago registry package-sets         # list the available sets
spago upgrade                       # move spago.yaml to the latest set
spago upgrade --package-set 60.7.0  # or to a specific one
```

Each set is built against one compiler version, shown in the last column of
`spago registry package-sets`. Compare it with `purs --version` in the
container before choosing: a set built for a compiler newer than the image's
needs a newer `purescript-tools` image first (an older one is fine, as the
current set shows). That image is pinned in `compose.yaml`
with its PureScript version in the tag, and Dependabot bumps it within that
PureScript version; moving to a new PureScript version is a manual edit of
the pin.

Then run `./scripts/build`, which rebuilds against the new set and rewrites
`spago.lock`. Commit `spago.yaml` and `spago.lock` together.

# Git Blame Ignore Revs

To ignore certain large commits for git blame purposes, such as changing automatic
formatting, you can add a line to .git-blame-ignore-revs and run

`git config --local blame.ignoreRevsFile .git-blame-ignore-revs`

to set git blame to ignore the SHAs listed in this file.
