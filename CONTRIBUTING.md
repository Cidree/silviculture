# Contributing to silviculture

Thanks for helping improve `silviculture`.

## Recommended workflow

1.  Work from your own fork of the repository.
2.  Keep `Cidree/silviculture` as the `upstream` remote.
3.  Create one branch per pull request.
4.  Make small, focused commits.
5.  Run the package tests before opening a PR.

## Local setup

``` bash
git clone git@github.com:<your-user>/silviculture.git
cd silviculture
git remote add upstream https://github.com/Cidree/silviculture.git
git fetch upstream
```

## Before opening a pull request

- Check that the package loads locally.
- Run
  [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
  from the repository root.
- Update documentation if you change user-facing behavior.
- Mention any assumptions or known limitations in the PR description.
