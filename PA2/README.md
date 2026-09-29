The first step that you do here in this repository is to clone it/copy it. You do that through the "Use this template" green button that is located on the top right below your profile picture, right next to button option "Star". 
* When creating the repository, make sure to put OU-CS3200 as the Owners of your homework repo. 
* Additionally, MAKE SURE to change the visibility of your repo to PRIVATE. You can do that right below choosing the owner and naming your repo at the repository creation screen, or in the settings after your repo has been created.

What you will submit is a zip file of your GitHub repo. How you do this is after you have gone through your code and solved everything, including git pushing back everything into your repository: you will download a zip of your GitHub repository and submit that zip file on Canvas for the assignment. To get a zip file of your repository click on the green button Code which will provide a dropdown menu listing a few options and go ahead with downloading a zip file of your repo.

## Setting up

You need an opam switch with the build tool and the two testing libraries this
assignment uses. If you already did PA0, you have most of this:

```
opam install dune alcotest qcheck qcheck-alcotest
```

Or, from the root of this repository, let opam read the dependency list straight
out of `dune-project`:

```
opam install --deps-only .
```

Every time you open a new terminal, make sure your switch is active:

```
eval $(opam env)
opam switch show     # should print the switch you created for this course
which dune           # should be under ~/.opam/<your-switch>/bin, not /usr/bin
```

Check that the handout builds *before* you start editing:

```
dune build
```

### If something goes wrong

* `Library "alcotest" not found` or `Library "qcheck-alcotest" not found` — one
  of the packages above is missing. Install **all three**; `lib/util.ml` opens
  `QCheck_alcotest`, so `alcotest` on its own is not enough.
* `Unbound module Alcotest` in your editor even though `dune test` works in the
  terminal — your editor was launched without the opam environment. Quit it
  completely and reopen it from a terminal where the checks above pass (for
  VS Code, run `code .` from that terminal).
* An error mentioning `QCheck.small_list` and `alert deprecated` — the `dune`
  files in `lib/` and `test/` carry a flag that silences this. Do not delete it.

## Working the assignment

Once the build is clean:

1.    Complete the exercises as directed in the comments in the file `lib/lib.ml`, making sure that you include your name and Ohio ID.

2.    Make sure that, before you submit, your program typechecks and all test cases pass (or as many as you can make pass). Run the tests with the command `dune test` from the project root directory.
