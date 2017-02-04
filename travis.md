# Travis



Travis-CI is a free-for-open-source continuous integration service. See [Validating Patches](testing-patches#travis) for how to use it to validate your own GHC fork on Github.


## What does it do?



It watches the [repository](repositories) for new commits (any branch) and validates them. The results are presented on


- [ the Travis results page](https://travis-ci.org/ghc/ghc/builds)


where you can select one of the recent build results.



To view a build log, you would


- click on the build number in the first column (e.g. 617)
- then scroll down and select one of the two jobs (one is built with debugging, one without). Click on the number (e.g. 617.1)
- the next page contains the build log, but presented using JavaScript, which may or may not be too much for your browser, and may be truncated. Therefore, while the page is loading, quickly click the “Download Log” above the build log. This will open the log as a plain text file.

## What is validated?



Because of time constraints, not a full validation run is done. Instead


- only static libraries are built,
- GHC is linked statically,
- the test suite is run in “fast” mode,
- all performance tests are skipped,
- and neither haddock nor documentation is built.


It does all this in two variants


- without `-DDEBUG`, to match what we release, and
- with `-DDEBUG`, to catch assertions.


These settings are made in [ghc/.travis.yml](/trac/ghc/browser/ghc/.travis.yml)[](/trac/ghc/export/HEAD/ghc/.travis.yml). You can conveniently experiment with different settings in a `wip/...` branch.


## Statuses


- **Success** (green checkmark):

>
>
> The validation run went through without problems. Great!
>
>

- **Failure** (Red cross):

>
>
> There was a validation error, such as a build failure or a failing test case. Go and fix it!
>
>

- **Error** (Grey exclamation mark): Travis could not finish the build. Most often, this is due to the build exceeding the time limit of 50 minutes. 


   


>
>
> In that case, you can probably ignore the problem. If you are a [
> member of the GitHub GHC team](https://github.com/orgs/ghc/people), you can restart the build, to keep the build history tidy.
>
>

