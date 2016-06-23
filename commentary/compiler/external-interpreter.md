# The External Interpreter



When the flag `-fexternal-interpreter` is used, GHC runs interpreted code in a separate process.  



For the background and rationale, see [RemoteGHCi](remote-gh-ci).


## Where the code for `-fexternal-interpreter` lives



The main pieces are:


- `libraries/ghci`, containing:

  - types for talking about remote values ([
    GHCi.RemoteTypes](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/RemoteTypes.hs))
  - the message protocol ([
    GHCi.Message](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/Message.hs))
  - implementation of the messages ([
    GHCi.Run](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/Run.hs))
  - implementation of Template Haskell ([
    GHCi.TH](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/TH.hs))
  - a few other things needed to run interpreted code

- `iserv` directory at the top-level, containing the code for the external
  server.  This is a fairly simple wrapper, most of the functionality
  is provided by modules in `libraries/ghci`.

- The [
  \| GHCi](https://phabricator.haskell.org/diffusion/GHC/browse/master/compiler%2Fghci%2FGHCi.hs) module in `compiler/ghci` which provides the interface to the server used
  by the rest of GHC.

## Implementation overview



GHC works with and without `-fexternal-interpreter`.  With the flag, all
interpreted code is run by the `iserv` binary.  Without the flag,
interpreted code is run in the same process as GHC.



With `-fexternal-interpreter`, the first time we need to run some interpreted code, we start the `iserv` server. This is done by `withIServ` in [
\| GHCi](https://phabricator.haskell.org/diffusion/GHC/browse/master/compiler%2Fghci%2FGHCi.hs).



GHC and `iserv` communicate over a pair of pipes, one for sending messages and one for receiving.



All communication is done using messages serialized using the `binary` package.  The main message type is `Message` in [
GHCi.Message](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/Message.hs).  To send a message from GHC, use `iservCmd` in [
\| GHCi](https://phabricator.haskell.org/diffusion/GHC/browse/master/compiler%2Fghci%2FGHCi.hs).  There are wrappers for common message types.



There are multiple versions of `iserv`: plain `iserv`, `iserv_p`, and `iserv_dyn`.  The latter two are compiled with `-prof` and `-dynamic` respectively.  One big advantage of `-fexternal-interpreter` is that we can run interpreted code in `-prof` mode without GHC itself being compiled with `-prof`; in order to do that, we invoke `iserv_p` rather than `iserv`.


### What runs where?



In the GHC process:


- The compiler: everything from `.hs` to byte code and object code.
- When we're running TH code, the methods of the `Quasi` class, like `qReify`, run in the GHC process.  The results are sent back to the TH computation running in the `iserv` process.


In the `iserv` process:


- Byte code is executed here, including compiled TH code (the contents of splices)
- External packages and object code are linked into this process, so that the compiled byte code can call functions from packages and other modules.

### How does byte code execution work?



To run some interpreted code, GHC compiles the Haskell code to byte code as usual, and then sends the byte code over the pipe to `iserv`, which is responsible for linking it, executing it, and sending back responses if any.



Any compiled packages and object files are linked into the `iserv` process using either the system dynamic linker or the RTS linker, depending on whether we're using `-dynamic` or not respectively.


### How does Template Haskell work?



This is a bit more tricky, because the communication is two way: we send the TH code to `iserv`, but during execution the TH code may make requests back to GHC, e.g. to look up a `Name`.  



The set of operations that TH code can perform is defined by the `Quasi` class in [
Language.Haskell.TH.Syntax](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs).  Under `-fexternal-interpreter`, each of these operations results in a message sent back from `iserv` to GHC, and a response to the message sent from GHC back to `iserv`.  The `iserv` side of this communication is in [
GHCi.TH](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries/ghci/GHCi/TH.hs), and the GHC side is in `runRemoteTH` in [
TcSplice.hs](https://phabricator.haskell.org/diffusion/GHC/browse/master/compiler%2Ftypecheck%2FTcSplice.hs).



For more details, see `Note [Remote Template Haskell]` in [
\| libraries/ghci/GHCi/TH.hs](https://phabricator.haskell.org/diffusion/GHC/browse/master/libraries%2Fghci%2FGHCi%2FTH.hs)


