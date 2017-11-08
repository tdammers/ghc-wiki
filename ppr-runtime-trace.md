# `pprRuntimeTrace`



In the process of trying to de-lazify `TypeRep`s, I found it somewhat helpful to
write a simple mechanism for injecting runtime calls to `Debug.Trace.trace`
in the desugarer. Why? When there is an infinite loop in one of the functions in
`Data.Typeable.Internal`, trying to print out information about its arguments is
likely to fall into an infinite loop. Oy! The function I came up with is


```
-- | Inject a trace message into the compiled program.
pprRuntimeTrace :: String   -- ^ header
                -> SDoc     -- ^ information to output
                -> CoreExpr -- ^ expression
                -> DsM CoreExpr
pprRuntimeTrace str doc expr = do
  traceId <- dsLookupGlobalId traceName
  unpackCStringId <- dsLookupGlobalId unpackCStringName
  dflags <- getDynFlags
  let message :: CoreExpr
      message = App (Var unpackCStringId) $
                Lit $ mkMachString $ showSDoc dflags (hang (text str) 4 doc)
  return $ mkApps (Var traceId) [Type (exprType expr), message, expr]
```


`pprRuntimeTrace header doc expr` will produce an expression that looks
like `trace (header+doc) expr`.



I've put up [
Phab:D4162](https://phabricator.haskell.org/D4162) to add this utility to `DsMonad`.


