# Exceptions in GHC



Relevant pages:


- [Exceptions/PreciseExceptions](exceptions/precise-exceptions)
- [FixingExceptions](fixing-exceptions)
- [Commentary/CmmExceptions](commentary/cmm-exceptions)
- [Commentary/Rts/AsyncExceptions](commentary/rts/async-exceptions)

## Tickets



Use Keyword = `Exceptions` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#7411](http://gitlabghc.nibbler/ghc/ghc/issues/7411)</th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th>[\#11222](http://gitlabghc.nibbler/ghc/ghc/issues/11222)</th>
<td>Teach strictness analysis about \`catch\`-like operations</td></tr>
<tr><th>[\#11829](http://gitlabghc.nibbler/ghc/ghc/issues/11829)</th>
<td>C++ does not catch exceptions when used with Haskell-main and linked by ghc</td></tr>
<tr><th>[\#12096](http://gitlabghc.nibbler/ghc/ghc/issues/12096)</th>
<td>Attach stacktrace information to SomeException</td></tr>
<tr><th>[\#12696](http://gitlabghc.nibbler/ghc/ghc/issues/12696)</th>
<td>Exception gives not enough information to be useful</td></tr>
<tr><th>[\#13357](http://gitlabghc.nibbler/ghc/ghc/issues/13357)</th>
<td>Check demand signatures for catchRetry\# and catchSTM\#</td></tr>
<tr><th>[\#13370](http://gitlabghc.nibbler/ghc/ghc/issues/13370)</th>
<td>exprIsBottom inconsistent with strictness analyser</td></tr>
<tr><th>[\#13380](http://gitlabghc.nibbler/ghc/ghc/issues/13380)</th>
<td>raiseIO\# result looks wrong</td></tr>
<tr><th>[\#14998](http://gitlabghc.nibbler/ghc/ghc/issues/14998)</th>
<td>Sort out the strictness mess for exceptions</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#942](http://gitlabghc.nibbler/ghc/ghc/issues/942)</th>
<td>Windows programs throw uncaught Invalid HANDLE exception on exit</td></tr>
<tr><th>[\#1905](http://gitlabghc.nibbler/ghc/ghc/issues/1905)</th>
<td>runProcess: misbehaving exception on nonexistent working directory</td></tr>
<tr><th>[\#2211](http://gitlabghc.nibbler/ghc/ghc/issues/2211)</th>
<td>Installing latest GHC-6.8.2 stable: pwd with floating point exception</td></tr>
<tr><th>[\#3983](http://gitlabghc.nibbler/ghc/ghc/issues/3983)</th>
<td>-O2 makes exception disappear</td></tr>
<tr><th>[\#4021](http://gitlabghc.nibbler/ghc/ghc/issues/4021)</th>
<td>Problem of Interaction Between the FreeBSD Kernel and the GHC RTS</td></tr>
<tr><th>[\#4343](http://gitlabghc.nibbler/ghc/ghc/issues/4343)</th>
<td>Add throwSTM and generalize catchSTM</td></tr>
<tr><th>[\#5611](http://gitlabghc.nibbler/ghc/ghc/issues/5611)</th>
<td>Asynchronous exception discarded after safe FFI call</td></tr>
<tr><th>[\#5626](http://gitlabghc.nibbler/ghc/ghc/issues/5626)</th>
<td>Miscompilation, exception omitted with -O</td></tr>
<tr><th>[\#10435](http://gitlabghc.nibbler/ghc/ghc/issues/10435)</th>
<td>catastrophic exception-handling disablement on Windows Server 2008 R2</td></tr>
<tr><th>[\#10712](http://gitlabghc.nibbler/ghc/ghc/issues/10712)</th>
<td>Regression: make TEST=exceptionsrun001 WAY=optasm is failing</td></tr>
<tr><th>[\#11555](http://gitlabghc.nibbler/ghc/ghc/issues/11555)</th>
<td>catch \_\|\_ breaks at -O1</td></tr>
<tr><th>[\#13330](http://gitlabghc.nibbler/ghc/ghc/issues/13330)</th>
<td>forkIO has inconsistent behavior under optimization</td></tr>
<tr><th>[\#13348](http://gitlabghc.nibbler/ghc/ghc/issues/13348)</th>
<td>Consider making throw and throwIO strict</td></tr>
<tr><th>[\#13977](http://gitlabghc.nibbler/ghc/ghc/issues/13977)</th>
<td>ExnStr doesn't propagate "outwards"</td></tr>
<tr><th>[\#15226](http://gitlabghc.nibbler/ghc/ghc/issues/15226)</th>
<td>GHC doesn't know that seq\# produces something in WHNF</td></tr></table>



