# GHC plans for 8.0.1






This page is our road-map for what will be in 8.0.


- We hope to incorporate all the "Landed" and "In-flight" stuff under "Release highlights" below.

- We'll include (or at least review) all patches in tickets in "Status: patch" below.

- We will address all the tickets under "Status: new" below with "highest" or "high" priority.  We love help to do more, but there are far too many "normal" tickets to make any promises.


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!


## Dates



Release candidate by **mid-January 2016**. Release candidate three available in early April 2016, release in **mid-April 2016**.


## Libraries Status



See [Libraries](status/gh-c-8.0.1/libraries)


## Release highlights



Below are the major highlights of 8.0.


### In-flight, and likely to land in time


- Documentation for `-XTypeInType` (Richard)

### Landed in HEAD


- Option to ignore unrecognised warning flags ([\#10429](http://gitlabghc.nibbler/ghc/ghc/issues/10429), Ben, [
  Phab:D1830](https://phabricator.haskell.org/D1830))

- Support for strictness and unpacking in Template Haskell (Trac [\#10697](http://gitlabghc.nibbler/ghc/ghc/issues/10697), [
  Phab:D1603](https://phabricator.haskell.org/D1603))

- Visible type application ([\#5296](http://gitlabghc.nibbler/ghc/ghc/issues/5296))

- Consistent warning options ([\#11218](http://gitlabghc.nibbler/ghc/ghc/issues/11218), [
  Phab:D1613](https://phabricator.haskell.org/D1613))

- More expressive Generics metadata (Ryan Scott, Trac [\#10716](http://gitlabghc.nibbler/ghc/ghc/issues/10716), [\#10697](http://gitlabghc.nibbler/ghc/ghc/issues/10697), [
  Phab:D493](https://phabricator.haskell.org/D493))

- Expose enabled language extensions to [TemplateHaskell](template-haskell) ([\#10820](http://gitlabghc.nibbler/ghc/ghc/issues/10820), Ben, [
  Phab:D1200](https://phabricator.haskell.org/D1200))

- Recursive superclasses (Simon, [
  Phab:D1594](https://phabricator.haskell.org/D1594), [\#11067](http://gitlabghc.nibbler/ghc/ghc/issues/11067))

- Documentation for OverloadedLabels (Adam)

- Use `Cxt` in Template Haskell's representation of `deriving` clauses ([\#10819](http://gitlabghc.nibbler/ghc/ghc/issues/10819), Ben, [
  Phab:D1202](https://phabricator.haskell.org/D1202))

- Fix [\#10845](http://gitlabghc.nibbler/ghc/ghc/issues/10845) with [
  Phab:1422](https://phabricator.haskell.org/1422) (Rework the Implicit CallStack solver to handle local lets)

- Support for reasoning about kind equalities, which gives promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. There is some discussion in [DependentHaskell/Phase1](dependent-haskell/phase1), but that's very low-level. I (Richard) have no good user-oriented write-up yet, but there shouldn't be much in the way of new syntax -- just fewer type errors. ([
  Phab:D808](https://phabricator.haskell.org/D808))

- Support for [implicit parameters providing callstacks/source locations](explicit-call-stack/implicit-locations), allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([
  Phab:D861](https://phabricator.haskell.org/D861), [
  Phab:D1422](https://phabricator.haskell.org/D1422))

- Improved optimization diagnostics. The compiler is now more liberal about issues warnings of failed specialization attempts

- Support for wildcards in data and type family instances ([
  Phab:D1092](https://phabricator.haskell.org/D1092))

- Support for [Injective Type Families](injective-type-families), which allows you to specify type families which are injective, i.e. a one-to-one relationship. ([
  Phab:D202](https://phabricator.haskell.org/D202))

- Support for typed holes in Template Haskell ([
  Phab:D835](https://phabricator.haskell.org/D835))

- Support for [Applicative Do](applicative-do), allowing GHC to desugar do-notation to `Applicative` where possible. ([
  Phab:D729](https://phabricator.haskell.org/D729))

- Support for deriving the `Lift` typeclass ([
  Phab:D1168](https://phabricator.haskell.org/D1168))

- A beautiful new users guide, written in reStructured Text, and significantly improved output.

- Support for [Strict Haskell](strict-pragma) including both the `StrictData` and `Strict` language extensions ([
  Phab:D1033](https://phabricator.haskell.org/D1033) and [
  Phab:D1142](https://phabricator.haskell.org/D1142))

- Support for record pattern synonyms ([
  Phab:D1152](https://phabricator.haskell.org/D1152))

- Allow patterns synonyms to be exported in the same way as data constructors ([
  Phab:D1258](https://phabricator.haskell.org/D1258))

- [Custom type errors](proposal/custom-type-errors). ([\#9637](http://gitlabghc.nibbler/ghc/ghc/issues/9637), [
  Phab:D1236](https://phabricator.haskell.org/D1236)).

- Implement the `MonadFail` proposal ([\#10751](http://gitlabghc.nibbler/ghc/ghc/issues/10751))

- Support for [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields), allowing multiple datatypes to declare the same record field names provided they are used unambiguously ([
  Phab:D761](https://phabricator.haskell.org/D761))

- Support for [OverloadedLabels](records/overloaded-record-fields/overloaded-labels), allowing a form of type-directed name resolution ([
  Phab:D1331](https://phabricator.haskell.org/D1331))

- Simon PJ's `wip/spj-wildcard-refactor` branch, which refactors the handling of implicit binders and wildcards

- A huge improvement to pattern matching (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [
  their paper](http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf). ([
  Phab:D1535](https://phabricator.haskell.org/D1535))

- Backpack is chugging along; we have a new user-facing syntax which allows multiple modules to be defined a single file, and are hoping to release at least the ability to publish multiple "units" in a single Cabal file.

- Improved [DWARF based debugging support](dwarf) from Peter Wortmann, Arash Rouhani, and Ben Gamari with backtraces from Haskell code.

- Significantly more stable support on ARM ([\#11206](http://gitlabghc.nibbler/ghc/ghc/issues/11206))

- Stack traces in GHCi ([\#11047](http://gitlabghc.nibbler/ghc/ghc/issues/11047))

- Better instrumentation for tracking compiler performance of ([\#11710](http://gitlabghc.nibbler/ghc/ghc/issues/11710))

### Possible, if the authors push forward fast enough



There's nothing here.


### Won't make it, but being actively worked on


- An [Improved LLVM Backend](improved-llvm-backend) that ships with every major Tier 1 platform (Austin, [\#10074](http://gitlabghc.nibbler/ghc/ghc/issues/10074))
- Make compilation results more deterministic ([\#4012](http://gitlabghc.nibbler/ghc/ghc/issues/4012))
- Complete support for [OverloadedRecordFields](records/overloaded-record-fields)
- Support for **Type Signature Sections**, allowing you to write `(:: ty)` as a shorthand for `(\x -> x :: ty)`.
- A `DEPRECATED` pragma for exports ([\#4879](http://gitlabghc.nibbler/ghc/ghc/issues/4879))
- A new, type-indexed type representation, `data TTypeRep (a :: k)`. See [TypeableT](typeable-t).
- A (possible) overhaul of GHC's build system to use **Shake** instead of Make.
- Polish and merge Compact Normal Form support for efficient GC/serialization (ICFP'15)

## Migration Guide to 8.0



FIXME Write the migration guide.



[
https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0](https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0)


## Known issues with release candidate 2



Release candidate 2 was [
released](https://mail.haskell.org/pipermail/ghc-devs/2016-February/011309.html) on 7 Feb 2016. These significant issues are known to be present in this release,


- [\#11334](http://gitlabghc.nibbler/ghc/ghc/issues/11334): Solving for `Typeable (Proxy :: Proxy 'Compose)` fails

- [\#11339](http://gitlabghc.nibbler/ghc/ghc/issues/11339): `microlens` package doesn't typecheck

- [\#11608](http://gitlabghc.nibbler/ghc/ghc/issues/11608): `microlens` and `free` packages both fail to typecheck

- [\#11414](http://gitlabghc.nibbler/ghc/ghc/issues/11414): Use of `-XStrict` results in compiler abort

- [\#11471](http://gitlabghc.nibbler/ghc/ghc/issues/11471): Typechecker allows unsafe use of runtime-representation-polymorphism

## Known issues with release candidate 1



Release candidate 1 was [
released](https://mail.haskell.org/pipermail/ghc-devs/2016-January/010966.html) on 13 Jan 2016. These significant issues are present in this release,


- The new `-XInjectiveTypeFamilies` language extension will likely be
  renamed to `-XTypeFamilyDependencies`

- [\#11120](http://gitlabghc.nibbler/ghc/ghc/issues/11120): Type representations are missing for some types and promoted
  constructors

- [\#11334](http://gitlabghc.nibbler/ghc/ghc/issues/11334): Solving for `Typeable (Proxy :: Proxy 'Compose)` fails

- [\#11276](http://gitlabghc.nibbler/ghc/ghc/issues/11276): Pattern checker performance can degrade significantly in
  presence of pattern matches with guards

- [\#11405](http://gitlabghc.nibbler/ghc/ghc/issues/11405): Type-level skolem-escape check fails incorrectly

- [\#11414](http://gitlabghc.nibbler/ghc/ghc/issues/11414): Use of `-XStrict` results in compiler abort

- [\#11379](http://gitlabghc.nibbler/ghc/ghc/issues/11379): Instance solver fails to terminate

- [\#11419](http://gitlabghc.nibbler/ghc/ghc/issues/11419): Haddock documentation is currently not included in the binary
  distributions.

## Tickets marked merge with no milestone




  
  
  
  
  
    

## Status: merge (1 match)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, milestone: , group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, milestone: , group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#16094](http://gitlabghc.nibbler/ghc/ghc/issues/16094)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [panic! (the 'impossible' happened): for powerpc-unknown-linux getRegister(ppc): I64\[I32\[BaseReg + 812\] + 64\]](http://gitlabghc.nibbler/ghc/ghc/issues/16094)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      trommler
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



## Tickets slated for 8.0.1


### merge/patch/upstream




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Differential Rev(s) (Ticket query: status: merge, status: patch,
status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type,
col: summary, col: priority, col: differential, col: owner, order: differential)
      </th>
<th>
        
        Owner (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



### new




  
  
  
  
  
    

## Status: new (3 matches)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: new, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: new, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: type)
      </th>
<th>
        
        Summary (Ticket query: status: new, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: new, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1,
order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: new, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10735](http://gitlabghc.nibbler/ghc/ghc/issues/10735)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Smooth out the differences between \`compiler/utils/Pretty.hs\` and \`libraries/pretty\`](http://gitlabghc.nibbler/ghc/ghc/issues/10735)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10927](http://gitlabghc.nibbler/ghc/ghc/issues/10927)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [IndexError: pop from empty list](http://gitlabghc.nibbler/ghc/ghc/issues/10927)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#3351](http://gitlabghc.nibbler/ghc/ghc/issues/3351)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Generated ghc man page missing xrefs](http://gitlabghc.nibbler/ghc/ghc/issues/3351)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



### infoneeded




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: infoneeded, milestone: 8.0.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: infoneeded, milestone: 8.0.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: type)
      </th>
<th>
        
        Summary (Ticket query: status: infoneeded, milestone: 8.0.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: infoneeded, milestone: 8.0.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: infoneeded, milestone: 8.0.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



