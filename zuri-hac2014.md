# Bug squashing at ZuriHac2014



Joachim (nomeata) wants to run a small bugsquashing sprint at [
ZuriHac 2014](http://www.haskell.org/haskellwiki/ZuriHac2014/Projects). 


## Requirements



You should bring some Haskell experience and be confident reading other people’s Haskell code. You do not need to know all the latest fancy type hackery – GHC itself is written in quite plain Haskell. Some knowledge of git is also useful.



Obviously, you need a machine to work on. The more core it has, the less you’ll have to wait.


## Setup



If you want to join in, you can come prepared:


- Read through [Newcomers](newcomers)
- Make sure that you have built GHC once yourself.
- Your changes need to be validated. So make sure you validated GHC once. I suggest to have a second working copy of GHC that you only use to validate. There is a [section](working-conventions/git#) explaining how to do this.
- Fork [
  ghc on github](https://github.com/ghc/ghc/) (or otherwise publish a fork of the GHC repo) for easier collaboration during the hackathon.
- Get an account on this trac.
- Join `#ghc` on freenode.
- (optional, if you plan to stick around) Subscribe to `ghc-dev` and `ghc-tickets` mailing lists.

## Optional tips



If you have a strong remote machine with lots of cores, you can have the validate tree remotely.



For more convenient validation, especially if the validate repository is remotely, I (Joachim) have a script `ci-validate.sh` that waits for a new branch calls `validate/foo`, then validates it cleanly and either moves it to `validated/foo` or `broken/foo`. If you want to set up that as well, fetch the script from my [
ghc-devscripts repository](https://github.com/nomeata/ghc-devscripts).


## Possible tickets



This is a list of tickets that might be suitable for a hacking sprint, but feel free to look for others (click “All Bugs“ and “All Tasks” on the left). And of course, feel free to extend this list.




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, desc: 1, order: id)
      </th>
<th>
        
        Summary (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: summary)
      </th>
<th>
        
        Owner (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: owner)
      </th>
<th>
        
        Type (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: type)
      </th>
<th>
        
        Status (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: status)
      </th>
<th>
        
        Priority (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: priority)
      </th>
<th>
        
        Milestone (Ticket query:
id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836%2C9127%2C8613%2C393%2C1262%2C3314%2C9086,
max: 0, order: milestone)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#17](http://gitlabghc.nibbler/ghc/ghc/issues/17)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Separate warnings for unused local and top-level bindings](http://gitlabghc.nibbler/ghc/ghc/issues/17)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [8.0.1](/trac/ghc/milestone/8.0.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#95](http://gitlabghc.nibbler/ghc/ghc/issues/95)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [GHCi :edit command should jump to the the last error](http://gitlabghc.nibbler/ghc/ghc/issues/95)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      lortabac
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [⊥](/trac/ghc/milestone/%E2%8A%A5)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#393](http://gitlabghc.nibbler/ghc/ghc/issues/393)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [functions without implementations](http://gitlabghc.nibbler/ghc/ghc/issues/393)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      Iceland\_jack
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [⊥](/trac/ghc/milestone/%E2%8A%A5)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#1262](http://gitlabghc.nibbler/ghc/ghc/issues/1262)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [RecursiveDo in Template Haskell](http://gitlabghc.nibbler/ghc/ghc/issues/1262)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      mgsloan
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [⊥](/trac/ghc/milestone/%E2%8A%A5)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#1388](http://gitlabghc.nibbler/ghc/ghc/issues/1388)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Newbie help features](http://gitlabghc.nibbler/ghc/ghc/issues/1388)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [⊥](/trac/ghc/milestone/%E2%8A%A5)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#3314](http://gitlabghc.nibbler/ghc/ghc/issues/3314)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Add compilation date to +RTS --info](http://gitlabghc.nibbler/ghc/ghc/issues/3314)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ak3n
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
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
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#4836](http://gitlabghc.nibbler/ghc/ghc/issues/4836)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [literate markdown not handled correctly by unlit](http://gitlabghc.nibbler/ghc/ghc/issues/4836)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#8429](http://gitlabghc.nibbler/ghc/ghc/issues/8429)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [GHC.Base.{breakpoint, breakpointCond} do nothing](http://gitlabghc.nibbler/ghc/ghc/issues/8429)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      iand675
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
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
<th>[\#8613](http://gitlabghc.nibbler/ghc/ghc/issues/8613)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [simplifier ticks exhausted](http://gitlabghc.nibbler/ghc/ghc/issues/8613)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
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
<th>[\#8959](http://gitlabghc.nibbler/ghc/ghc/issues/8959)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [GHCi should honour UnicodeSyntax](http://gitlabghc.nibbler/ghc/ghc/issues/8959)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [8.0.1](/trac/ghc/milestone/8.0.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9086](http://gitlabghc.nibbler/ghc/ghc/issues/9086)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [main :: IO Int does different things with runghc and when compiled](http://gitlabghc.nibbler/ghc/ghc/issues/9086)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      gintas
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [7.10.1](/trac/ghc/milestone/7.10.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9095](http://gitlabghc.nibbler/ghc/ghc/issues/9095)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [make sdist picks up test files](http://gitlabghc.nibbler/ghc/ghc/issues/9095)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      thomie
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [8.2.1](/trac/ghc/milestone/8.2.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9122](http://gitlabghc.nibbler/ghc/ghc/issues/9122)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Make Lint check for bad uses of \`unsafeCoerce\`](http://gitlabghc.nibbler/ghc/ghc/issues/9122)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      qnikst
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [8.0.1](/trac/ghc/milestone/8.0.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9127](http://gitlabghc.nibbler/ghc/ghc/issues/9127)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Don't warn about pattern-bindings of the form \`let !\_ = rhs\`](http://gitlabghc.nibbler/ghc/ghc/issues/9127)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
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
<th>[\#9132](http://gitlabghc.nibbler/ghc/ghc/issues/9132)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [takeWhile&C. still not fusible](http://gitlabghc.nibbler/ghc/ghc/issues/9132)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      skeuchel
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [7.10.1](/trac/ghc/milestone/7.10.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9136](http://gitlabghc.nibbler/ghc/ghc/issues/9136)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Constant folding in Core could be better](http://gitlabghc.nibbler/ghc/ghc/issues/9136)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [8.6.1](/trac/ghc/milestone/8.6.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9156](http://gitlabghc.nibbler/ghc/ghc/issues/9156)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Duplicate record field](http://gitlabghc.nibbler/ghc/ghc/issues/9156)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      gintas
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9177](http://gitlabghc.nibbler/ghc/ghc/issues/9177)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [Suggest Int when user uses int](http://gitlabghc.nibbler/ghc/ghc/issues/9177)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      nomeata
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      [7.10.1](/trac/ghc/milestone/7.10.1)
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9178](http://gitlabghc.nibbler/ghc/ghc/issues/9178)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      [improve orphan instance warning](http://gitlabghc.nibbler/ghc/ghc/issues/9178)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
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
                  
                
              </td></tr></table>


  



## Summary


- *4* people particitpated: nomeata, gintas, skeuchel, Lorenzo *please add yourself*
- *8* Tickets worked on: [\#9177](http://gitlabghc.nibbler/ghc/ghc/issues/9177), [\#8959](http://gitlabghc.nibbler/ghc/ghc/issues/8959), [\#9127](http://gitlabghc.nibbler/ghc/ghc/issues/9127), [\#9178](http://gitlabghc.nibbler/ghc/ghc/issues/9178), [\#9132](http://gitlabghc.nibbler/ghc/ghc/issues/9132), [\#393](http://gitlabghc.nibbler/ghc/ghc/issues/393), [\#95](http://gitlabghc.nibbler/ghc/ghc/issues/95), [\#9181](http://gitlabghc.nibbler/ghc/ghc/issues/9181)
