# Safe Compilation



One of the use cases we see for [SafeHaskell](safe-haskell) is creating applications from code written by multiple parties, where some are untrusted. We would like to be able to support a use case like this where the code is included in an automated fashion, for example submitting plugins for a website and allowing for them to be added without manual authorization. While SafeHaskell provides a lot of the guarantees we need, a malicious third party developer can still cause lots of problems at compile time. Some quick examples of these include:


- Using CPP to include sensitive files in their source code.
- Using custom link options to link in libraries with known exploits
- Using custom program options for passes (e.g a custom CPP) to execute an arbitrary script.


To combat these attacks we will add a new static flag to ghc, currently called **`--safe-compile`** that disables the use of many of GHC's flags. A use case such as the one above will still need to take further measures to be 'secure', such as running all compilations in a new virtual machine created from a safe snapshot. However safe compilation should reduce the attack surface and make GHC and Haskell easier to reason about.


## GHC Flags



GHC flags currently come from three sources:


- Default flags set in the GHC source code.
- Flags set in module PRAGMA's.
- Flags set by the invoker of GHC.

  - This may be the user invoking GHC directly,
  - **OR** More likely it is a build tool such as Cabal or Make calling GHC.


For the implementation of **--safe-compile** it must be decided how to filter each of these three sources. (Note that in the third case, a build tool, the flags come from two sources, those set statically in the .cabal or make file and those set by the user invoking cabal or make on the command line. However GHC can't differentiate between them so we treat them the same). The first two cases are easy, default flags will need to be checked and changed accordingly. Currently we believe the defaults are safe though. PRAGMA flags should be filtered and unsafe flags disallowed. The third case has two possibilities though:


>
>
> 1) We treat arguments to GHC the same as PRAGMA flags and simply disallow unsafe flags. 
>
> 2) We allow all flags still as arguments to GHC.
>
>


The first case is simpler, both in implementation and user interface. Its also the safest. It is fairly inflexible though and would mean there would be no way to allow CPP in combination with **--safe-compile**. The second case is more complex to implement and in user interface. While we trust the user invoking GHC, that user may not trust the .cabal file which in this option would be trusted by GHC. The burden of checking the .cabal file would then fall to the user. How much of a problem this is, is debatable. While it weakens the safety a .cabal file is at least a single central file that can be audited with relative ease. (NOte this may not be so true in the future if cabal allows multiple .cabal files... ect, its also not true for make which can be very complex). The advantage of the second approach is the flexability it gives a user in selectively enabling unsafe flags in '--safe-compile' mode. This flexibility is fairly important, take platforms like SunOS or FreeBSD. It is very common for users of GHC on these platforms to have to manually specify the location of certain tools and libraries given the non-GNU defaults.



There some other approaches. Just quickly as they come to mind:


- Have GHC support setting flags in an environment variable. '--safe-compile' would disable unsafe flags on the command line but not an environment variable. This could still be exploited with a custom Cabal Setup or using Make but for the very common case of a Cabal with a default Setup it would work.
- Same as above but support setting the flags in a .ghc-flags file in the user home directory. User could secure this file by making it read only to GHC and owned by a different user. This file could support compile flag groups for more flexibility. So the user would choose a group to use when calling ghc. This would be a useful feature independent of '--safe-compile'.
- Disable unsafe flags on the command line if they appear **after** '--safe-compile' but not before.

## Unsafe GHC Flags



**Dynamic Flags:**


- -cpp and -XCPP
- -pgm{L,P,lo,lc,m,s,a,l,dll,F,windres}
- -opt{L,P,lo,lc,m,s,a,l,dll,F,windres}
- -F
- -l*lib*
- -framework
- -L*dir*
- -framework-path*dir*
- -main-is
- -package-name
- -D*symbol*
- -U*symbol*
- -I*dir*
- -with-rts-opts
- -rts-opts=
- -dylib-install-name
- -o
- -hidir, -odir, -stubdir, -tmpdir, -outputdir
- -hcsuf, -hisuf, -ohi, -osuf


**Static Flags:**


- -trust, -distrust, -distrust-all-packages


**NOTE:** I made this list originally only looking at dynamic flags, needs to be updated to include static flags.


