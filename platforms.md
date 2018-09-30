# Platforms



The following table describes to what extent GHC currently supports
various platforms.  To find out who is responsible for each platform, see [GHC Code Owners](code-owners).



For information about what distributions GHC is part of, see the [
distribution packages](http://haskell.org/ghc/distribution_packages) page.


## Tier 1 platforms



Tier 1 platforms are our top priority.  We only release GHC when they all work.
Although there are not many Tier 1 platforms, they cover a very large fraction of our users.



Criteria for Tier 1 platforms:


- An active buildbot client, capable of doing full builds and uploading distributions.
- An active sponsor, willing to investigate and fix platform-specific bugs, and 
  to work with us during the release process

<table><tr><th> **Architecture** </th>
<th> **OS**        </th>
<th> **Build name**         </th>
<th> **GHCi** </th>
<th> **NCG** </th>
<th> **Dyn libs** </th>
<th> **Sponsor** </th>
<th> **WikiPage** 
</th></tr>
<tr><th> x86                </th>
<th> Windows (MinGW) </th>
<th> i386-unknown-mingw32     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes(\*W)        </th>
<th> GHC HQ        </th>
<th> [Platforms/Windows](platforms/windows) 
</th></tr>
<tr><th> x86-64             </th>
<th> Windows (MinGW) </th>
<th> x86\_64-unknown-mingw32   </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes(\*W)        </th>
<th> GHC HQ        </th>
<th> [Platforms/Windows](platforms/windows) 
</th></tr>
<tr><th> x86                </th>
<th> Linux           </th>
<th> i386-unknown-linux       </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> GHC HQ        </th>
<th> 
</th></tr>
<tr><th> x86-64             </th>
<th> Linux           </th>
<th> x86\_64-unknown-linux     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> GHC HQ        </th>
<th> 
</th></tr>
<tr><th> x86-64             </th>
<th> MacOS X         </th>
<th> x86\_64-apple-darwin      </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> GHC HQ        </th>
<th> [Building/Preparation/MacOSX](building/preparation/mac-osx) 
</th></tr></table>



**\*W** Windows DLL support currently quite limited due to platform limitations (see [\#5987](http://gitlabghc.nibbler/ghc/ghc/issues/5987))


## Tier 2 platforms



Tier 2 platforms work (to varying degrees), but we rely on community support for
developing, testing, and building distributions.  We may release GHC
with some Tier 2 platforms not working.



Platform-specific bugs on Tier 2 platforms are marked "low priority" (unless there's
a strong reason not to do so), not because they are unimportant to the users of that
platform, but to express the fact that they aren't going to hold up the release.



We'd like to promote as many
Tier 2 platforms as possible to Tier 1, as soon as they meet the Tier 1 criteria.


<table><tr><th> **Architecture** </th>
<th> **OS**        </th>
<th> **Build name**         </th>
<th> **GHCi** </th>
<th> **NCG** </th>
<th> **Dyn libs** </th>
<th> **WikiPage** 
</th></tr>
<tr><th> x86                </th>
<th> FreeBSD         </th>
<th> i386-portbld-freebsd     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> [FreeBSDGhc](free-bsd-ghc) 
</th></tr>
<tr><th> x86-64             </th>
<th> FreeBSD         </th>
<th> amd64-portbld-freebsd    </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> [FreeBSDGhc](free-bsd-ghc) 
</th></tr>
<tr><th> x86                </th>
<th> OpenBSD         </th>
<th> i386-unknown-openbsd     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> x86                </th>
<th> Solaris         </th>
<th> i386-unknown-solaris2    </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes (\*S4)      </th>
<th> 
</th></tr>
<tr><th> x86-64             </th>
<th> OpenBSD         </th>
<th> amd64-unknown-openbsd    </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> [Preparing and Building OpenBSD](building/preparation/open-bsd) 
</th></tr>
<tr><th> x86-64             </th>
<th> DragonFly       </th>
<th> x86\_64-portbld-dragonfly </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> PowerPC            </th>
<th> Linux           </th>
<th> powerpc-unknown-linux    </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> PowerPC            </th>
<th> MacOS X         </th>
<th> powerpc-apple-darwin     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> PowerPC64          </th>
<th> Linux           </th>
<th> powerpc64-unknown-linux  </th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> PowerPC64le        </th>
<th> Linux           </th>
<th> powerpc64le-unknown-linux</th>
<th> Yes        </th>
<th> Yes       </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> Sparc              </th>
<th> Linux           </th>
<th> sparc-unknown-linux      </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> IA-64              </th>
<th> Linux           </th>
<th> ia64-unknown-linux       </th>
<th> Yes        </th>
<th> No        </th>
<th> No             </th>
<th> [Building/IA64Linux](building/i-a64-linux) 
</th></tr>
<tr><th> Alpha              </th>
<th> Linux           </th>
<th> alpha-unknown-linux      </th>
<th> Yes        </th>
<th> No        </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> HPPA               </th>
<th> Linux           </th>
<th> hppa-unknown-linux       </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> S/390              </th>
<th> Linux           </th>
<th> s390-ibm-linux           </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> m68k               </th>
<th> Linux           </th>
<th> m68k-unknown-linux       </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> mips               </th>
<th> Linux           </th>
<th> mips-unknown-linux       </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> mipsel             </th>
<th> Linux           </th>
<th> mipsel-unknown-linux     </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> ARM                </th>
<th> Linux           </th>
<th> arm-unknown-linux        </th>
<th> Yes        </th>
<th> No        </th>
<th> Yes            </th>
<th> 
</th></tr>
<tr><th> ARM                </th>
<th> Debian armel    </th>
<th> arm-linux-gnueabi        </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> [Building/ARMLinuxGnuEABI](building/arm-linux-gnu-eabi) 
</th></tr>
<tr><th> ARM                </th>
<th> iOS             </th>
<th> arm-apple-darwin10       </th>
<th> No         </th>
<th> Yes       </th>
<th> No             </th>
<th> [Building/CrossCompiling/iOS](building/cross-compiling/i-os) 
</th></tr>
<tr><th> PowerPC            </th>
<th> AIX             </th>
<th> powerpc-ibm-aix          </th>
<th> No         </th>
<th> Yes       </th>
<th> No             </th>
<th> [Building/AIX](building/aix) 
</th></tr>
<tr><th> x86                </th>
<th> MacOS X         </th>
<th> i386-apple-darwin        </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> [Attic/X86OSXGhc](attic/x86-osx-ghc) 
</th></tr></table>



**\*S4** shared libraries are supported on Solaris 11 version 11/11 and higher



In most cases, binaries for the Tier 2 platforms can be downloaded from the [Distribution Packages](http://www.haskell.org/ghc/distribution_packages) page, e.g. you can get binaries for most of the Linux platforms from Debian. In some cases, for example the Solaris platforms, you'll need to go to the [download page](http://www.haskell.org/ghc/download) of a particular release to get a bindist.


## Tier 3 platforms



Tier 3 platforms worked in the past, but probably do not work now.


<table><tr><th> **Architecture** </th>
<th> **OS**         </th>
<th> **Build name**        </th>
<th> **GHCi** </th>
<th> **NCG** </th>
<th> **Dyn libs** </th>
<th> **WikiPage** 
</th></tr>
<tr><th> Mips64             </th>
<th> Irix             </th>
<th> mips-sgi-irix           </th>
<th> ?          </th>
<th> No        </th>
<th> ?              </th>
<th> 
</th></tr>
<tr><th> x86                </th>
<th> Windows (Cygwin) </th>
<th> i386-unknown-cygwin32   </th>
<th> No         </th>
<th> Yes       </th>
<th> No(?)          </th>
<th> 
</th></tr>
<tr><th> Alpha              </th>
<th> Dec OSF          </th>
<th> alpha-dec-osf3          </th>
<th> No         </th>
<th> No        </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> ARM                </th>
<th> Maemo (Linux)    </th>
<th> arm-unknown-linux-gnu   </th>
<th> No         </th>
<th> No        </th>
<th> No             </th>
<th> [ArmLinuxGhc](arm-linux-ghc)    
</th></tr>
<tr><th> x86                </th>
<th> NetBSD           </th>
<th> i386-unknown-netbsd     </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> x86-64             </th>
<th> NetBSD           </th>
<th> amd64-unknown-netbsd    </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> 
</th></tr>
<tr><th> Sparc              </th>
<th> Solaris          </th>
<th> sparc-sun-solaris2      </th>
<th> Yes        </th>
<th> Yes       </th>
<th> No             </th>
<th> [Building/Solaris](building/solaris) 
</th></tr></table>


## Definitions



**GHCi**


>
>
> The interactive environment, including dynamic linking of object
> code and dynamic generation of FFI calls.
>
>


**NCG**


>
>
> Native code generator: GHC can generate assembly code directly for this platform, bypassing gcc.
>
>


**Dynamic libraries**


>
>
> Support for generating dynamically-linked sharable libraries from
> Haskell code.
>
>

