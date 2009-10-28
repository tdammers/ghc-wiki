
This page should document issues with packing code and the pointer tagging optimisation in GHC.



For now, it reproduces e-mail exchange between Simon Marlow and Hans-Wolfgang Loidl, to document some technical details mentioned. 


>
> >
> >
> > On Tue, 20 Oct 2009 13:16:56 +0100
> > Simon Marlow\<marlowsd@...\>  wrote:
> >
> >
> > >
> > >
> > > On 16/10/2009 21:33, Hans-Wolfgang Loidl wrote:
> > >
> > >
> > > >
> > > >
> > > > Hi Simon (cced to other parallel hackers for general interest),
> > > >
> > > >
> > > >
> > > > Mustafa and I are currently debugging a 6.8-based version of GUM,
> > > > and ran into problems with pointer-tagging, so here some concrete
> > > > questions.
> > > >
> > > >
> > > >
> > > > I'm using these resources as main reference. Any others we should be
> > > > aware of?
> > > >
> > > >
> > >
> >
>

- [
  http://hackage.haskell.org/trac/ghc/wiki/SemiTagging](http://hackage.haskell.org/trac/ghc/wiki/SemiTagging)
- Faster laziness using dynamic pointer tagging (paper)
- Cmm.h, GC.c

>
> >
> > >
> > >
> > > You didn't find the best reference:
> > >
> > >
> >
>

>
>
> [
> http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/PointerTagging](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/PointerTagging)
>
>
> >
> > >
> > > >
> > > >
> > > > I consider that a failure on our part, where should it have been
> > > > linked from?  (I just added a link from the [SemiTagging](semi-tagging) page you
> > > > found)
> > > >
> > > >
> > >
> >
>

>
> >
> >
> > One Bad News for me was that tagging is no longer optional :-(
> >
> >
> >
> > Is there any chance of having an optional version of tagging, that
> > would allow us in the RTS to just throw away tags, or would that
> > require a lot of changes in codeGen and RTS? Obviously, it would
> > not be the default implementation, since it will cause loss of
> > sequential performance, but could usefully be enabled for parallel
> > compilation. For one thing it would simplify the parallelism code in
> > the RTS and give us a precious invariant to make debugging simpler.
> >
> >
>
>
> The problem with making tagging optional is that you have to change the code generator to take that into account.  The wiki page explains the various places that would need to be changed.  At those places you would have to mask out tag bits rather than assuming their value, which would be slightly slower.
>
>
>
> Much worse is that it would mean recompiling \*everything\*, so you'd be back to having a separate library way for parallelism, which is something we want to avoid at all costs IMO.
>
>
>
> Another option you might pursue is to make the GC reconstruct tag bits.  Right now the GC \*preserves\* tag bits, which is enough given the invariant that we have, but if you made the GC \*establish\* the invariant, that might let you drop the tag bits in some cases.  The reason we don't do this right now is because I think we found that preserving the tag bits was a bit quicker than creating them (you don't have to look at the constructor tag in the GC).  Oh, and another problem is that the GC would have to know whether the constructor came from a family with few enough constructors to fit in the tag bits.
>
>
> >
> >
> > Another reason for raising this is that we might want to use tagging for parallelism purposes too. For example Vladimir distinguishes closures by expected runtime, and such a classification might be encoded in a tag. So, the even bolder request: could tagging be made modular, so that with the right compiler and RTS-build flags it can be turned off safely, with the GC ignoring but preserving tags, so that we can make use of the tagging infrastructure in the parallel RTS?
> >
> >
> > >
> > > >
> > > >
> > > > Do you have concrete sanity check functions w.r.t tagging?
> > > > I haven't found specific ones in Sanity.c. But I figure only
> > > > specific combinations of tags and closure types make sense.
> > > > Any suggestions for such checks?
> > > >
> > > >
> > >
> > >
> > > Yes, Sanity should be checking for pointer tagging anomalies, as
> > > should other parts of the RTS when DEBUG is on.
> > >
> > >
> >
> >
> > I'll have a go at defining sanity fcts on tags, to make sure we don't
> > break things in the parallel RTS, and might run it past you.
> >
> >
> > >
> > > >
> > > >
> > > > Are there plans for using the tag-space-explosion with 64-bit
> > > > machines to fill up the space with lots of useful information?
> > > > I figure you must be ecstatic with the new possibilities ;-)
> > > >
> > > >
> > >
> > >
> > > Don't get too excited, there's only one extra bit :)   At the moment
> > > we're just using it to be able to encode constructors with tags
> > > greater than 3, and functions with arity greater than 3.
> > >
> > >
> > > >
> > > >
> > > > Any concrete plans about changes w.r.t tagging in the near
> > > > future (or since 6.8, for that matter)?
> > > >
> > > >
> > >
> > >
> > > We're currently sketching ideas for independent per-CPU GC.  While
> > > this probably won't affect the form of the tag bits, it may have
> > > other implications for the heap representation, as most of the
> > > strategies for doing this involve moving objects around between GCs.
> > > Pointer tagging makes our life a bit more difficult here too.
> > >
> > >
> >
> >
> > That's what I'm afraid of: that tagging will impact many aspects of
> > the parallel RTS too. Many of the bugs we eliminated last week were
> > due to not handling tags properly in GUM6.
> > Hence the request for the optional tagging.
> >
> >
> >
> > In any case, we should put these things on the discussion list for the
> > parallel hackaton:
> >
> >
> > - tagging
> > - per-CPU GC
>
>

