# Allow for multiple instances of the GHCi linker



This page discusses a plan to fix bug [\#3372](http://gitlabghc.nibbler/ghc/ghc/issues/3372).


## The problem



GHC includes its own linker, to be used by GHCi to resolve symbols. It is currently implemented using global variables for the symbol tables and other internals. This means one cannot have two or more instances of GHC's interpreter running simultaneously on different threads, since their entries on the symbol tables will conflict. The basic idea to solve this is to move all the global variables to a suitable datastructure and associate an instance of it to GHC's state.



Now, the linker is composed of two rather different parts: the bytecode linker and the object linker, each with its own symbol tables (and, of course, global variables). The latter is part of the RTS, written in C with plenty of \#ifdefs to handle a variety of platforms, object file formats, etc.



Fixing the bytecode linker, as discussed next, seems relatively straightforward. The object linker is much more fragile. In particular, it is harder to test since there is a lot of platform-dependent code under conditional compilation.



*Question:* Can we just leave the object linker as it is right now? If I understand correctly, in that case we will run into trouble if, for example, two instances of GHC try to load different .o files with conflicting symbols. If this may happen by attempting to load two incompatible versions of an installed package, then it might be a frequent scenario.


## Plan for the bytecode linker



The relevant code is in [compiler/ghci/Linker.lhs](/trac/ghc/browser/ghc/compiler/ghci/Linker.lhs). The linker's state is kept in the global variable:


```wiki
v_PersistentLinkerState :: IORef PersistentLinkerState
```


There is an additional global variable `v_InitLinkerDone :: Bool` that is used to make the initialization routine idempotent. This routine is: 


```wiki
initDynLinker :: DynFlags -> IO ()
```


and is (lazily) called by the exported functions `linkExpr` and `unload`. It is also called explicitly from [ghc/GhciMonad.hs](/trac/ghc/browser/ghc/ghc/GhciMonad.hs).



The proposed plan would be to define something along the lines of:


```wiki
newtype DynLinker = DynLinker (IORef (Maybe PersistentLinkerState))

uninitializedLinker :: IO DynLinker
uninitializedLinker = DynLinker `fmap` newIORef Nothing

initDynLinker :: DynFlags -> DynLinker -> IO ()
initDynLinker dflags DynLinker r =
    = do s <- readIORef r
         when (isNothing s) $
          reallyInitDynLinker dflags r


withLinkerState :: (MonadIO m, ExceptionMonad m) => DynLinker -> (IORef PersistentLinkerState -> m a) -> m a
withLinkerState (DynLinker r) action
    = do maybe_s <- readIORef r
         case maybe_s of
           Nothing -> panic "Dynamic linker not initialised"
           Just s  -> do r' <- liftIO $ newIORef s
                         action r'
                         liftIO $ writeIORef r =<< readIORef r'
```


This way we keep the lazy initialization and minimize the modifications needed on the rest of the functions. For example we would turn the following exported function:


```wiki
extendLinkEnv :: [(Name,HValue)] -> IO ()
-- Automatically discards shadowed bindings
extendLinkEnv new_bindings
  = do	pls <- readIORef v_PersistentLinkerState
	let new_closure_env = extendClosureEnv (closure_env pls) new_bindings
	    new_pls = pls { closure_env = new_closure_env }
	writeIORef v_PersistentLinkerState new_pls
```


into this version:


```wiki
extendLinkEnv :: DynLinker -> [(Name,HValue)] -> IO ()
-- Automatically discards shadowed bindings
extendLinkEnv dl new_bindings
  = withLinkerState $ \v_PersistentLinkerState -> 
    do	pls <- readIORef v_PersistentLinkerState
	let new_closure_env = extendClosureEnv (closure_env pls) new_bindings
	    new_pls = pls { closure_env = new_closure_env }
	writeIORef v_PersistentLinkerState new_pls
```


*Question:* Would it be better to use an `MVar` instead of an `IORef` in `DynLinker`?



Finally, to make the `DynLinker` available everywhere, we would have to add a field in `HscEnv` ([compiler/main/HscTypes.lhs](/trac/ghc/browser/ghc/compiler/main/HscTypes.lhs)):


```wiki
data HscEnv 
  = HscEnv { 
     ...
#ifdef GHCI
        hsc_dynLinker :: DynLinker,
#endif	
     ...
    }
```

## Plan for the object linker



The object linker ([rts/Linker.c](/trac/ghc/browser/ghc/rts/Linker.c)) is responsible of loading and keeping track of symbols in object files and shared libraries. For object files it basically uses three global variables:


```wiki
/* Hash table mapping symbol names to Symbol */
static /*Str*/HashTable *symhash;

/* Hash table mapping symbol names to StgStablePtr */
static /*Str*/HashTable *stablehash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */
```


Each time an object file is loaded, a new `ObjectCode` node is added to the `objects` linked list and `symhash` is populated with a pointer for each symbol.



*Question:* What is `stablehash` used for? 



For shared libraries the code varies with each platform. On Windows a linked list of handles to opened DLLs is stored in a global variable:


```wiki
typedef
   struct _OpenedDLL {
      char*              name;
      struct _OpenedDLL* next;
      HINSTANCE instance;
   }
   OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;
```


To lookup a symbol one has to iterate `opened_dlls` and for each handle, lookup the symbol there.



For the ELF and Mach-O case, libraries are dlopen'd using RTLD\_GLOBAL and later accessed using the program's dl-handle. This is stored in:


```wiki
static void *dl_prog_handle;
```


A possible solution would be to put all these variables in a datastructure:


```wiki
typedef struct _ObjLinkerState {
  /* Hash table mapping symbol names to Symbol */
  /*Str*/HashTable *symhash;

  /* Hash table mapping symbol names to StgStablePtr */
  /*Str*/HashTable *stablehash;

  /* List of currently loaded objects */
  ObjectCode *objects = NULL;	/* initially empty */

#if defined(OBJFORMAT_PEi386)
  OpenedDLL* opened_dlls = NULL;
#endif

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
  void *dl_prog_handle;
#endif
} ObjLinkerState;
```


and add to `PersistentLinkerState` a `ForeignPtr` to a malloc'd `ObjLinkerState`.



*Question:* Will this work in the case of ELF shared libraries if two instances of GHC load two different (conflicting) versions of a .so? My impression is that it won't and that the workaround would be to use a linked list of handles like is done with DLLs.



*Question:* There are other platform-specific global variables defined in [rts/Linker.c](/trac/ghc/browser/ghc/rts/Linker.c) that I don't know how should be handled:


- This one seems to be a constant that may be overridden during initialization:

  ```wiki
  static void *mmap_32bit_base = (void *)MMAP_32BIT_BASE_DEFAULT
  ```

>
>
> I guess it can continue being a global variable.
>
>

- No idea about these ones:

  ```wiki
  static Elf_Addr got[GOT_SIZE];
  static unsigned int gotIndex;
  static Elf_Addr gp_val = (Elf_Addr)got;
  ```
- No idea about these ones either:

  ```wiki
  static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
  static unsigned int functionTableIndex;
  ```