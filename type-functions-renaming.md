# Type Functions: Renaming


## Phasing



GHC is organised such that class and type declarations are processed (during renaming and type checking) before any instance declarations are considered.  In the presence of associated types, instance declarations may contain type definitions.  In particular, the *data constructors* introduced by associated data declarations need to be brought into scope before we can rename any expressions.  Otherwise, the intution wrt. to phasing is that family declarations are handled in conjunction with vanilla algebraic data type declarations and family instances are handled together with class instance *heads*.  (NB: GHC splits the treatment of class instances into two phases, where instances heads are processed in the first and member function declarations in the second phase.)


## Renaming of indexed types


### Family declarations



Family declarations are renamed by `RnSource.rnFamily`, which is parametrised by a function that handles the binders (i.e., index variables) of the declaration.  This is so that we can use the same code for toplevel signatures and those in classes.  In the former case, the variables are in a defining position, whereas in classes they are in a usage position (as all index variables must be class parameters).


### Determining binders



Before the actual renaming starts, GHC builds an environment of all names bound by a declaration group.  These names go into the global `RdrName` environment and are determined by the function `RnNames.getLocalDeclBinders`.  We need to be careful in handling data and new type instances in this context, as all the data constructors they define need to go into this environment.


#### Type instances of associated types



To get hold of the data constructors of associated types, `RnNames.getLocalDeclBinders` now also traverses all instance declarations and descends into the associated type instances.


#### Family names in type instances



Family names in the heads of type instances are usage occurences, not binders.  However, we cannot just use `RnNames.lookupGlobalOccRn` as we are only now determining the binders to construct the global `RdrName` environment.  On the other hand, we cannot use `RnNames.newTopSrcBinder` either, although it produces the same name when called multiple times.  It does not handle the case, where we define an instance for an imported family.  Hence, we introduced a new function `RnEnv.lookupFamInstDeclBndr` that first attempts a lookup, similar to `RnEnv.lookupGlobalOccRn`, but if that fails, instead of raising an error, calls `newTopSrcBinder`.  If after all, there is no family declaration in scope, this will be picked up and properly reported during renaming by `RnSource.rnTyClDecl`.



Despite the effort we go to, to get the right `Name` for the family `RdrName`, `getLocalDeclBinders` does not return that name as part of the binders declared by the current binding group - otherwise, we would get a duplicate declaration error.  However, we use the `Name` to specify the correct name parent for the data constructors of the instance (see below).


### Renaming of type instances



There is little extra to be done for type instances.  The main difference between vanilla type declarations and family instance declarations is that, in the latter case, the `tcdTyPats` field is not `Nothing`.  We simply call `rnTyPats` on these fields, which traverses them in the usual way.  Moreover, we need to be careful with the family type constructor in the instance head: it is in an occurence position and needs to be looked up suitably, such that we get a nice "out of scope" error if no appropriate family is in scope.  By the same token, the family names needs to go into the set of free variables.  Finally, `RnSource.rnSrcInstDecl` invokes `RnSource.rnTyClDecl` on all associated types of an instance to rename them.


## Renaming of equational constraints



Renaming (by `RnTypes.rnPred`) and kind checking (by `TcHsType.kc_pred`) is straight forward.  Afterwards, `HsPred` is desugared into `TypeRep.PredType`, where the wellformedness of equational constraints in type contexts is further tested by `TcMType.check_pred_ty`; in particular, we require the type arguments to be rank 0.


## Name parents & importing and exporting



GHC, in `RnNames`, derives its knowledge of which names may appear in parenthesis after a type or class name in an import or export list by querying the name parent relation, as encoded in `RdrName.gre_par` of `RdrName.GlobalRdrElt`.  Hence, it is crucial that all the data constructors defined in instances of a family get the family name, not the name of the representation tycon, as their name parent.  We go to some effort in `RnNames.getLocalDeclBinders` to achieve this when renaming source.  We also , when sucking declarations from interface files in `LoadIface.loadDecl`, make the family the name parent for all implicit things of the declaration.



An extra complication with the name parent relation that arises due to associated types is that the name parent relation can have a depth of two (not just one as in H98).  Here is an example:


```wiki
class GMapKey a where
  data GMap a :: * -> *
instance GMapKey Int where
  data GMap Int v = GMapInt ...
```


The data constructor GMapInt's parent is GMap whose parent in turn is the class GMapKey; ie, GMapKey is GMapInt's grand parent.  In H98, data types have no parents (which is in some places in the code represented by making them their own parent).



We address this in `RnNames.filterImports` by extending the occurences environment `occ_env` by an extra, but optional parent component.  In the case of associated types, this new component gives the class in which the associated types has been declared.



Finally, to ensure that family instances put into interface files and associated type declarations are not duplicated on the top-level of an interface file, we need to take care in `TyCon.isImplicitTyCon` to guarantee that (a) associated family declarations are implicit and (b) family instance representation tycons are not.


