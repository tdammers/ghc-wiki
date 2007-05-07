# Type Functions: Interface


## Representation of family instances



The `IfaceSyn.IfaceData` variant of `IfaceDecl` contains a new `ifFamInst :: Maybe (IfaceTyCon, [IfaceType])` field that distinguishes ordinary data/newtype declarations from family instances.  In the latter case, the `(IfaceTyCon, [IfaceType])` value gives the family instance type.  In addition, each family instance is represented by a value of type `IfaceFamInst` that includes the instances rough match (i.e., name of the family type and a `[Maybe IfaceTyCon]` value that gives the outermost type constructor of each index argument for that instance) and refers to the type declaration for full details (as a class instance does with its dfun).



Moreover, much like class instances, family instance heads of a module are collected in the new `mi_fam_insts :: [IfaceFamInst]` field of `HscTypes.ModIface`.  This allows to enter the rough matches into the `ExternalPackageState` or `ModDetails`, when reading the interface, without having to parse the full tycon that represents the instance yet.  This in turn avoids pulling in everything that hangs of that tycon.


## Module dependencies due to family instances



To avoid superfluous (i.e., already previously performed overlap checks), each `ModIface` keeps a list of the modules it depends on that contain family instances.  This information is in the field `dep_finsts` of `HscTypes.Dependencies`.  Moreover, the field `mi_finsts` indicates whether there are an family instances in the current module.  When importing a module (in `RnNames.rnImportDecl`), this information (`mi_finsts` and `dep_finsts` combined) makes its way into `ImportAvails`.


