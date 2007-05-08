## Implementation notes for closure conversion


### General



Almost all of the code concerning closure conversion is in the module `ClosureConv` in the directory `vectorise/`.  This module exports the function `closureConvert`, which is invoked as part of the core-to-core passes right after the desugarer if the option `fclosure-conv` is provided.


### Actual conversion



There is a [separate description](data-parallel/closure-conversion/class-less) of the conversion scheme.  This abstract description uses the convention that the existance of an `Id`, `TyCon`, or `dataCon` name followed by `_CC` indicates whether we have a closure converted variant of the corresponding declaration.  In the concrete implementation this information is maintained in `UniqFM`s.


### Cross-module information



The vectorisation information relevant across individual modules is maintained as values of type `HscTypes.VectInfo` and `HscTypes.IfaceVectInfo`.  The former is the representation in `HscTypes.ModGuts`, `HscTypes.ModDetails`, and the `HscTypes.ExternalPackageState`; the latter is used in `HscTypes.ModIface`.  The conversion between the two forms is peformed in `MkIface` and `TcIface`, respectively



In the `ExternalPackageState`, we use the same approach to combine the `VectInfo` of the various modules from the `eps_PIT` in a single table as is used for class instances, family instances, and rules.  The corresponding field in `ExternalPackageState` is `eps_vect_info::!PackageVectInfo`.  The information in this field is extended by `LoadIface.loadInterface` along with the corresponding fields for instances and rules.



The function `HscTypes.hptVectInfo` computes the combined vectorisation information of a home package table.


