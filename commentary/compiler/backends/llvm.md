# The LLVM backend



David Terei wrote a new code generator for GHC which targets the LLVM compiler infrastructure. Most of the work was done as part of an honours thesis at the University of New South Wales under the supervision of Manuel Chakravarty. It was merged into GHC Head around May of 2010 and has been included in GHC since the 7.0 release.



Documentation:


- [Installing & Using](commentary/compiler/backends/llvm/installing)
- [Design & Implementation](commentary/compiler/backends/llvm/design)
- [LLVM Mangler](commentary/compiler/backends/llvm/mangler)
- [Bugs & Other Problems](commentary/compiler/backends/llvm/development-notes)
- [Porting GHC/LLVM to another platform](commentary/compiler/backends/llvm/gh-c_-llvm-porting)


Work in Progress:


- [SIMD instructions and LLVM](simd)
- [Improving Alias Analysis](commentary/compiler/backends/llvm/alias)


Future Ideas:


- [ToDo List of Sorts](commentary/compiler/backends/llvm/wip)
- [Replacing the Native Code Generator](commentary/compiler/backends/llvm/replacing-ncg)
- [
  David Terei blog post of LLVM-related projects](http://dterei.blogspot.com/2011/09/ghc-project-for-all.html)


Other information:


- The [
  thesis paper](http://www.cse.unsw.edu.au/~pls/thesis/davidt-thesis.pdf) which offers a detailed performance evaluation, as well as the motivation and design of the back-end.
- [
  Blog post](http://blog.llvm.org/2010/05/glasgow-haskell-compiler-and-llvm.html) on the LLVM blog about the backend.
- A more recent [
  paper](http://www.cse.unsw.edu.au/~chak/papers/TC10.html) submitted to the Haskell Symposium '10, gives updated design overview and performance numbers.
