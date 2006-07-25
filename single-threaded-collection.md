
Back to [GarbageCollectorNotes](garbage-collector-notes)


# An Overview of Single Threaded Collection



The layout of the Haskell heap, as described before, consists of multiple generations, where each generation consists of multiple steps:



[
http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/layout.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/layout.jpg)



Each step can be though of as consisting two lists. One of which is the list containing normal objects and the other one containing large objects. The large objects are objects that are greater than a block in size (4k). 



[
http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/step-layout.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/step-layout.jpg)



Each block in the normal object lists consists of many objects. These two lists of blocks contain all the objects that are part of this step. 



When GC happens, the copy collection algorithm works on each step that is in a generation that is going to be collected. You may want to familiarise yourself with the [
Copy Collection algorithm](http://www.brpreiss.com/books/opus5/html/page427.html) at this point. 



The algorithm works as follows: the existing list of objects is renamed to be an "old" list. This old list acts as the from space. Now we use some "magic information" that the runtime maintains to know which objects in this from space are guaranteed to be alive after garbage collection. This magic information is typically the program stack, references that we track from older generations to newer generations etc. 



The objects that are guaranteed to be alive are copied to a list of "new" objects. This "new" object list acts as out to-space. Once the objects are copied to the to-space, each copied objects is scanned to references to other objects. If a live objects refers to an object in the from-space we know that the referred object must also be one that should be live. Hence we copy the object to the to-space. 



To make this a little clearer, here is a diagram:



[
http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/st-scanning.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/st-scanning.jpg)



The above picture reveals a little more of the underlying complexity. One simple optimisation that can be made while copying objects is that if one knows that a certain object is of a type that contains no references to other objects, we need not scan it. Hence the to-space in the GHC GC is a list that grows in both directions. The dark green colour indicates blocks that contain objects that do not need to be scanned. As more objects that need not be scanned are encountered, they are copied into the left most block. More blocks are added if required and the list grows leftwards in this way. 



Every time an object with references is encountered it is copied into the rightmost block and new blocks are added to the right end of the list in this way. Objects yet to be scanned are indicated in red. In the middle of the list is the scan pointer which advances to the right. Every time the scan pointer encounters an object, all objects that it refers to which are still in the from-space are copied to either end of the list according its type (object with references go to the right side, those without go to the left side). Objects that have already been scanned have been indicated in a light green colour. 



So where does the GC spend most of its time? The GC spends most of its time in getting the scan pointer to catch up with the rightmost free pointer. In other words the GC spends most of its time in scanning objects. 



Hence a parallel GC should try to share this load of scanning objects. The distance between the scan pointer and the free pointer (in number of blocks) is the amount of work that the GC needs to do at any point. The essential idea behind work can be shared is as follows: if different threads could on different blocks in this region (on the red blocks) then they could essential scan objects in parallel. Since they need to synchronised access to the list only when an entire block full of objects is scanned, the amount of contention on the list should not be very high. 



[
http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/st-scanning-2.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/st-scanning-2.jpg)



To make this idea more concrete we go on to measure the block distance between the scan pointer and free pointer to see if there is enough work to be shared. 



[
http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/st-scanning-3.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/st-scanning-3.jpg)



The above copying process is what happens for the normal objects. The large objects are not copied to a "new" large object list but instead are merely unlinked from, the old list and relinked into the new list thus saving the overhead of copying.


