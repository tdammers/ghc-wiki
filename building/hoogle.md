# Making GHC's source code searchable with Hoogle



Hoogle is a great way to find the function you want, and it's easy to make Hoogle search your own GHC source code base.


- Build GHC from source. IIRC you just need to get it to build with the HADDOCK options on.) When done, it produces the following file:

  ```wiki
  $(TOP)/compiler/stage2/doc/html/ghc/ghc.txt
  ```

  The `ghc.txt` is the hoogle database. 

- Hoist the file out somewhere:

  ```wiki
  cp ghc.txt ~/
  ```

- If you want the search results to link to the standard GHC docs    rather than the files you built from, then change the line at the top of ghc.txt to something like

  ```wiki
  @url http://www.haskell.org/ghc/docs/7.4.1/html/libraries/ghc-7.4.1/
  ```

- Install hoogle

  ```wiki
  cabal install hoogle 
  ```

  and populate its default database:

  ```wiki
  ./cabal/bin/hoogle data
  ```

- Convert the hoogle database into .hoo

  ```wiki
  cd ~/
  ./cabal/bin/hoogle convert ghc.txt
  ```

  This should produce a file `ghc.hoo`

- Copy the .hoo file into the hoogle directory:

  ```wiki
  cp ~/ghc.hoo ~/.cabal/share/hoogle-4.2.2/databases/default.hoo
  ```

  (The above is odd -- it "blows out" the default hoogleDB file and replaces it with the GHC database)

- Fire up a hoogle server:

  ```wiki
  	sudo .cabal/bin/hoogle server --port=80 
  ```

- Point your browser to `localhost:80` and you're off!
