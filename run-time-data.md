
Sometimes a data file will be presented to an application and the application lacks the appropriate renderer to insert the de-serialised file contents into the currently running code.



One example of this is a web browser that can not handle all mime-types.  If the data file carries information on how to render it, then both the data file and renderer can be loaded dynamically and safely injected into type-safe code.



Here is an example:


```wiki
module Runtime where

import Data.Binary
import Data.Typeable

import Control.Monad

import System.IO 
import System.Environment
import System.Exit

data RenderState = RS Int
data Render a = R { runRender :: RenderState -> (a,RenderState) }

class Renderable a where
    render :: a -> Render b -> IO b

data Image1 a = Image1 [(a,a)]
data Image2 a = Image2 [a] [a]

instance (Typeable a, Binary a) => Binary (Image1 a) where
    put (Image1 zs) = do
                      put $ length zs
{-
                      case typeOf (fst $ head zs) of
                           typeOf (undefined :: Float)  -> put (0 :: Int) -- like Show a => String
                           typeOf (undefined :: Double) -> put (1 :: Int)
-}      
                      mapM_ (\(x,y) -> put x >> put y) zs
    get = do
          n <- get
{- why not be able to do a typecase on an enumerated value?
       all the values readable from a bytestream are concrete, thus can be made instances
       of Dynamic, but we don't know what type they will be when the program is compiled or run

          t <- get :: Get Int
          case t of                              
                 0 -> shift :: Float >>= a  -- here the `shift' call provides the
                 1 -> shift :: Double >>= a -- contents of a hole which get propagated up 
-}                                          --  -> type variable binding
          zs <- replicateM n $ do
                               x <- get {- :: a -}
                               y <- get {- :: a -} 
                               return (x,y)
          return $ Image1 zs

instance Binary a => Binary (Image2 a) where
    put (Image2 xs ys) = do
                         put $ length xs
                         mapM_ put xs
                         put $ length ys
                         mapM_ put ys
    get = do
          nx <- get
          xs <- replicateM nx get
          ny <- get
          ys <- replicateM ny get
          return $ Image2 xs ys

instance Show a => Renderable (Image1 a) where
    render (Image1 zs) (R r) = do
                               print zs
                               let (e,_) = r (RS 0)
                               return e

instance Show a => Renderable (Image2 a) where
    render (Image2 xs ys) (R r) = do
                                  print $ zip xs ys
                                  let (e,_) = r (RS 0)
                                  return e
    
loadFileAndRender :: FilePath -> Render a -> IO a
loadFileAndRender fn r = do 
                         x <- decodeFile fn {- MISSING TYPE ANNOTATION -} {- pathological production -}
                         {- decodeFile has monomorphic type on LHS
                                           polymorphic class type on RHS
                         x <- decodeFile fn ::  prompt <- delimited control, 
                                                          see definition of Binary instance -}
                         {- and what if we want to be able to use renderers and datatypes that
                            were not known about at the time of compilation?
                         c <- loadClassMethod "Renderable" "render" :: prompt -- requires `primTypingDeferUntilRuntime'
                            the push_prompt is the hole that the shift will fill at runtime 
                         e <- c x r :: prompt -- and the push_prompt triggers dictionary lookup -}
                         e <- render x r                                  {- pathological consumption -}
                         {- render has polymorphic type on LHS -}
                         return e
{-
    Ambiguous type variable `a' in the constraints:
      `Renderable a'
        arising from a use of `render'
                     at ../Haskell/Runtime/Runtime.hs:80:30-39
      `Binary a'
        arising from a use of `decodeFile'
                     at ../Haskell/Runtime/Runtime.hs:70:30-42
    Probable fix: add a type signature that fixes these type variable(s)
-}

main = do
       a <- getArgs
       e <- loadFileAndRender (head a) (R (\s -> (0,s)))
       when (e /= 0) $ exitWith $ ExitFailure e
```