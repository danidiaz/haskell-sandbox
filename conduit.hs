{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (catch,(.))
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Category
import Control.Monad
import Control.Applicative
import Control.Error
import qualified Data.Text as T

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Network
import Data.ByteString.Char8 (pack)

sendFile fileName appData = sourceFile fileName $$ appSink appData

main = runResourceT $ runTCPClient (clientSettings 8000 (pack "localhost")) (sendFile "book.tex")


