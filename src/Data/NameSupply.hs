module Data.NameSupply where

data TreePath = Start | TLeft TreePath | TRight TreePath
  deriving (Eq, Ord, Show)

type NameSupply = TreePath
type Name = TreePath

newNameSupply :: NameSupply
newNameSupply = Start

split :: NameSupply -> (NameSupply, NameSupply)
split s = (TLeft s, TRight s)

supplyName :: NameSupply -> Name
supplyName = id
