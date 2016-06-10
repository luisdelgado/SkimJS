module Kind (Kind (..)) where

data Kind = Local
          | Global
          | Nulo deriving (Eq)

instance Show Kind where
   show Global = "Global"
   show Local = "Local"