module Util exposing (..)

allSame : List a -> Bool
allSame xs =
  case List.head xs of
    Nothing -> True
    Just x0 -> List.all (\x -> x == x0) xs

chunks : Int -> List a -> List (List a)
chunks n xs =
  if List.isEmpty xs 
  then [] 
  else List.take n xs :: chunks n (List.drop n xs)

find : (a -> Bool) -> List a -> Maybe a
find pred xs =
  case xs of
    [] -> Nothing
    x :: xs' -> if pred x then Just x else find pred xs'

