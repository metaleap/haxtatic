{-# OPTIONS_GHC -Wall #-}

module Bloks where


data Blok = Blok {
    name :: String,
    title :: String,
    desc :: String,
    atomFile :: String,
    dater :: String
}


toParseString projline =
    "Blok {"++projline++"}"
