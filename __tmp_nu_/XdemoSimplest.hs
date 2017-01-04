{-# OPTIONS_GHC -Wall #-}
module XdemoSimplest where



registerX (_xname , _tname) (_cfgfullstr , _cfgsplitsall) (_cfgsplitat1st_prefix , _cfgsplitat1st_reststr) =
    renderer
    where

    renderer _ctxpage _argstr =
        "<h1>Hello world!</h1>"
