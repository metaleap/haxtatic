module Blogs where

import qualified Util


data Blog = Blog { name :: String, title :: String, nameAsCat :: Bool, desc :: String, atom :: String, df :: String } deriving (Read)


tmplMarkupSrc blogs src curbname =
    Util.replaceIn src (concat (map perblog blogs)) where
        perblog b = dis++cur where
            dis = [("{{B:Title:"++bn++"}}",bt),("{{B:Desc:"++bn++"}}",bd)]
            cur = if bn==curbname then [("{{B:Name:}}",bn),("{{B:Title:}}",bt),("{{B:Desc:}}",bd)] else []
            bn = name b ; bt = title b ; bd = desc b
