module XRepeater where

import Pages
import Util

data Args = Args { nums :: (Int,Int), skip :: [Int], vars :: KeyVals, nvars :: [(String,[String])] } deriving (Read, Show)
data Cfg = Cfg { v :: KeyVals, c :: String } deriving (Read)

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ =
        map (\(num,i) -> Util.replace html $ replvars++[("{{_nr}}", show num),("{{_i}}", show i)]) $ map indices iter
        where
            args = read ("Args { "++argstr++" }") :: Args
            noskip n = not $ Util.isin n skips ; skips = (skip args)
            iter = filter noskip fromto
            indices n = (n,n-numskips) where numskips = (Util.count ((>) n) skips)
            replvars = rva++(concat replnv)++rva where rva = (repl $ vars args)
            repl v = map (\(k,v) -> ("{{"++k++"}}",v)) v
            replnv = map nv2r (nvars args) where
                nv2r (n,vs) = map (pern n vs) fromto
                pern n vs i = ("{{"++n++(show i)++"}}",val) where
                    val = if i>(length vs) then (Util.keyVal (vars args) key key) else (Util.whilein vs (i-1) null ((+) (-1)) ("{{"++key++"}}"))
                    key = "_"++n++"0"
            html = Util.replace (c cfg) $ [("{{_c}}",show $ length iter)] ++ replvars ++ (repl $ v cfg)
            (numfrom,numuntil) = nums args ; fromto = [numfrom..numuntil]
