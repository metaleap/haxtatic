module XRepeater where

import qualified Pages
import qualified Util

data Args = Args { nums :: (Int,Int), alt :: [String], skip :: [Int], vars :: Util.KeyVals, nvars :: [(String,[String])] } deriving (Read, Show)
data Cfg = Cfg { v :: Util.KeyVals, p :: String, c :: String, ps :: String } deriving (Read)

ext tagname cfg = Pages.X [ Pages.Tmpl tagname apply ] where
    apply _ argstr _ =
        [pre] ++ (map per_elem $ map indices iter) ++ [post]
        where
            pre = prerepl $ p cfg ; html = prerepl $ c cfg ; post = prerepl $ ps cfg
            iter = filter noskip fromto
            indices n = (n,n-numfrom+1-numskips) where numskips = (Util.count ((>) n) skips)
            noskip n = notElem n skips ; skips = (skip args)
            per_elem (num,i) = Util.replaceIn html $ replvars++[
                ("{{_v}}", if usealt then (alts!!(i-1)) else (show num)),
                ("{{_nr}}", show num),("{{_i}}", show i)
                ]
            prerepl s = Util.replaceIn s $ [("{{_c}}",show $ length iter)] ++ (repl $ v cfg) ++ replvars
            usealt = lalt > 0 ; alts = alt args ; lalt = length alts
            args = read ("Args { "++argstr++" }") :: Args
            replvars = rva++(concat replnv)++rva where rva = (repl $ vars args)
            repl v = map (\(k,v) -> ("{{"++k++"}}",v)) v
            replnv = map nv2r (nvars args) where
                nv2r (n,vs) = map (pern n vs) fromto
                pern n vs i = ("{{"++n++(show i)++"}}",val) where
                    val = if i>(length vs) then (Util.keyVal (vars args) key key) else (Util.whileIn vs (i-1) null ((+) (-1)) ("{{"++key++"}}"))
                    key = "_"++n++"0"
            (numfrom,numuntil) = if usealt then (1,lalt) else (nums args) ; fromto = [numfrom..numuntil]
