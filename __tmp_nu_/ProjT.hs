{-# OPTIONS_GHC -Wall #-}
module ProjT where

import qualified Tmpl
import qualified Util
import Util ( (~:) , (>~) , (~.) , (~|) , noNull )

import qualified Data.Map.Strict
import qualified Text.Read



parseProjLines linessplits canparsestr =
    Data.Map.Strict.fromList$ linessplits>~foreach ~|fst~.noNull where
        foreach ("|T|":tname:tvalsplits) =
            ( tname~:Util.trim ,
                srcparsestr$ tvalsplits~:(Util.join ":")~:Util.trim )
        foreach _ =
            ( "" , "" )
        srcparsestr str
            | canparsestr
                && Util.startsWith str _parsestr_topen
                && Util.endsWith str _parsestr_tclose
            = case (Text.Read.readMaybe str) :: Maybe String of
                --  for open/close tokens other than " --- switch str above to:
                --  "\""++ (Util.crop (_parsestr_topen~>length) (_parsestr_tclose~>length) str) ++"\""
                Nothing -> str ; Just parsed -> parsed
            | otherwise
            = str


_parsestr_topen = "\""
_parsestr_tclose = "\""


srcLinesExpandMl rawsrc =
    --  original lines exposing {'|multi-line
    --  fragments|'} collapsed into single-line in-place {T|_hax_MlRepl_n|} placeholders ..
    ((mlchunked>~fst) ~: concat ~: lines) ++
        --  .. plus additional `T::_hax_MlRepl_n:"original-but-\n-escaped-and-quoted"`
        --  lines appended, supplying the original extracted&replaced multi-line fragments
        (mlchunked>~ snd~.mlwriteln)
    where
    mlwriteln ("",_) = ""
    mlwriteln (k,v) = "|T|:"++k++":"++v
    mlchunked = mlchunks>~forchunk where
        mlchunks = Util.indexed$ Util.splitUp ["{'|"] "|'}" rawsrc
        --  we splitUp above in order to now turn all {'|multi-line
        --  fragments|'} into single-line "Text.Read-able" ones,
        --  put into new T::key:value lines, with the original
        --  occurrence rewritten into {T|key|}
        forchunk (i , (str , "{'|")) =
            let tkey = "_hax_MlRepl_"++(show i) in
            ( Tmpl.tag_T++tkey++Tmpl.tag_Close , (tkey , str~:show) )
            -- if to enclose within other tokens than " and ", switch from str~:show to:
            -- _parsestr_topen++ (Util.crop 1 1 $str~:show) ++_parsestr_tclose))
        forchunk (_ , (str , _)) =
            (str , ("" , ""))



tagResolver ttags key =
    Data.Map.Strict.lookup key ttags
