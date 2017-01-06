{-# OPTIONS_GHC -Wall #-}
module ProjT where

import Base
import qualified Tmpl
import qualified Util

import qualified Data.Map.Strict



parseProjLines linessplits canparsestr =
    Data.Map.Strict.fromList$ linessplits>~foreach ~|fst~.is where
        foreach ("|T|":tname:tvalsplits) =
            ( tname ~> Util.trim ,
                outputfor tname $ tvalsplits ~> (Util.join ":") ~> Util.trim )
        foreach _ =
            ( "" , "" )
        outputfor tname str =
            if canparsestr
                && Util.startsWith str _parsestr_topen
                && Util.endsWith str _parsestr_tclose
                --  for open/close tokens other than " --- switch `str` below to:
                --  "\""++ (Util.crop (_parsestr_topen~>length) (_parsestr_tclose~>length) str) ++"\""
            then let iferror = "{!T|" ++tname++ ":" ++str++ "|!}"
                    in Util.tryParseOr iferror str
            else str


_parsestr_topen = "\""
_parsestr_tclose = "\""


srcLinesExpandMl rawsrc =
    --  original lines exposing {``|multi-line
    --  fragments|``} collapsed into single-line in-place {T|_hax_multiline_n|} placeholders ..
    ((mlchunked>~fst) ~> concat ~> lines >~ Util.trim) ++
        --  .. plus additional `T::_hax_multiline_n:"original-but-\n-escaped-and-quoted"`
        --  lines appended, supplying the original extracted&replaced multi-line fragments
        (mlchunked >~ (snd~.mlwriteln))
    where
    mlwriteln ("",_) = ""
    mlwriteln (k,v) = "|T|:"++k++":"++v
    mlchunked = mlchunks>~forchunk where
        mlchunks = Util.indexed$ Util.splitUp id ["{``|"] "|``}" rawsrc
        --  we splitUp above in order to now turn all {``|multi-line
        --  fragments|``} into single-line "Text.Read-able" ones,
        --  put into new T::key:value lines, with the original
        --  occurrence rewritten into {T|key|}
        forchunk (i , (str , "{``|")) =
            let tkey = "_hax_multiline_"++(show i)
                escstr = Util.replaceAny Tmpl.tags_All (++"``:") str
            in ( Tmpl.tag_T++tkey++Tmpl.tag_Close , (tkey , show escstr) )
            -- if to enclose within other tokens than " and ", switch from str~>show to:
            -- _parsestr_topen++ (Util.crop 1 1 $str~>show) ++_parsestr_tclose))
        forchunk (_ , (str , _)) =
            (str , ("" , ""))



tagHandler ttags key =
    Data.Map.Strict.lookup key ttags
