{-# OPTIONS_GHC -Wall #-}

module ProjTxts where

import qualified Util
import Util ( (~:) , (>~) , (~.) , (~|) )

import qualified Data.Map.Strict
import qualified Text.Read



parseDefs linessplits canparsestr =
    \key -> Data.Map.Strict.findWithDefault ("{!T{"++key++"}!}") key ttags where
        ttags = Data.Map.Strict.fromList$
            linessplits>~persplit ~|fst~.Util.is where
                persplit ("T":"":tname:tvalsplits) =
                    ( tname~:Util.trim ,
                        srcparsestr$ tvalsplits~:(Util.join ":")~:Util.trim )
                persplit _ =
                    ( "" , "" )
                srcparsestr str
                    | canparsestr && Util.startsWith str "\"" && Util.endsWith str "\""
                        = case (Text.Read.readMaybe str) :: Maybe String of
                            Nothing -> str ; Just parsed -> parsed
                    | otherwise
                        = str



srcLinesExpandMl rawsrc =
    --  original lines exposing {'{multi-line
    --  fragments}'} collapsed into single-line in-place {T{_Hx_MlRepl_n}} placeholders ..
    ((mlchunked>~fst) ~: concat ~: lines) ++
        --  .. plus additional `T::_Hx_MlRepl_n:"original-but-\n-escaped-and-quoted"`
        --  lines appended, supplying the original extracted&replaced multi-line fragments
        (mlchunked>~ snd~.mlwriteln)
    where
    mlwriteln ("",_) = ""
    mlwriteln (k,v) = "T::"++k++":"++v
    mlchunked = mlchunks>~perchunk where
        mlchunks = Util.indexed$ Util.splitUp ["{'{"] "}'}" rawsrc
        --  we splitUp above in order to now turn all {'{multi-line
        --  fragments}'} into single-line "Text.Read-able" ones,
        --  put into new T::key:value lines, with the original
        --  occurrence rewritten into {T{key}}
        perchunk (i , (str , "{'{")) =
            let tkey = "_Hx_MlRepl_"++(show i) in
            ( "{T{"++tkey++"}}" , (tkey , "\"" ++(show str ~: (Util.truncate 1 1))++ "\""))
        perchunk (_ , (str , _)) =
            (str , ("" , ""))
