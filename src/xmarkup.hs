module XMarkup where

import Pages


ext tagname fulltag = Pages.X [ Pages.Tmpl tagname apply ] where
    -- could also do:
    -- Html.out fulltag [("",argstr)] []
    -- but let's keep this as a lean template showcasing the simplicity of custom-coded X-tag providers
    apply _ argstr _ = [ "<"++fulltag++">"++argstr++"</"++fulltag++">" ]
