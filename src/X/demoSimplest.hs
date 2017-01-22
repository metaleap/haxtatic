module X.DemoSimplest where

import X


registerX _project _tag_config =
    Early$ \ (_page , _tag_args) ->
        Just "<h1>Hello World!</h1>"
