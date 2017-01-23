module X.DemoSimplest where

import X


registerX project tag_config =
    Early$ \ (page , tag_args) ->
        Just "<h1>Hello World!</h1>"
