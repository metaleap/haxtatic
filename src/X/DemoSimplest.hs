module X.DemoSimplest where

import X


registerX project tagconfig =
    Early$ \ (page , tagparams) ->
        Just "<h1>Hello World!</h1>"
