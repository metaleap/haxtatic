module X.DemoSimplest where

import X


registerX _project _tagconfig =
    Early$ \ (_page , _tagparams) ->
        Just "<h1>Hello World!</h1>"
