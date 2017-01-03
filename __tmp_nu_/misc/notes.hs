--  reminder list comp:

let nums = [1,2,3,4,5]
in [ (-n) | n <- nums , let limit = 3 , n >= limit ]		==		[-3,-4,-5]
