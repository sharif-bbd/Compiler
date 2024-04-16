//BEGIN Narrowing unconditionally should panic with unvalid cast (2pts)
let x = ((1 @ Any) @! Float)
let main = print(x)
//OUT
panic
//END
