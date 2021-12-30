io.write [[
Hello There
]] 
local x = "Here there is an \" escaped double quote\""

io.write [[

]]io.write(x)
io.write [[

]] 
local y = "Here there is only one \" escaped double quote"

io.write [[

]]io.write(y)
io.write [[
]]