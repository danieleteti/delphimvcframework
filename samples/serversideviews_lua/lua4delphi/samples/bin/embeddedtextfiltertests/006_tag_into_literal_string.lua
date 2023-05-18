io.write [[
Hello There
]] 
local x = "This is an open tag <?lua"

io.write [[

]]io.write(x)
io.write [[

]] 
local y = "This is a closing tag ?>"

io.write [[

]]io.write(y)
io.write [[
]]