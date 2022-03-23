io.write [[
This is a normal text file
but now I'm start a Lua tag 
]]
  --here I can write simple Lua code
  for i = 1,5 do
    print("i = " .. tostring(i));
  end

io.write [[

And now back to the simple text mode
Expressions are supported too.
Do you know what is the result of 5*5?
5*5 = ]]io.write( 5*5 )
io.write [[

bye bye]]