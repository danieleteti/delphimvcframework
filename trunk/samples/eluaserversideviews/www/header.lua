--[[
Here you cannot use eLua syntax byt plain Lua.
To generate text, use the _out function
]]
for i = 1,3 do
  _out('<h'..i..'>This is and header</h'..i..'>');
end
_out('<hr>');