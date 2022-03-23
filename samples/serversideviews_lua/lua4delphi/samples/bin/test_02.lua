require "lua_taival.datasets"

local s = open_dataset(people);
s.first();
local f = io.open("test_02.txt", "w")
while not s.eof() do
  f:write(s.get('first_name').." "..s.get('last_name').." "..s.get('born_date').." "..s.get("is_male").."\n")
  s.movenext()
end;
s.close()

f:write(tostring(s.is_open()))
s.open()
f:write(tostring(s.is_open()))
s.movenext()
f:write(tostring(s.is_open()))
s.first()
f:write(tostring(s.is_open()))
s.movenext()
f:write(tostring(s.is_open()))
s.close()
f:write(tostring(s.is_open()))
f:close()
