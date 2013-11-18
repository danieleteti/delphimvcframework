--require "dumper"
-- Define a shortcut function for testing
--function dump(...)
--  print(DataDumper(...), "\n---")
--end

function dump(o)
	if type(o) == 'table' then
		local s = '{ '
		for k,v in pairs(o) do
			if type(k) ~= 'number' then k = '"'..k..'"' end
			s = s .. '['..k..'] = ' .. dump(v) .. ','
		end
		return s .. '} '
	else
		return tostring(o)
	end
end

sample = {
  get = function(name)
    return "this is the name: "..name;
  end
}

local function __index (self, name)
  return function(...)
      local parent_table = self
	  local method_name = name	  
	  print("parent table: "..tostring(self));
	  print("method name: "..name);	  
	  for k,v in ipairs(arg) do
		print(k.."\t"..tostring(v))
	  end
  end  
end

local function __newindex(self,key,value)
  print ("setting",key,"to",value)
  -- ... do some here with key / values
end
setmetatable(sample,{__index = __index, __newindex = __newindex})
print(sample.get("daniele"))
c = sample.dget("daniele",4,true); --("daniele")
