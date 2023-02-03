sample = {}

local function getvalue (self, key, args)
  print ("retrieving", key)  
  if (key == 'pippo') then
    return function (p1,p2,p3,p4,p5,p6) return p2 end
  end
  return "__newindex "..key 
end

local function setvalue(self,key,value)
  print ("setting",key,"to",value)
  -- ... do some here with key / values
end

local function oncall(self, key, args)
  print("function calling...")
  print(key)
end

setmetatable(sample,{__index = getvalue, __newindex = setvalue, __call = oncall})

--sample['hello'] = 'world';
--print(sample['hello']);
--print(sample(123))

print(sample:pippo(23))