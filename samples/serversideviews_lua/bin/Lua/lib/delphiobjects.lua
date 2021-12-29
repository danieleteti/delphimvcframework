function connect_to_delphi_object(ud)
  m = {
    pointer = ud,
  };

  meta = {
    __index = function (self, name)
	  if _delphi_is_a_property(ud, name) then
		  return _delphi_call_method(ud,name,{maxindex = -1})
	  else
		  return function(...)
			  local parent_table = self
			  local method_name = name
			  local methodargs = {}
			  local index = 0
			  for k,v in ipairs(arg) do
				methodargs['_'..tostring(index)] = v
				index = index + 1
			  end
			  methodargs['maxindex'] = index - 1
			  logger:debug("Ecco i parametri:")
			  logger:debug(methodargs)
			  return _delphi_call_method(ud,method_name,methodargs)
		  end
	  end
	end,

	__newindex = function (self, key, value)
		  logger:debug("setting "..key.." to "..tostring(value))
		  return _delphi_set_property(ud, key, value)
	end
  }

  setmetatable(m, meta);

  return m;
end

function create_delphi_object(ClassName, ...)
  return connect_to_delphi_object(_delphi_create_object(ClassName, ...))
end

