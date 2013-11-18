html = {}

local function join_tables(t1, t2)
	 local t1 = t1 or {}
	 local t2 = t2 or {}
   for k,v in pairs(t2) do
      t1[k] = v
   end
   return t1
end

local function tag(tagname, systemopts, useropts)
	local opts = join_tables(systemopts, useropts)
	local s = '<' .. tagname .. ' '
	for k,v in pairs(opts) do
	  s = s .. string.format('%s="%s" ', k,v)
	end
	s = s .. ">"
	return s
end

function html:input(name, type, value, useropts)
  return tag('input', {["name"] = name, ["type"]= type, ["value"] = value}, useropts)
end

function html:submit(name, value, useropts)
  return tag('input', {["name"] = name, ["type"]= "submit", ["value"] = value}, useropts)
end

function html:button(value, useropts)
  return tag('input', {["type"]= "button", ["value"] = value}, useropts)
end

function html:form_start(name, method, action, useropts)
  return tag('form', {
		["name"] = name,
		["action"] = action,
		["method"] = method
		}, {})
end

function html:form_end()
  return "</form>"
end