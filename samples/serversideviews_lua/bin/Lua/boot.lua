dump = require "Lua.lib.inspect"
json = require "Lua.lib.dkjson"
require "userlib.commons"

function elua_out(value)
  internal_elua_out(__stringbuilder, tostring(value))
end

function htmlize(s)
  s = s:gsub('&', '&amp;')
  s = s:gsub('<', '&lt;')
  s = s:gsub('>', '&gt;')
	if s == ' ' then
		s = '&nbsp;'
	end
  return s
end

function dumppre(value)
  return '<pre style="padding: 10px; border: thin #a0a0a0 solid; background-color: #f0f0f0">' .. htmlize(dump(value)) .. '</pre>'
end