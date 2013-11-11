require "Lua.helper.view"
--[[require "Lua.delphi.datasets"]]
dump = require "Lua.inspect"
json = require "dkjson"

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