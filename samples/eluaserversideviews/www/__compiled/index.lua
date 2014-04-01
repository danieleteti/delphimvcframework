 require 'Lua.html.form'
_out [[

This is an eLua file
]]_out("Hello World" )
_out [[


]]_out(html:form_start("POST", "/customers") )
_out [[

]]_out(html:input("text", "Hello World") )
_out [[

]]_out(html:submit("Save") )
_out [[

]]_out(html:form_end() )
_out [[
]]