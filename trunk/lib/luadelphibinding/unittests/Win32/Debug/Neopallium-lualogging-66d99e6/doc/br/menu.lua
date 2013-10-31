
local menu = {
	{ "In&iacute;cio", "index.html",
		{ "Vis&atilde;o geral", "index.html#overview" },
		{ "Status", "index.html#status" },
		{ "Download", "index.html#download" },
		{ "Hist&oacute;rico", "index.html#history" },
		{ "Cr&eacute;ditos", "index.html#credits" },
		{ "Fale conosco", "index.html#contact" },
	},
	{ "Manual", "manual.html",
		{ "Introdu&ccedil;&atilde;o", "manual.html#introduction" },
		{ "Instala&ccedil;&atilde;o", "manual.html#installation" },
		{ "Objetos logger", "manual.html#logger" },
		{ "Exemplos", "manual.html#examples" },
	},
	{ "Appenders", "manual.html#appenders",
		{ "Console", "console.html" },
		{ "Arquivo", "file.html" },
		{ "SQL", "sql.html" },
		{ "Soquete", "socket.html" },
		{ "Email", "email.html" },
	},
	{ "Project", "https://github.com/Neopallium/lualogging",
		{ "Bug Tracker", "https://github.com/Neopallium/lualogging/issues" },
	},
	{ "Licen&ccedil;a", "license.html" },
}

local function dump_section(out, sect, page, depth)
	local name, url = sect[1], sect[2]
	local len = #sect
	local indent = ("\t"):rep(depth)
	-- list title.
	out:write(indent)
	if url == page then
		out:write("<li><strong>", name, "</strong>")
	else
		out:write('<li><a href="', url, '">', name, '</a>')
	end
	-- sub-sections
	if len >= 3 then
		local sub_indent = indent .. "\t"
		out:write("\n", sub_indent, "<ul>\n")
		for i=3,len do
			dump_section(out, sect[i], page, depth + 2)
		end
		out:write(sub_indent, "</ul>\n")
		out:write(indent, "</li>\n")
	else
		out:write("</li>\n")
	end
end

function dump_menu(out, page)
	out:write([[
<div id="navigation">
<h1>LuaLogging</h1>
	<ul>
]])
	local depth = 2
	for i=1,#menu do
		dump_section(out, menu[i], page, depth)
	end
	out:write([[
	</ul>
</div> <!-- id="navigation" -->

]])
end

