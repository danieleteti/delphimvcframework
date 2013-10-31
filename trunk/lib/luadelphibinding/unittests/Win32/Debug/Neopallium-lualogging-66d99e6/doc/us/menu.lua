
local menu = {
	{ "Home", "index.html",
		{ "Overview", "index.html#overview" },
		{ "Status", "index.html#status" },
		{ "Download", "index.html#download" },
		{ "Dependencies", "index.html#dependencies" },
		{ "History", "index.html#history" },
		{ "Credits", "index.html#credits" },
		{ "Contact", "index.html#contact" },
	},
	{ "Manual", "manual.html",
		{ "Introduction", "manual.html#introduction" },
		{ "Installation", "manual.html#installation" },
		{ "Logger objects", "manual.html#logger" },
		{ "Examples", "manual.html#examples" },
	},
	{ "Appenders", "manual.html#appenders",
		{ "Console", "console.html" },
		{ "File", "file.html" },
		{ "Rolling File", "rolling_file.html" },
		{ "SQL", "sql.html" },
		{ "Socket", "socket.html" },
		{ "Email", "email.html" },
	},
	{ "Project", "https://github.com/Neopallium/lualogging",
		{ "Bug Tracker", "https://github.com/Neopallium/lualogging/issues" },
	},
	{ "License", "license.html" },
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

