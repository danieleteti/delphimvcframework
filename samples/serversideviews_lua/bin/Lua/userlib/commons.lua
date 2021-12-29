function comma_value(amount)
  local formatted = amount
  while true do
    formatted, k = string.gsub(formatted, "^(-?%d+)(%d%d%d)", '%1,%2')
    if (k==0) then
      break
    end
  end
  return formatted
end

function htmltable_row(tt)
  local s = '<tr>'
	for k,v in pairs(tt) do
	  s = s ..'<td>' .. htmlize(tostring(v)) .. '</td>'
  end
	return s .. '</tr>'
end

function keys(t)
  local tt = {}
	for k,v in pairs(t) do
		table.insert(tt, k)
	end
	return tt
end

function round(val, decimal)
  if (decimal) then
    return math.floor( (val * 10^decimal) + 0.5) / (10^decimal)
  else
    return math.floor(val+0.5)
  end
end

function format_num(amount, decimal, prefix, neg_prefix)
  local str_amount,  formatted, famount, remain

  decimal = decimal or 2  -- default 2 decimal places
  neg_prefix = neg_prefix or "-" -- default negative sign

  famount = math.abs(round(amount,decimal))
  famount = math.floor(famount)

  remain = round(math.abs(amount) - famount, decimal)

        -- comma to separate the thousands
  formatted = comma_value(famount)

        -- attach the decimal portion
  if (decimal > 0) then
    remain = string.sub(tostring(remain),3)
    formatted = formatted .. "." .. remain ..
                string.rep("0", decimal - string.len(remain))
  end

        -- attach prefix string e.g '$'
  formatted = (prefix or "") .. formatted

        -- if value is negative then format accordingly
  if (amount<0) then
    if (neg_prefix=="()") then
      formatted = "("..formatted ..")"
    else
      formatted = neg_prefix .. formatted
    end
  end

  return formatted
end


function table_size(t)
  local c = 0
	for i in pairs(t) do
	  c=c+1
	end
	return c
end

function html_table(t, headers, celldumpers, table_attributes)
  local headers = headers or {}
	local celldumpers = celldumpers or {}
	local html = {}

	if table_size(headers) == 0 then --[[doesn't work]]
		local firstobj = t[1]
		for k in pairs(firstobj) do
		  headers[k] = k
		end
	end

	--[[calculate table attributes]]
	local table_attributes = table_attributes or {cellpadding="2", cellspacing="10"}
	local attrs = ""
	for k,v in pairs(table_attributes) do
	  attrs = attrs .. tostring(k) .. '="' .. tostring(v) .. '" '
	end

	table.insert(html, '<table ' .. attrs .. ' ><thead><tr>')

	for k,v in ipairs(headers) do
		table.insert(html, '<th>'..v.coltitle..'</th>')
	end

	table.insert(html, '</tr></thead><tbody>')
  local cell
	for k, a in pairs(t) do
	  table.insert(html, "<tr>")
		for idx, obj in ipairs(headers) do
			if celldumpers[obj.fieldname] then
				cell = tostring(celldumpers[obj.fieldname](obj.fieldname, a))
			else
				if obj.fieldname == nil then
					cell = '&nbsp;'
				else
					cell = tostring(a[obj.fieldname])
				end
			end
			table.insert(html, "<td>".. cell .."</td>")
		end
		table.insert(html, "</tr>")
	end
	table.insert(html, '</tbody></table>')
	return table.concat(html)
end

function lookup(fieldvalue, lookuptable, keyfield, descfield)
	for k,v in pairs(lookuptable) do
		if fieldvalue == v[keyfield] then
			return v[descfield]
		end
	end
 	return ' '
end

function fmtfloat(n)
	return string.format("%2.2f", n)
end


--- Pads str to length len with char from left
string.lpad = function(str, len, char)
	if char == nil then
		char = ' '
	end
  return string.rep(char, len - #str) .. str
end

