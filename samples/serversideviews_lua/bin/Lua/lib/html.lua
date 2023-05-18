html = {}

function html:input(type, value)
  return string.format('<input type="%s" value="%s"/>', type, value)
end