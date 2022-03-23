io.write [[
<html>
<head>
<title>Hello DWS</title>
</head>
<body>
<h2>Hello DWS</h2>
<p>This page was generated at <?pas=FormatDateTime('hh:nn:ss.zzz', Now)?>.</p>
<p>Some test links<ul>
<li><a href="requestInfo.dws">blank request info</a></li>
<li><a href="requestInfo.dws?p=alpha&b=beta&c=gamma+delta">parameterized request info</a></li>
</ul></p>
<p>Some facts:<ul>]]

function print_facts(i)
  print(tostring(i) .. ' + ' .. tostring(i) .. ' = ' .. tostring(i+i))
end

for i = 1,5 do
  print('<li>');
  print_facts(i);   
  print('</li>');
end

io.write [[

</ul></p>
</body>
</html>]]