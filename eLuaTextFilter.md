# Introduction #

LuaDelphiBinding allows to use eLua files. An eLua file is a text file with embedded Lua code in it, just like PHP or JSP.

All the eLua file starts in "Text" mode and to switch to Lua mode you need to put `<?lua` open tag. To close the Lua mode use `?>`.


# Sample #

Let's say you have the following eLua file:
```
This is a normal text file
but now I'm start a Lua tag 
<?lua
  --here I can write simple Lua code
  for i = 1,5 do
    print("i = " .. tostring(i));
  end
?>
And now back to the simple text mode
Expressions are supported too.
Do you know what is the result of 5*5?
5*5 = <?lua= 5*5 ?>
bye bye
```

and you want to convert it in executable Lua file, you can use the following Delphi code to convert the eLua in Lua.

```
program LuaEmbedded;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  LuaBind.Filters.Text,
  System.IOUtils;

var
  eLuaFilter: TLuaEmbeddedTextFilter;

begin
  eLuaFilter := TLuaEmbeddedTextFilter.Create;
  try
    eLuaFilter.OutputFunction := 'io.write';
    eLuaFilter.TemplateCode := TFile.ReadAllText(ParamStr(1));
    eLuaFilter.Execute;

    TFile.WriteAllText(TPath.ChangeExtension(ParamStr(1), '.lua'), eLuaFilter.LuaCode);
  finally
    eLuaFilter.Free;
  end;

end.
```

This snippet convert the eLua file passed as parameter to a plain Lua file.

The output file will be as below.

```
io.write [[
This is a normal text file
but now I'm start a Lua tag 
]]
  --here I can write simple Lua code
  for i = 1,5 do
    print("i = " .. tostring(i));
  end

io.write [[

And now back to the simple text mode
Expressions are supported too.
Do you know what is the result of 5*5?
5*5 = ]]io.write( 5*5 )
io.write [[

bye bye]]
```

This technique is used in DelphiMVCFramework to generate server side view using eLua.
If you want to declare variable, push Delphi objects ot Lua table to the Lua execution state, you can use one of the Declare**methods of TLuaBind.**

