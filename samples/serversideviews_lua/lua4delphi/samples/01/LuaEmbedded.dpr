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
