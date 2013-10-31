program LuaEmbedded;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.ioutils,
  LuaBind,
  LuaBind.Filters.Text;

var
  lua: TLuaEngine;
  filter: TLuaEmbeddedTextFilter;

begin
  try
    filter := TLuaEmbeddedTextFilter.Create;
    try
      filter.OutputFunction := 'io.write';
      filter.TemplateCode := TFile.ReadAllText(ParamStr(1));
      filter.Execute;
      TFile.WriteAllText(ChangeFileExt(ParamStr(1), '.lua'), filter.LuaCode);
    finally
      filter.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
