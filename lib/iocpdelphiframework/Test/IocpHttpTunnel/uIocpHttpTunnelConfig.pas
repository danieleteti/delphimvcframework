unit uIocpHttpTunnelConfig;

interface

uses
  Windows, Classes, SysUtils, Math, IoUtils, StrUtils, System.Generics.Collections, VaniConfig;

type
  THostEntry = record
    Host: string;
    Port: Word;
  end;

  TTunnelEntry = record
    SrcHost, DstHost: THostEntry;
  end;

  TIocpHttpTunnelConfig = class(TVaniConfig)
  public
    destructor Destroy; override;

    procedure InitDataStruct; override;
    procedure ReadConfig; override;
  public
    Port: Word;
    Timeout, Lifeout: Integer;
    TunnelList: TList<TTunnelEntry>;
  end;

var
  IocpHttpTunnelConfig: TIocpHttpTunnelConfig;
  
implementation

uses
  uGlobalVars, Iocp.Logger;

{ TIocpHttpTunnelConfig }

destructor TIocpHttpTunnelConfig.Destroy;
begin
  TunnelList.Free;
  inherited Destroy;
end;

procedure TIocpHttpTunnelConfig.InitDataStruct;
begin
  TunnelList := TList<TTunnelEntry>.Create;
end;

procedure TIocpHttpTunnelConfig.ReadConfig;
var
  S: string;
  StrList: TStringList;
  TunnelEntry: TTunnelEntry;
  i, j: Integer;
begin
  Port := FIniFile.ReadInteger('Server', 'Port', 80);
  Timeout := FIniFile.ReadInteger('Server', 'Timeout', 0);
  Lifeout := FIniFile.ReadInteger('Server', 'Lifeout', 0);
  StrList := TStringList.Create;
  try
    FIniFile.ReadSectionValues('Tunnel', StrList);
    TunnelList.Clear;
    for S in StrList do
    begin
      i := Pos('=', S);
      if (i < 1) then Continue;
      TunnelEntry.SrcHost.Host := Copy(S, 1, i - 1);
      Inc(i);
      j := PosEx(':', S, i);
      if (j > 0) then
      begin
        TunnelEntry.DstHost.Host := Copy(S, i, j - i);
        TunnelEntry.DstHost.Port := StrToIntDef(Copy(S, j + 1, Length(S)), 80);
      end else
      begin
        TunnelEntry.DstHost.Host := Copy(S, i, Length(S));
        TunnelEntry.DstHost.Port := 80;
      end;
      TunnelList.Add(TunnelEntry);
    end;
  finally
    StrList.Free;
  end;
end;

initialization
  IocpHttpTunnelConfig := TIocpHttpTunnelConfig.Create('', False);
  if not IocpHttpTunnelConfig.CheckConfig then
  begin
    AppendLog('配置文件不完整', ltWarning);
    Halt;
  end;

finalization
  IocpHttpTunnelConfig.Free;
  
end.
