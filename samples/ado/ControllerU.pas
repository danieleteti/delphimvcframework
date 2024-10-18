unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type

  [MVCPath('/api/people')]
  TMyController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetPeople;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, DataModuleU, Winapi.ActiveX;

procedure TMyController.GetPeople;
begin
  var lDM := TdmMain.Create(nil);
  try
    lDM.ADOQuery1.SQL.Text := 'select * from people';
    lDM.ADOQuery1.Open;
    Render(lDM.ADOQuery1, False);
  finally
    lDM.Free;
  end;
end;

procedure TMyController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  CoInitialize(nil);
end;

end.
