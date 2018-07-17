unit BaseControllerU;

interface

uses
  MVCFramework, BaseDataModuleU, FireDAC.Comp.Client, MVCFramework.Serializer.Commons;

type
  TBaseController = class(TMVCController)
  private
    FCommon: TBaseDataModule;

  public
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionNAme: string); override;
    function Connection: TFDConnection;
    function CommonModule: TBaseDataModule;

    // function CreateQuery(const SQL: String; Obj: TObject = nil): TFDQuery;
  end;

implementation

uses
  System.SysUtils;

function TBaseController.CommonModule: TBaseDataModule;
begin
  if not Assigned(FCommon) then
    FCommon := TBaseDataModule.Create(nil);
  Result := FCommon;
end;

function TBaseController.Connection: TFDConnection;
begin
  Result := CommonModule.Conn;
end;

// function TBaseController.CreateQuery(const SQL: String; Obj: TObject): TFDQuery;
// var
// qry: TFDQuery;
// begin
// qry := TFDQuery.Create(nil);
// try
// qry.Connection := Connection;
// qry.SQL.Text := SQL;
// qry.Prepare;
// if Assigned(Obj) then
// Mapper.ObjectToFDParameters(qry.Params, Obj);
// Result := qry;
// except
// FreeAndNil(qry);
// raise;
// end;
// end;

procedure TBaseController.OnAfterAction(Context: TWebContext; const AActionNAme: string);
begin
  inherited;
  FreeAndNil(FCommon);
end;

procedure TBaseController.OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean);
begin
  inherited;

end;

end.
