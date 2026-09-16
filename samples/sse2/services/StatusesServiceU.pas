unit StatusesServiceU;

interface

uses BaseServiceU, MainDMU, EntitiesU;

type
  TStatusService = class(TBaseService)
  private
    fModule: TdmMain;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PersistStatus(aStatus: TNotificationEntity);
    function GetCurrentStatus: TCurrentStatusEntity;
    function GetLastPersistedStatus: TFullStatusEntity;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  Data.DB, FireDAC.Comp.Client;

{ TStatusService }

constructor TStatusService.Create;
begin
  inherited;
  fModule := TdmMain.Create(nil);
end;

destructor TStatusService.Destroy;
begin
  fModule.Free;
  inherited;
end;

function TStatusService.GetCurrentStatus: TCurrentStatusEntity;
var
  lInitializer: TFunc<TFullStatusEntity>;
begin
  lInitializer := function: TFullStatusEntity
    begin
      Result := Self.GetLastPersistedStatus;
      // Result := fModule.Connection.ExecSQLScalar
      // ('SELECT	TOP 1 CS.idStato FROM CambiStato CS ORDER BY CS.idRecord DESC');
    end;
  Result := TCurrentStatusEntity.GetInstance(lInitializer);
end;

function TStatusService.GetLastPersistedStatus: TFullStatusEntity;
var
  lQry: TFDQuery;
begin
  lQry := TFDQuery.Create(nil);
  try
    lQry.Connection := fModule.Connection;
    lQry.SQL.Text := 'select id, value, created_at from notifications order by id desc limit 1';
    lQry.Open;
    Result := TFullStatusEntity.Create;
    if lQry.Eof then
    begin
      Result.Id := -1;
      Result.Value := '';
      Result.PushedAt := '';
    end
    else
    begin
      Result.Id := lQry.FieldByName('id').AsInteger;
      Result.Value := lQry.FieldByName('value').AsString;
      Result.PushedAt := lQry.FieldByName('created_at').AsString;
    end;
  finally
    lQry.Free;
  end;
end;

procedure TStatusService.PersistStatus(aStatus: TNotificationEntity);
begin
  fModule.qryInsertNotification.ExecSQL('', [aStatus.Value]);
  TCurrentStatusEntity.GetInstance.SetStatus(GetLastPersistedStatus);
end;

end.
