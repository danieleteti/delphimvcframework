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
  Data.DB;

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
  lDataSet: TDataSet;
begin
  fModule.Connection.ExecSQL('select id, value, created_at from notifications order by id desc limit 1', lDataSet);
  try
    Result := TFullStatusEntity.Create;
    if lDataSet.Eof then
    begin
      Result.Id := -1;
      Result.Value := '';
      Result.PushedAt := '';
    end
    else
    begin
      Result := TFullStatusEntity.Create;
      Result.Id := lDataSet.FieldByName('id').AsInteger;
      Result.Value := lDataSet.FieldByName('value').AsString;
      Result.PushedAt := lDataSet.FieldByName('created_at').AsString;
    end;
  finally
    lDataSet.Free;
  end;
end;

procedure TStatusService.PersistStatus(aStatus: TNotificationEntity);
begin
  fModule.qryInsertNotification.ExecSQL('', [aStatus.Value]);
  TCurrentStatusEntity.GetInstance.SetStatus(GetLastPersistedStatus);
end;

end.
