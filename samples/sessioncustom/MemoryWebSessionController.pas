unit MemoryWebSessionController;

interface

uses classes, MVCFramework.Session;

type
  TWebSessionMemoryController = class(TWebSessionMemory)
  private
    FList: TStringList;
  public
    constructor Create(const SessionID: string; const Timeout: UInt64); override;
    destructor Destroy; override;
    property List: TStringList read FList;
  end;

implementation

{ TWebSessionMemoryController }

constructor TWebSessionMemoryController.Create(const SessionID: string; const Timeout: UInt64);
begin
  inherited Create(SessionID, Timeout);
  FList := TStringList.Create;
end;

destructor TWebSessionMemoryController.Destroy;
begin
  FList.Free;
  inherited;
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('memoryController', TWebSessionMemoryController);

end.
