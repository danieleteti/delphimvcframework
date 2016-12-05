unit MemoryWebSessionController;

interface

uses classes, MVCFramework.Session;

   type TWebSessionMemoryController = class(TWebSessionMemory)
   private
   protected
   public
      list:TStringList;
      constructor Create(const SessionID: string; const Timeout: UInt64); override;
      destructor Destroy; override;
   end;

implementation

{ TWebSessionMemoryController }

constructor TWebSessionMemoryController.Create(const SessionID: string; const Timeout: UInt64);
begin
   inherited Create(SessionID,Timeout);
   list:=TStringList.Create;
end;

destructor TWebSessionMemoryController.Destroy;
begin
   list.Free;
   inherited;
end;

initialization
   TMVCSessionFactory.GetInstance.RegisterSessionType('memoryController', TWebSessionMemoryController);

end.
