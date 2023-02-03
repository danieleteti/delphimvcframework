unit MainSSEClientViewerFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MVCFramework.SSEClient, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    MessagesMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fSSEClient: TMVCSSEClient;
    procedure OnSSEMessage(Sender: TObject; const MessageID: Integer; const Event, Data: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fSSEClient := TMVCSSEClient.Create('http://localhost:8080/stocks');
  fSSEClient.OnSSEEvent := OnSSEMessage;
  fSSEClient.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fSSEClient.Free;
end;

procedure TMainForm.OnSSEMessage(Sender: TObject; const MessageID: Integer;
  const Event, Data: string);
var
  lMessageID: Integer;
  lEvent, lData: string;
begin
  lMessageID:= MessageID;
  lEvent := Event;
  lData:= Data;
  TThread.Queue(nil,
  procedure
  begin
    if Assigned(Self) and Assigned(MessagesMemo) then
    begin
      MessagesMemo.Lines.Add(
      Format('ID: %d; Event: %s; Data: %s', [
        lMessageID,
        lEvent,
        lData
      ]));
    end;
  end)
end;

end.
