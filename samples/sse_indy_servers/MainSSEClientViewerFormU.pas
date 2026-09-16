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

  fSSEClient.OnEvent :=
    procedure(const AId, AEvent, AData: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(Self) and Assigned(MessagesMemo) then
            MessagesMemo.Lines.Add(Format('ID: %s; Event: %s; Data: %s',
              [AId, AEvent, AData]));
        end);
    end;

  fSSEClient.OnError :=
    procedure(const AError: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(Self) and Assigned(MessagesMemo) then
            MessagesMemo.Lines.Add('ERROR: ' + AError);
        end);
    end;

  fSSEClient.OnOpen :=
    procedure
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(Self) and Assigned(MessagesMemo) then
            MessagesMemo.Lines.Add('--- Connected ---');
        end);
    end;

  fSSEClient.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fSSEClient.Free;
end;

end.
