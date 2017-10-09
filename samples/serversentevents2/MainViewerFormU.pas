unit MainViewerFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Threading,
  Vcl.StdCtrls;

type
  TForm11 = class(TForm)
    mmMessages: TMemo;
    lblMessage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ftask: ITask;
    fTerminate: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

uses
  JsonDataObjects;

{$R *.dfm}

procedure TForm11.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fTerminate := True;
  fTask.Wait;
end;

procedure TForm11.FormCreate(Sender: TObject);
begin
  fTerminate := False;
  fTask := TTask.Run(
    procedure
    var
      lHTTPClient: THTTPClient;
      lRes: IHTTPResponse;
      lPieces: TArray<string>;
      lLastEventID, lMessage, lPiece, lData, lRetry: string;
      lRetryMS: Integer;
    begin
      lLastEventID := '-1';
      lRetryMS := 100;
      lHTTPClient := THTTPClient.Create;
      try
        while not fTerminate do
        begin
          lRes := lhttpclient.Get('http://localhost:8080/api/notifications/messages', nil,
            [TNetHeader.Create('Last-Event-ID', lLastEventID)]);
          if fTerminate then
          begin
            break;
          end;
          if lRes.StatusCode = 200 then
          begin
            lMessage := lres.ContentAsString;
            lPieces := lMessage.Split([#13]);
            lLastEventID := '-1';
            lData := '';
            lRetryMS := 100;
            for lPiece in lPieces do
            begin
              if lPiece.StartsWith('id') then
              begin
                lLastEventID := lpiece.Split([':'])[1].Trim;
              end
              else if lPiece.StartsWith('data') then
              begin
                lData := lpiece.Substring(lPiece.IndexOf(':') + 1).TrimLeft;
              end
              else if lPiece.StartsWith('retry') then
              begin
                lRetry := lpiece.Split([':'])[1].Trim;
                lRetryMS := StrToIntDef(lRetry, 100);
              end;
            end;

            if not fTerminate then
            begin
              TThread.Synchronize(nil,
                procedure
                var
                  lJSON: TJsonObject;
                begin
                  if Assigned(Self) then
                  begin
                    mmMessages.Lines.Add(lData);
                    lJSON := TJsonObject.parse(lData) as TJsonObject;
                    try
                      lblMessage.Caption := lJSON.S['value'];
                    finally
                      lJSON.Free;
                    end;
                  end;
                end);
            end;
          end;
          Sleep(lRetryMS);
        end; // while
      finally
        lHTTPClient.Free;
      end;
    end).Start;
end;

end.
