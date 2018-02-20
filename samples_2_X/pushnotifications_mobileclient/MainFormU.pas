unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, System.Actions, FMX.ActnList, FMX.DialogService, System.Net.HTTPClient;

type
  TForm7 = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    ActionList1: TActionList;
    acLogin: TAction;
    acSend: TAction;
    procedure acLoginUpdate(Sender: TObject);
    procedure acSendUpdate(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acSendExecute(Sender: TObject);
  private
    FLogged: Boolean;
    FSendQueue: THTTPClient;
    FUserName: string;
    FPullThread: TThread;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses System.JSON;

{$R *.fmx}


const
  // BASEURL = 'http://192.168.3.75:8080';
  BASEURL = 'http://192.168.1.109:8080';

procedure TForm7.acLoginExecute(Sender: TObject);
begin
  FUserName := Edit1.Text;
  FLogged := not FUserName.Trim.IsEmpty;

  if FLogged then
  begin
    ListBox1.Items.Add('LOGGED AS ' + FUserName);
    ListBox1.Items.Add('---');
    FPullThread := TThread.CreateAnonymousThread(
      procedure
      var
        lHTTPClient: THTTPClient;
        lRes: IHTTPResponse;
        lContent: string;
        lJObj: TJSONObject;
        lJMessages: TJSONArray;
        I: Integer;
        lUserName: string;
        lMessage: string;
      begin
        lHTTPClient := THTTPClient.Create;
        lHTTPClient.ResponseTimeout := 30000;
        while FLogged do
        begin
          try
            lRes := lHTTPClient.Get(BASEURL + '/topics/test1/' + FUserName);
          except
            if lRes.StatusCode = 0 then
            begin
              Sleep(10000);
              Continue;
            end;
          end;
          if lRes.StatusCode = 200 then
          begin
            lContent := lRes.ContentAsString(TEncoding.UTF8);
            lJObj := TJSONObject.ParseJSONValue(lContent) as TJSONObject;
            try
              lJMessages := lJObj.GetValue('data') as TJSONArray;
              for I := 0 to lJMessages.Count - 1 do
              begin
                lUserName := lJMessages.Items[I].GetValue<TJSONString>('username').Value;
                lMessage := lJMessages.Items[I].GetValue<TJSONString>('message').Value;
                TThread.Synchronize(nil,
                  procedure
                  begin
                    ListBox1.Items.Add(TimeToStr(Time) + ' [' + lUserName + ']: ' +
                      lMessage.Trim);
                    ListBox1.SelectRange(
                      ListBox1.ItemByIndex(ListBox1.Items.Count - 1),
                      ListBox1.ItemByIndex(ListBox1.Items.Count - 1)
                      );
                  end);
              end;
            finally
              lJObj.Free;
            end;
          end;
        end;
      end);
    FPullThread.Start;
  end;
end;

procedure TForm7.acLoginUpdate(Sender: TObject);
begin
  acLogin.Enabled := not FLogged;
end;

procedure TForm7.acSendExecute(Sender: TObject);
var
  lBody: TStringStream;
  lResp: IHTTPResponse;
  lJObj: TJSONObject;
begin
  lBody := TStringStream.Create;
  try
    lJObj := TJSONObject.Create;
    try
      lJObj.AddPair('username', FUserName);
      lJObj.AddPair('message', Memo1.Lines.Text);
      lBody.WriteString(lJObj.ToJSON);
    finally
      lJObj.Free;
    end;
    lBody.Position := 0;
    lResp := FSendQueue.Post(BASEURL + '/topics/test1/' + FUserName, lBody);
    if lResp.StatusCode <> 200 then
      TDialogService.ShowMessage('Cannot send message: ' + lResp.StatusText);
  finally
    lBody.Free;
  end;
end;

procedure TForm7.acSendUpdate(Sender: TObject);
begin
  acSend.Enabled := FLogged;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  FSendQueue := THTTPClient.Create;
end;

end.
