unit MainFormClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, stompclient, StompTypes;

type
  TForm5 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    tmr: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Memo2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    stomp: IStompClient;
    roomname: string;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation


{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
begin
  roomname := '/topic/' + Edit2.Text;
  stomp := TStompClient.Create;
//  stomp.SetUserName('admin');
//  stomp.SetPassword('password');
  stomp.Connect(Edit1.Text);

  //Setup for reading messages
  stomp.Subscribe(roomname, amClient);

  Button1.Enabled := False;
  Edit1.Enabled := False;
  Edit2.Enabled := False;
  Edit3.Enabled := False;
  Button2.Enabled := True;
  Memo2.Enabled := True;

  tmr.Enabled := true;
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  stomp.Send(roomname, Memo2.Lines.Text,
    StompUtils.NewHeaders
      .Add('sender', Edit3.Text)
      .Add('datetime', formatdatetime('yyyy/mm/dd hh:nn:ss', now))
      .Add(TStompHeaders.NewPersistentHeader(true))
      );
  Memo2.Lines.Clear;
end;

procedure TForm5.Memo2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 13) and not (ssCtrl in Shift) then
  begin
    key := 0;
    Button2.Click;
  end;
end;

procedure TForm5.tmrTimer(Sender: TObject);
var
  f: IStompFrame;
  fw: FLASHWINFO;
begin
  f := stomp.Receive(50);  //this should be done in a secondary thread
  if assigned(f) then
  begin
    Memo1.Lines.Add('[' + f.GetHeaders.Value('datetime') + ' ' + f.GetHeaders.Value('sender') + ']' + sLineBreak + f.GetBody);
    if (WindowState = wsMinimized) or (Application.ActiveFormHandle <> self.Handle) then
    begin
      fw.cbSize := SizeOf(FLASHWINFO);
      fw.hwnd := self.Handle;
      fw.dwFlags := FLASHW_ALL;
      fw.uCount := 5;
      fw.dwTimeout := 500;
      FlashWindowEx(fw);
    end;
  end;

end;

end.
