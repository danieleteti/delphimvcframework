unit ReceiverForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StompClient, StompTypes, ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button5: TButton;
    Timer1: TTimer;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    stomp: TStompClient;
    procedure rcv;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  stomp.Subscribe(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  rcv;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  stomp.Unsubscribe(Edit1.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  stomp.Disconnect;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  stomp.Subscribe(Edit1.Text, amAuto,
    StompUtils.StompHeaders.Add('activemq.subscriptionName', 'pippo'));
  //activemq.subscriptionName
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  stomp.Connect('localhost');
  Timer1.Enabled := CheckBox1.Checked;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Timer1.Enabled := CheckBox1.Checked;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Stomp.Connected then
    stomp.Disconnect;

  Timer1.Enabled := false;
  sleep(500);
  stomp.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  stomp := TStompClient.Create;
  stomp.ClientID := 'myclientid';
end;

procedure TForm1.rcv;
var
  fr: TStompFrame;
begin
  fr := stomp.Receive(100);
  if assigned(fr) then
  begin
    Memo1.Lines.Add(fr.Body);
    rcv;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  rcv;
end;

end.

