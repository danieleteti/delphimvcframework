unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REdis.Client, REdis.Commons, REdis.NetLib.Indy,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons, System.Actions,
  Vcl.ActnList, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TMainForm = class(TForm)
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    lstLogs: TListBox;
    ActionList1: TActionList;
    actRefresh: TAction;
    actClearLogs: TAction;
    ImageList1: TImageList;
    actFollowTail: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ppmRefreshInterval: TPopupMenu;
    N1Sec1: TMenuItem;
    N2Sec1: TMenuItem;
    N5Sec1: TMenuItem;
    N10Sec1: TMenuItem;
    N30seconds1: TMenuItem;
    GroupBox1: TGroupBox;
    EditRedisHostname: TEdit;
    EditRedisPort: TEdit;
    btnApply: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgLogsTypesClick(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actClearLogsExecute(Sender: TObject);
    procedure actFollowTailExecute(Sender: TObject);
    procedure N2Sec1Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure lstLogsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
  private
    FRedis: IRedisClient;
    function GetCurrentLogKey: string;
    procedure RefreshLogs;
    procedure SetRefreshInterval(const aIntervalMS: Int64);
    function Connected: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  REdis.Values, System.StrUtils;

{$R *.dfm}

const
  LOGGERPRO_KEY = 'loggerpro::logs';

procedure TMainForm.actClearLogsExecute(Sender: TObject);
begin
  FRedis.DEL([GetCurrentLogKey]);
  RefreshLogs;
end;

procedure TMainForm.actFollowTailExecute(Sender: TObject);
begin
  actFollowTail.Checked := not actFollowTail.Checked;
end;

procedure TMainForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  btnApply.Caption := ifthen(not Connected, 'Connect', 'Disconnect');
  EditRedisHostname.Enabled := not Connected;
  EditRedisPort.Enabled := not Connected;
  ToolBar1.Enabled := Connected;
end;

procedure TMainForm.actRefreshExecute(Sender: TObject);
begin
  RefreshLogs;

end;

procedure TMainForm.btnApplyClick(Sender: TObject);
begin
  if not Connected then
    FRedis := NewRedisClient(EditRedisHostname.Text, StrToInt(EditRedisPort.Text))
  else
    FRedis := nil;
end;

function TMainForm.Connected: Boolean;
begin
  Result := FRedis <> nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRedis := nil;
  actFollowTail.Checked := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshLogs;
  N2Sec1.Checked := True;
  SetRefreshInterval(2000);
end;

function TMainForm.GetCurrentLogKey: string;
begin
  Result := LOGGERPRO_KEY;
end;

procedure TMainForm.lstLogsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  lCanvas: TCanvas;
  lText: string;
  lRect: TRect;
  lBGColor, lFGColor: TColor;
begin
  lCanvas := (Control as TListBox).Canvas;
  lText := lstLogs.Items[index];
  if lText.Contains('ERROR') then
  begin
    lBGColor := clRed;
    lFGColor := clWhite;
  end
  else if lText.Contains('INFO') then
  begin
    lBGColor := clWhite;
    lFGColor := clBlack;
  end
  else if lText.Contains('WARNING') then
  begin
    lBGColor := clYellow;
    lFGColor := clRed;
  end
  else if lText.Contains('DEBUG') then
  begin
    lBGColor := clWhite;
    lFGColor := clBlue;
  end;
  lCanvas.Brush.Color := lBGColor;
  lCanvas.Font.Color := lFGColor;
  lCanvas.FillRect(Rect);
  lRect := Rect;
  lrect.Left := lrect.Left + 5;
  lCanvas.TextRect(lRect, lText, [TTextFormats.tfLeft, TTextFormats.tfEndEllipsis]);
end;

procedure TMainForm.rgLogsTypesClick(Sender: TObject);
begin
  RefreshLogs;
end;

procedure TMainForm.RefreshLogs;
var
  lArray: TRedisArray;
  lSavedItemIndex: Integer;
begin
  if not Connected then
  begin
    StatusBar1.Panels[2].Text := 'Not Connected';
    Exit;
  end;
  StatusBar1.Panels[2].Text := 'Connected';

  lSavedItemIndex := lstLogs.ItemIndex;
  lstLogs.Items.BeginUpdate;
  try
    lstLogs.Items.Clear;
    // from the first to the last element of the list
    lArray := FRedis.LRANGE(GetCurrentLogKey, 0, -1);
    if lArray.HasValue then
    begin
      lstLogs.Items.AddStrings(lArray.ToArray);
      if actFollowTail.Checked then
      begin
        lstLogs.ItemIndex := lstLogs.Items.Count - 1;
      end
      else
      begin
        if lstLogs.Count > lSavedItemIndex then
        begin
          lstLogs.ItemIndex := lSavedItemIndex;
        end;
      end;
    end;
  finally
    lstLogs.Items.EndUpdate;
  end;
  StatusBar1.Panels[0].Text := 'Last update: ' + DateTimeToStr(Now);
  StatusBar1.Panels[1].Text := 'Refresh Interval: ' + (Timer1.Interval div 1000).ToString + ' second/s';
end;

procedure TMainForm.SetRefreshInterval(const aIntervalMS: Int64);
begin
  Timer1.Enabled := False;
  Timer1.Interval := aIntervalMS;
  Timer1.Enabled := True;
  RefreshLogs;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  RefreshLogs;
end;

procedure TMainForm.N2Sec1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ppmRefreshInterval.Items.Count - 1 do
  begin
    ppmRefreshInterval.Items[i].Checked := False;
  end;
  (Sender as TMenuItem).Checked := True;
  SetRefreshInterval((Sender as TMenuItem).Tag * 1000);
end;

end.
