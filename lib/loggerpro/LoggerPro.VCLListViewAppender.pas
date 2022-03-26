unit LoggerPro.VCLListViewAppender;
{ <@abstract(The unit to include if you want to use the @link(TVCLMemoLogAppender))
  @author(Daniele Teti) }

interface

uses
  LoggerPro,
  System.Classes,
  Vcl.ComCtrls;

type
  { @abstract(Appends formatted @link(TLogItem) to a TMemo in a VCL application) }
  TVCLListViewAppender = class(TLoggerProAppenderBase)
  private
    FLV: TListView;
    FMaxLogLines: Word;
    FLogFormat: string;
  public const
    { @abstract(Defines the default format string used by the @link(TLoggerProFileAppender).)
      The positional parameters are the followings:
      @orderedList(
      @itemSetNumber 0
      @item TimeStamp
      @item ThreadID
      @item LogType
      @item LogMessage
      @item LogTag
      )
    }
    DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-10s] %3:s [%4:s]';
    constructor Create(aLV: TListView; aMaxLogLines: Word = 500; aLogFormat: string = DEFAULT_LOG_FORMAT); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages;

{ TVCLMemoLogAppender }

constructor TVCLListViewAppender.Create(aLV: TListView; aMaxLogLines: Word; aLogFormat: string);
begin
  inherited Create;
  FLogFormat := aLogFormat;
  FLV := aLV;
  FMaxLogLines := aMaxLogLines;
end;

procedure TVCLListViewAppender.Setup;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FLV.Clear;
    end);
end;

procedure TVCLListViewAppender.TearDown;
begin
  // do nothing
end;

procedure TVCLListViewAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
begin
  lText := Format(FLogFormat, [datetimetostr(aLogItem.TimeStamp), aLogItem.ThreadID, aLogItem.LogTypeAsString,
    aLogItem.LogMessage, aLogItem.LogTag]);
  TThread.Queue(nil,
    procedure
    var
      Lines: integer;
    begin
      FLV.Items.BeginUpdate;
      try
        Lines := FLV.Items.Count;
        if Lines > FMaxLogLines then
        begin
          FLV.Items.Delete(0);
        end;
        FLV.AddItem(lText, nil)
      finally
        FLV.Items.EndUpdate;
      end;
      FLV.Scroll(0, FLV.Items.Count);
      SendMessage(FLV.Handle, EM_SCROLLCARET, 0, 0);
    end);
end;

end.
