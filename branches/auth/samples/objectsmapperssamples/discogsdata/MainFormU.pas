unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Log(Value: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses DiscogsClasses, ObjectsMappers, System.IOUtils, System.JSON, System.Generics.collections;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LTxt: string;
  LJObj: TJSONArray;
  LDiscogsReleases: TObjectList<TDiscogsCollectionRelease>;
  LRelease: TDiscogsCollectionRelease;
  LArtist: TDiscogsArtist;
  LFormat: TDiscogsFormat;
  LLabel: TDiscogsLabel;
begin
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
    LTxt := TFile.ReadAllTExt('..\..\Discogs_Folder_Releases.JSON');
    LJObj := TJSONObject.ParseJSONValue(LTxt) as TJSONArray;
    LDiscogsReleases := Mapper.JSONArrayToObjectList<TDiscogsCollectionRelease>(LJObj, True);
    try
      for LRelease in LDiscogsReleases do
      begin
        Log(''.PadRight(70, '-'));
        Log('ReleaseID:'.PadRight(20) + LRelease.instance_id.tostring);
        Log('Title:'.PadRight(20) + LRelease.basic_information.title);
        Log('Year:'.PadRight(20) + LRelease.basic_information.year.tostring);
        Log('[artists in the release]');
        for LArtist in LRelease.basic_information.artists do
        begin
          Log('-' + LArtist.Name);
        end;
        Log('[available formats]');
        for LFormat in LRelease.basic_information.formats do
        begin
          Log('-' + LFormat.Name);
        end;
        Log('[labels]');
        for LLabel in LRelease.basic_information.labels do
        begin
          Log('-' + LLabel.Name);
        end;
      end;
    finally
      LDiscogsReleases.Free;
    end;
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TMainForm.Log(Value: string);
begin
  Memo1.Lines.Add(Value);
end;

end.
