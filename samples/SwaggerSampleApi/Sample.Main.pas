{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Sample.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.TabControl
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    btnGenerateDelphiMVCController: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Memo2: TMemo;
    btnGenerateClient: TButton;
    procedure btnGenerateClientClick(Sender: TObject);
    procedure btnGenerateDelphiMVCControllerClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Swag.Doc,
  Sample.SwagDoc,
  Sample.SwagDoc.DelphiMVCFramework,
  Sample.SwagDoc.DelphiRESTClient
  ;

procedure TForm1.btnGenerateClientClick(Sender: TObject);
var
  vSampleDocApi: TSampleApiSwagDocBuilder;
  mvcFramework : TSwagDocToDelphiRESTClientBuilder;
begin
  vSampleDocApi := nil;
  mvcFramework := nil;
  try
    vSampleDocApi := TSampleApiSwagDocBuilder.Create;
    vSampleDocApi.DeployFolder := ExtractFilePath(ParamStr(0));
    vSampleDocApi.Generate;
    mvcFramework := TSwagDocToDelphiRESTClientBuilder.Create(vSampleDocApi.SwagDoc);
    memo2.Lines.Text := mvcFramework.Generate;
  finally
    FreeAndNil(mvcFramework);
    FreeAndNil(vSampleDocApi);
  end;
end;

procedure TForm1.btnGenerateDelphiMVCControllerClick(Sender: TObject);
var
  vSampleDocApi: TSampleApiSwagDocBuilder;
  mvcFramework : TSwagDocToDelphiMVCFrameworkBuilder;
begin
  vSampleDocApi := nil;
  mvcFramework := nil;
  try
    vSampleDocApi := TSampleApiSwagDocBuilder.Create;
    vSampleDocApi.DeployFolder := ExtractFilePath(ParamStr(0));
    vSampleDocApi.Generate;
    mvcFramework := TSwagDocToDelphiMVCFrameworkBuilder.Create(vSampleDocApi.SwagDoc);
    memo2.Lines.Text := mvcFramework.Generate;
  finally
    FreeAndNil(mvcFramework);
    FreeAndNil(vSampleDocApi);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  vSampleDocApi: TSampleApiSwagDocBuilder;
begin
  vSampleDocApi := TSampleApiSwagDocBuilder.Create;
  try
    vSampleDocApi.DeployFolder := ExtractFilePath(ParamStr(0));
    memo1.Lines.Text := vSampleDocApi.Generate;
  finally
    FreeAndNil(vSampleDocApi);
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
