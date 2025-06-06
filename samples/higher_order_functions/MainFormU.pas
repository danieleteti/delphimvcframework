// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MVCFramework.Utils,
  System.Generics.Collections;

type
  TMainForm = class(TForm)
    btnMapAddStars: TButton;
    lbMap: TListBox;
    btnFilterBetween: TButton;
    lbFilter: TListBox;
    btnReduceSum: TButton;
    lbReduce: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    btnReduceMin: TButton;
    btnReduceMul: TButton;
    btnReduceMax: TButton;
    Label3: TLabel;
    btnFilterOdd: TButton;
    btnFilterEven: TButton;
    btnMapCapitalize: TButton;
    btnForEach: TLabel;
    btnJustLoop: TButton;
    lbForEach: TListBox;
    btnForEachWithException: TButton;
    btnMap2: TButton;
    btnMap2Array: TButton;
    btnFilterEnumerable: TButton;
    procedure btnMapAddStarsClick(Sender: TObject);
    procedure btnReduceSumClick(Sender: TObject);
    procedure btnFilterBetwenClick(Sender: TObject);
    procedure btnReduceMulClick(Sender: TObject);
    procedure btnReduceMinClick(Sender: TObject);
    procedure btnReduceMaxClick(Sender: TObject);
    procedure btnFilterOddClick(Sender: TObject);
    procedure btnFilterEvenClick(Sender: TObject);
    procedure btnMapCapitalizeClick(Sender: TObject);
    procedure btnJustLoopClick(Sender: TObject);
    procedure btnForEachWithExceptionClick(Sender: TObject);
    procedure btnMap2Click(Sender: TObject);
    procedure btnMap2ArrayClick(Sender: TObject);
    procedure btnFilterEnumerableClick(Sender: TObject);
  private
    function GetFormButtons(): TList<TButton>;
    procedure FillList(Data: TArray<String>; AStrings: TStrings); overload;
    procedure FillList(Data: TArray<Integer>; AStrings: TStrings); overload;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  STRINGS_ARRAY: TArray<String> = ['daniele',
                                     'debora',
                                     'mattia',
                                     'jake',
                                     'amy',
                                     'george',
                                     'joseph',
                                     'katrine',
                                     'max',
                                     'mary'];

  INTEGERS_ARRAY: TArray<Integer> = [1,2,3,4,5,6,7,8,9,10];

function GetComponentArrayOfData(const Form: TForm): TArray<TComponent>;
begin
  SetLength(Result, Form.ComponentCount);
  var lIdx := 0;
  for var lComp in Form do
  begin
    Result[lIdx] := lComp;
    Inc(lIdx);
  end;
end;


procedure TMainForm.btnFilterBetwenClick(Sender: TObject);
var
  OutputData: TArray<Integer>;
  FilterFunc: TPredicateClosure<Integer>;
begin
  FilterFunc := function(const Item: Integer): boolean
    begin
      Result := (Item > 2) and (Item < 8)
    end;
  OutputData := HigherOrder.Filter<Integer>(INTEGERS_ARRAY, FilterFunc);
  FillList(OutputData, lbFilter.Items);
end;

procedure TMainForm.btnFilterEnumerableClick(Sender: TObject);
var
  InputData, OutputData: TArray<TComponent>;
  Names: TArray<String>;
begin
  InputData := GetComponentArrayOfData(Self);
  OutputData := _.Filter<TComponent>(InputData,
    function(const Item: TComponent): Boolean
    begin
      Result := Item is TButton;
    end);
  Names := _.Map<TComponent, String>(OutputData,
    function(const Item: TComponent): String
    begin
      Result := Item.Name;
    end);
  FillList(Names, lbFilter.Items);
end;

procedure TMainForm.btnFilterEvenClick(Sender: TObject);
begin
  var lOutputData := _.Filter<Integer>([1,2,3,4,5,6,7,8,9,10],
    function(const Item: Integer): boolean
    begin
      Result := Item mod 2 = 0;
    end);
  FillList(lOutputData, lbFilter.Items);
end;

procedure TMainForm.btnFilterOddClick(Sender: TObject);
var
  OutputData: TArray<Integer>;
begin
  OutputData := _.Filter<Integer>(INTEGERS_ARRAY, function(const Item: Integer): boolean
                                                  begin
                                                    Result := Item mod 2 > 0;
                                                  end);
  FillList(OutputData, lbFilter.Items);
end;

procedure TMainForm.btnForEachWithExceptionClick(Sender: TObject);
begin
  lbForEach.Clear;
  _.ForEach<Integer>(INTEGERS_ARRAY, procedure(const Item: Integer)
                                     begin
                                       if Item = 5 then
                                       begin
                                         raise Exception.Create('This is an error!');
                                       end;
                                     end);
end;

procedure TMainForm.btnJustLoopClick(Sender: TObject);
begin
  lbForEach.Clear;
  _.ForEach<String>(['daniele','debora','mattia','jake','amy'],
    procedure(const Item: String)
    begin
      lbForEach.Items.Add(Item);
    end);
end;

procedure TMainForm.btnMap2ArrayClick(Sender: TObject);
begin
  lbMap.Clear;
  var lInput: TArray<Integer>;

  SetLength(lInput, 10);
  for var I := 1 to Length(lInput) do
  begin
    lInput[I-1] := I * 10;
  end;

  var lListOfStr := _.Map<Integer, String>(
    lInput, function(const Item: Integer): String
            begin
              Result := '**' + Item.ToString;
            end);
  lbMap.Items.AddStrings(lListOfStr);
end;

procedure TMainForm.btnMap2Click(Sender: TObject);
begin
  lbMap.Clear;
  var lList := GetFormButtons();
  try
    var lListOfStr := _.Map<TButton, String>(
      lList, function(const Item: TButton): String
             begin
               Result := String(Item.Caption).ToUpper;
             end);
    lbMap.Items.AddStrings(lListOfStr);
  finally
    lList.Free;
  end;
end;

procedure TMainForm.btnMapAddStarsClick(Sender: TObject);
var
  OutputData: TArray<string>;
begin
  OutputData := _.Map<String>(STRINGS_ARRAY, function(const Item: String): String
                                               begin
                                                 Result := '*' + Item + '*';
                                               end);
  FillList(OutputData, lbMap.Items);
end;

procedure TMainForm.btnMapCapitalizeClick(Sender: TObject);
var
  OutputData: TArray<string>;
begin
  OutputData := _.Map<string>(STRINGS_ARRAY, function(const Item: String): String
                                             begin
                                               Result := String(Item.Chars[0]).ToUpper + Item.Substring(1);
                                             end);
  FillList(OutputData, lbMap.Items);
end;

procedure TMainForm.btnReduceMaxClick(Sender: TObject);
var
  OutputData: Integer;
begin
  OutputData := _.Reduce<Integer>(INTEGERS_ARRAY, function(const Item1, Item2: Integer): Integer
                                                  begin
                                                    if Item1 > Item2 then
                                                      Exit(Item1)
                                                    else
                                                      Exit(Item2);
                                                  end, 0);
  lbReduce.Items.Add('MAX: ' + OutputData.ToString);
end;

procedure TMainForm.btnReduceMinClick(Sender: TObject);
var
  OutputData: Integer;
begin
  OutputData := _.Reduce<Integer>(INTEGERS_ARRAY, function(const Item1, Item2: Integer): Integer
                                                  begin
                                                    if Item1 < Item2 then
                                                      Exit(Item1)
                                                    else
                                                      Exit(Item2);
                                                  end, MaxInt);
  lbReduce.Items.Add('MIN: ' + OutputData.ToString);
end;

procedure TMainForm.btnReduceMulClick(Sender: TObject);
var
  OutputData: Integer;
begin
  OutputData := _.Reduce<Integer>(INTEGERS_ARRAY, function(const Item1, Item2: Integer): Integer
                                                  begin
                                                    Result := Item1 * Item2;
                                                  end, 1);
  lbReduce.Items.Add('MUL: ' + OutputData.ToString);
end;

procedure TMainForm.btnReduceSumClick(Sender: TObject);
var
  OutputData: Integer;
begin
  OutputData := _.Reduce<Integer>(INTEGERS_ARRAY, function(const Item1, Item2: Integer): Integer
                                                  begin
                                                    Result := Item1 + Item2;
                                                  end, 0);
  lbReduce.Items.Add('SUM: ' + OutputData.ToString);
end;

procedure TMainForm.FillList(Data: TArray<Integer>; AStrings: TStrings);
var
  lItem: Integer;
begin
  AStrings.Clear;
  for lItem in Data do
    AStrings.Add(lItem.ToString);
end;


function TMainForm.GetFormButtons: TList<TButton>;
begin
  Result := TList<TButton>.Create;
  for var lControl in GetControls([ceftEnabled]) do
  begin
    if lControl is TButton then
    begin
      Result.Add(TButton(lControl));
    end;
  end;
end;

procedure TMainForm.FillList(Data: TArray<String>; AStrings: TStrings);
var
  lItem: string;
begin
  AStrings.Clear;
  for lItem in Data do
    AStrings.Add(lItem);
end;

end.
