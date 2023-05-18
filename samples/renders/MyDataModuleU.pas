// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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
// ***************************************************************************

unit MyDataModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Phys.IBBase,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  MVCFramework.Serializer.Commons;

type
  TMyDataModule = class(TDataModule)
    FDConnection1: TFDConnection;
    qryCustomers: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    qryCountry: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryCustomersCUST_NO: TIntegerField;
    qryCustomersCUSTOMER: TStringField;
    qryCustomersCONTACT_FIRST: TStringField;
    qryCustomersCONTACT_LAST: TStringField;
    qryCustomersPHONE_NO: TStringField;
    qryCustomersADDRESS_LINE1: TStringField;
    qryCustomersADDRESS_LINE2: TStringField;
    qryCustomersCITY: TStringField;
    qryCustomersSTATE_PROVINCE: TStringField;
    qryCustomersCOUNTRY: TStringField;
    qryCustomersPOSTAL_CODE: TStringField;
    qryCustomersON_HOLD: TStringField;
    qryCustomersCN_HELLO: TWideStringField;
    qryCustomersSR_HELLO: TWideStringField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

end.
