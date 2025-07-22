#ifndef UnitSetupPages
#define UnitSetupPages

[Code]
{************************************************************************}
{                                                                        }
{ InnoSetup Tools Library for Delphi Components                          }
{                                                                        }
{ Copyright (c) 2024-2025 Ethea S.r.l.                                   }
{                                                                        }
{ Original Code is Copyright (c) 2021-2025 Skia4Delphi Project.          }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
// unit Setup.Pages;

// interface

// uses
  #include ".\RADStudio.inc"

/// <summary> Create the RAD Studio versions choise page </summary>
procedure CreateRADStudioVersionsChoicePage(const AAfterID: Integer; const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion); forward;
/// <summary> Get selected RAD Studio versions </summary>
function GetSelectedRADStudioVersions: TRADStudioInfos; forward;

// implementation

// uses
  #include ".\Setup.Utils.inc"

/// <summary> Get selected RAD Studio versions in versions choise page </summary>
function _GetSelectedVersions: TRADStudioInfos; forward;
/// <summary> On versions choise page click </summary>
function _OnRADStudioVersionsPageNextButtonClick(ASender: TWizardPage): Boolean; forward;

var
  _FRADStudioVersionsChoicePage: TInputOptionWizardPage;
  _FRADStudioVersionsChoicePageOptions: TRADStudioInfos;
  _FSelectedRADStudioVersions: TRADStudioInfos;

procedure CreateRADStudioVersionsChoicePage(const AAfterID: Integer; const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion);
var
  I: Integer;
  LMinIndexSelected: Integer;
  LHasRADStudioVersionsParam: Boolean;
  LSubItem: string;
begin
  if WizardSilent then
    _FSelectedRADStudioVersions := ARADStudioInfos
  else
  begin
    LHasRADStudioVersionsParam := HasParam('/RADStudioVersions');
    _FRADStudioVersionsChoicePageOptions := ARADStudioInfos;
    _FRADStudioVersionsChoicePage := CreateInputOptionPage(AAfterID, CustomMessage('SetupPagesRADStudioVersionsChoiceTitle'), '',
      FmtMessage(CustomMessage('SetupPagesRADStudioVersionsChoiceMessage'), ['{#LibraryName}']), False, False);

    LMinIndexSelected := GetArrayLength(ARADStudioInfos);
    for I := GetArrayLength(ARADStudioInfos) - 1 downto 0 do
    begin
      if ARADStudioInfos[I].Status in [riToolchainNotSupported, riNormal] then
      begin
        LMinIndexSelected := I;
        if CompareRADStudioVersions(ARADStudioInfos[I].Version, AMaxVersionOfficiallySupported) <= 0 then
          Break;
      end;
    end;

    for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
    begin
      case ARADStudioInfos[I].Status of
        riNotFound: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, '', 0, False, False, False, False, nil);
        riNeedOpenFirst: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, CustomMessage('SetupPagesRADStudioVersionsChoiceAlertNeedOpenFirst'), 0, False, False, False, False, nil);
        riToolchainNotSupported,
        riNormal:
          begin
            if CompareRADStudioVersions(ARADStudioInfos[I].Version, AMaxVersionOfficiallySupported) > 0 then
              LSubItem := CustomMessage('SetupPagesRADStudioVersionsChoiceUnofficialSupport')
            else
              LSubItem := '';
            _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, LSubItem, 0, LHasRADStudioVersionsParam or (I >= LMinIndexSelected), True, False, False, nil);
          end;
      else
      end;
    end;
    _FRADStudioVersionsChoicePage.OnNextButtonClick := @_OnRADStudioVersionsPageNextButtonClick;
  end;
end;

function GetSelectedRADStudioVersions: TRADStudioInfos;
begin
  Result := _FSelectedRADStudioVersions;
end;

function _GetSelectedVersions: TRADStudioInfos;
var
  I: Integer;
begin
  Result := [];
  if _FRADStudioVersionsChoicePage <> nil then
  begin
    for I := 0 to GetArrayLength(_FRADStudioVersionsChoicePageOptions) - 1 do
    begin
      if _FRADStudioVersionsChoicePage.CheckListBox.ItemEnabled[I] and (_FRADStudioVersionsChoicePage.CheckListBox.State[I] = cbChecked) then
      begin
        SetArrayLength(Result, GetArrayLength(Result) + 1);
        Result[GetArrayLength(Result) - 1] := _FRADStudioVersionsChoicePageOptions[I];
      end;
    end;
  end;
end;

function _OnRADStudioVersionsPageNextButtonClick(ASender: TWizardPage): Boolean;
begin
  _FSelectedRADStudioVersions := _GetSelectedVersions;
  Result := GetArrayLength(_FSelectedRADStudioVersions) <> 0;
  if (not Result) and (not IsVerySilent) then
    Result := MsgBox(CustomMessage('SetupPagesNoneRADStudioVersionsSelectedMessage'), mbConfirmation, MB_YESNO or MB_DEFBUTTON1) = IDYES;
end;

// end.
#endif
