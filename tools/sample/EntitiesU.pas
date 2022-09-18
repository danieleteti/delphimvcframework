// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit EntitiesU;

interface

uses
  MVCFramework.Serializer.Commons,
  MVCFramework.Nullables,
  MVCFramework.ActiveRecord,
  System.Classes;

type

  [MVCNameCase(ncLowerCase)]
  [MVCTable('COUNTRY')]
  TCountry = class(TMVCActiveRecord)
  private
    [MVCTableField('COUNTRY')]
    fCountry: String;
    [MVCTableField('CURRENCY')]
    fCurrency: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Country: String read fCountry write fCountry;
    property Currency: String read fCurrency write fCurrency;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('CUSTOMER')]
  TCustomer = class(TMVCActiveRecord)
  private
    [MVCTableField('CUST_NO')]
    fCustNo: Int32;
    [MVCTableField('CUSTOMER')]
    fCustomer: String;
    [MVCTableField('CONTACT_FIRST')]
    fContactFirst: NullableString;
    [MVCTableField('CONTACT_LAST')]
    fContactLast: NullableString;
    [MVCTableField('PHONE_NO')]
    fPhoneNo: NullableString;
    [MVCTableField('ADDRESS_LINE1')]
    fAddressLine1: NullableString;
    [MVCTableField('ADDRESS_LINE2')]
    fAddressLine2: NullableString;
    [MVCTableField('CITY')]
    fCity: NullableString;
    [MVCTableField('STATE_PROVINCE')]
    fStateProvince: NullableString;
    [MVCTableField('COUNTRY')]
    fCountry: NullableString;
    [MVCTableField('POSTAL_CODE')]
    fPostalCode: NullableString;
    [MVCTableField('ON_HOLD')]
    fOnHold: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CustNo: Int32 read fCustNo write fCustNo;
    property Customer: String read fCustomer write fCustomer;
    property ContactFirst: NullableString read fContactFirst write fContactFirst;
    property ContactLast: NullableString read fContactLast write fContactLast;
    property PhoneNo: NullableString read fPhoneNo write fPhoneNo;
    property AddressLine1: NullableString read fAddressLine1 write fAddressLine1;
    property AddressLine2: NullableString read fAddressLine2 write fAddressLine2;
    property City: NullableString read fCity write fCity;
    property StateProvince: NullableString read fStateProvince write fStateProvince;
    property Country: NullableString read fCountry write fCountry;
    property PostalCode: NullableString read fPostalCode write fPostalCode;
    property OnHold: NullableString read fOnHold write fOnHold;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('DEPARTMENT')]
  TDepartment = class(TMVCActiveRecord)
  private
    [MVCTableField('DEPT_NO')]
    fDeptNo: String;
    [MVCTableField('DEPARTMENT')]
    fDepartment: String;
    [MVCTableField('HEAD_DEPT')]
    fHeadDept: NullableString;
    [MVCTableField('MNGR_NO')]
    fMngrNo: NullableInt16;
    [MVCTableField('BUDGET')]
    fBudget: NullableCurrency;
    [MVCTableField('LOCATION')]
    fLocation: NullableString;
    [MVCTableField('PHONE_NO')]
    fPhoneNo: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property DeptNo: String read fDeptNo write fDeptNo;
    property Department: String read fDepartment write fDepartment;
    property HeadDept: NullableString read fHeadDept write fHeadDept;
    property MngrNo: NullableInt16 read fMngrNo write fMngrNo;
    property Budget: NullableCurrency read fBudget write fBudget;
    property Location: NullableString read fLocation write fLocation;
    property PhoneNo: NullableString read fPhoneNo write fPhoneNo;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('EMPLOYEE')]
  TEmployee = class(TMVCActiveRecord)
  private
    [MVCTableField('EMP_NO')]
    fEmpNo: Int16;
    [MVCTableField('FIRST_NAME')]
    fFirstName: String;
    [MVCTableField('LAST_NAME')]
    fLastName: String;
    [MVCTableField('PHONE_EXT')]
    fPhoneExt: NullableString;
    [MVCTableField('HIRE_DATE')]
    fHireDate: TDateTime {dtDateTimeStamp};
    [MVCTableField('DEPT_NO')]
    fDeptNo: String;
    [MVCTableField('JOB_CODE')]
    fJobCode: String;
    [MVCTableField('JOB_GRADE')]
    fJobGrade: Int16;
    [MVCTableField('JOB_COUNTRY')]
    fJobCountry: String;
    [MVCTableField('SALARY')]
    fSalary: Currency;
    [MVCTableField('FULL_NAME')]
    fFullName: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property EmpNo: Int16 read fEmpNo write fEmpNo;
    property FirstName: String read fFirstName write fFirstName;
    property LastName: String read fLastName write fLastName;
    property PhoneExt: NullableString read fPhoneExt write fPhoneExt;
    property HireDate: TDateTime {dtDateTimeStamp} read fHireDate write fHireDate;
    property DeptNo: String read fDeptNo write fDeptNo;
    property JobCode: String read fJobCode write fJobCode;
    property JobGrade: Int16 read fJobGrade write fJobGrade;
    property JobCountry: String read fJobCountry write fJobCountry;
    property Salary: Currency read fSalary write fSalary;
    property FullName: NullableString read fFullName write fFullName;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('EMPLOYEE_PROJECT')]
  TEmployeeProject = class(TMVCActiveRecord)
  private
    [MVCTableField('EMP_NO')]
    fEmpNo: Int16;
    [MVCTableField('PROJ_ID')]
    fProjId: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    property EmpNo: Int16 read fEmpNo write fEmpNo;
    property ProjId: String read fProjId write fProjId;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('JOB')]
  TJob = class(TMVCActiveRecord)
  private
    [MVCTableField('JOB_CODE')]
    fJobCode: String;
    [MVCTableField('JOB_GRADE')]
    fJobGrade: Int16;
    [MVCTableField('JOB_COUNTRY')]
    fJobCountry: String;
    [MVCTableField('JOB_TITLE')]
    fJobTitle: String;
    [MVCTableField('MIN_SALARY')]
    fMinSalary: Currency;
    [MVCTableField('MAX_SALARY')]
    fMaxSalary: Currency;
    [MVCTableField('JOB_REQUIREMENT')]
    fJobRequirement: NullableString;
    [MVCTableField('LANGUAGE_REQ')]
    fLanguageReq: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property JobCode: String read fJobCode write fJobCode;
    property JobGrade: Int16 read fJobGrade write fJobGrade;
    property JobCountry: String read fJobCountry write fJobCountry;
    property JobTitle: String read fJobTitle write fJobTitle;
    property MinSalary: Currency read fMinSalary write fMinSalary;
    property MaxSalary: Currency read fMaxSalary write fMaxSalary;
    property JobRequirement: NullableString read fJobRequirement write fJobRequirement;
    property LanguageReq: NullableString read fLanguageReq write fLanguageReq;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('PHONE_LIST')]
  TPhoneList = class(TMVCActiveRecord)
  private
    [MVCTableField('EMP_NO')]
    fEmpNo: NullableInt16;
    [MVCTableField('FIRST_NAME')]
    fFirstName: NullableString;
    [MVCTableField('LAST_NAME')]
    fLastName: NullableString;
    [MVCTableField('PHONE_EXT')]
    fPhoneExt: NullableString;
    [MVCTableField('LOCATION')]
    fLocation: NullableString;
    [MVCTableField('PHONE_NO')]
    fPhoneNo: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property EmpNo: NullableInt16 read fEmpNo write fEmpNo;
    property FirstName: NullableString read fFirstName write fFirstName;
    property LastName: NullableString read fLastName write fLastName;
    property PhoneExt: NullableString read fPhoneExt write fPhoneExt;
    property Location: NullableString read fLocation write fLocation;
    property PhoneNo: NullableString read fPhoneNo write fPhoneNo;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('PROJECT')]
  TProject = class(TMVCActiveRecord)
  private
    [MVCTableField('PROJ_ID')]
    fProjId: String;
    [MVCTableField('PROJ_NAME')]
    fProjName: String;
    [MVCTableField('PROJ_DESC')]
    fProjDesc: NullableString;
    [MVCTableField('TEAM_LEADER')]
    fTeamLeader: NullableInt16;
    [MVCTableField('PRODUCT')]
    fProduct: NullableString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ProjId: String read fProjId write fProjId;
    property ProjName: String read fProjName write fProjName;
    property ProjDesc: NullableString read fProjDesc write fProjDesc;
    property TeamLeader: NullableInt16 read fTeamLeader write fTeamLeader;
    property Product: NullableString read fProduct write fProduct;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('PROJ_DEPT_BUDGET')]
  TProjDeptBudget = class(TMVCActiveRecord)
  private
    [MVCTableField('FISCAL_YEAR')]
    fFiscalYear: Int32;
    [MVCTableField('PROJ_ID')]
    fProjId: String;
    [MVCTableField('DEPT_NO')]
    fDeptNo: String;
    [MVCTableField('QUART_HEAD_CNT')]
    fQuartHeadCnt: NullableInt32;
    [MVCTableField('PROJECTED_BUDGET')]
    fProjectedBudget: NullableCurrency;
  public
    constructor Create; override;
    destructor Destroy; override;
    property FiscalYear: Int32 read fFiscalYear write fFiscalYear;
    property ProjId: String read fProjId write fProjId;
    property DeptNo: String read fDeptNo write fDeptNo;
    property QuartHeadCnt: NullableInt32 read fQuartHeadCnt write fQuartHeadCnt;
    property ProjectedBudget: NullableCurrency read fProjectedBudget write fProjectedBudget;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('SALARY_HISTORY')]
  TSalaryHistory = class(TMVCActiveRecord)
  private
    [MVCTableField('EMP_NO')]
    fEmpNo: Int16;
    [MVCTableField('CHANGE_DATE')]
    fChangeDate: TDateTime {dtDateTimeStamp};
    [MVCTableField('UPDATER_ID')]
    fUpdaterId: String;
    [MVCTableField('OLD_SALARY')]
    fOldSalary: Currency;
    [MVCTableField('PERCENT_CHANGE')]
    fPercentChange: Double;
    [MVCTableField('NEW_SALARY')]
    fNewSalary: NullableDouble;
  public
    constructor Create; override;
    destructor Destroy; override;
    property EmpNo: Int16 read fEmpNo write fEmpNo;
    property ChangeDate: TDateTime {dtDateTimeStamp} read fChangeDate write fChangeDate;
    property UpdaterId: String read fUpdaterId write fUpdaterId;
    property OldSalary: Currency read fOldSalary write fOldSalary;
    property PercentChange: Double read fPercentChange write fPercentChange;
    property NewSalary: NullableDouble read fNewSalary write fNewSalary;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCTable('SALES')]
  TSales = class(TMVCActiveRecord)
  private
    [MVCTableField('PO_NUMBER')]
    fPoNumber: String;
    [MVCTableField('CUST_NO')]
    fCustNo: Int32;
    [MVCTableField('SALES_REP')]
    fSalesRep: NullableInt16;
    [MVCTableField('ORDER_STATUS')]
    fOrderStatus: String;
    [MVCTableField('ORDER_DATE')]
    fOrderDate: TDateTime {dtDateTimeStamp};
    [MVCTableField('SHIP_DATE')]
    fShipDate: NullableTDateTime {dtDateTimeStamp};
    [MVCTableField('DATE_NEEDED')]
    fDateNeeded: NullableTDateTime {dtDateTimeStamp};
    [MVCTableField('PAID')]
    fPaid: NullableString;
    [MVCTableField('QTY_ORDERED')]
    fQtyOrdered: Int32;
    [MVCTableField('TOTAL_VALUE')]
    fTotalValue: Currency;
    [MVCTableField('DISCOUNT')]
    fDiscount: Single;
    [MVCTableField('ITEM_TYPE')]
    fItemType: NullableString;
    [MVCTableField('AGED')]
    fAged: NullableCurrency;
  public
    constructor Create; override;
    destructor Destroy; override;
    property PoNumber: String read fPoNumber write fPoNumber;
    property CustNo: Int32 read fCustNo write fCustNo;
    property SalesRep: NullableInt16 read fSalesRep write fSalesRep;
    property OrderStatus: String read fOrderStatus write fOrderStatus;
    property OrderDate: TDateTime {dtDateTimeStamp} read fOrderDate write fOrderDate;
    property ShipDate: NullableTDateTime {dtDateTimeStamp} read fShipDate write fShipDate;
    property DateNeeded: NullableTDateTime {dtDateTimeStamp} read fDateNeeded write fDateNeeded;
    property Paid: NullableString read fPaid write fPaid;
    property QtyOrdered: Int32 read fQtyOrdered write fQtyOrdered;
    property TotalValue: Currency read fTotalValue write fTotalValue;
    property Discount: Single read fDiscount write fDiscount;
    property ItemType: NullableString read fItemType write fItemType;
    property Aged: NullableCurrency read fAged write fAged;
  end;

implementation

constructor TCountry.Create;
begin
  inherited Create;
end;

destructor TCountry.Destroy;
begin
  inherited;
end;

constructor TCustomer.Create;
begin
  inherited Create;
end;

destructor TCustomer.Destroy;
begin
  inherited;
end;

constructor TDepartment.Create;
begin
  inherited Create;
end;

destructor TDepartment.Destroy;
begin
  inherited;
end;

constructor TEmployee.Create;
begin
  inherited Create;
end;

destructor TEmployee.Destroy;
begin
  inherited;
end;

constructor TEmployeeProject.Create;
begin
  inherited Create;
end;

destructor TEmployeeProject.Destroy;
begin
  inherited;
end;

constructor TJob.Create;
begin
  inherited Create;
end;

destructor TJob.Destroy;
begin
  inherited;
end;

constructor TPhoneList.Create;
begin
  inherited Create;
end;

destructor TPhoneList.Destroy;
begin
  inherited;
end;

constructor TProject.Create;
begin
  inherited Create;
end;

destructor TProject.Destroy;
begin
  inherited;
end;

constructor TProjDeptBudget.Create;
begin
  inherited Create;
end;

destructor TProjDeptBudget.Destroy;
begin
  inherited;
end;

constructor TSalaryHistory.Create;
begin
  inherited Create;
end;

destructor TSalaryHistory.Destroy;
begin
  inherited;
end;

constructor TSales.Create;
begin
  inherited Create;
end;

destructor TSales.Destroy;
begin
  inherited;
end;

initialization

ActiveRecordMappingRegistry.AddEntity('country', TCountry);
ActiveRecordMappingRegistry.AddEntity('customer', TCustomer);
ActiveRecordMappingRegistry.AddEntity('department', TDepartment);
ActiveRecordMappingRegistry.AddEntity('employee', TEmployee);
ActiveRecordMappingRegistry.AddEntity('employee_project', TEmployeeProject);
ActiveRecordMappingRegistry.AddEntity('job', TJob);
ActiveRecordMappingRegistry.AddEntity('phone_list', TPhoneList);
ActiveRecordMappingRegistry.AddEntity('project', TProject);
ActiveRecordMappingRegistry.AddEntity('proj_dept_budget', TProjDeptBudget);
ActiveRecordMappingRegistry.AddEntity('salary_history', TSalaryHistory);
ActiveRecordMappingRegistry.AddEntity('sales', TSales);

end.