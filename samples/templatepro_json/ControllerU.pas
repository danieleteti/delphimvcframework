unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections, RandomUtilsU, Data.DB, MVCFramework.DataSet.Utils;

type
  [MVCPath]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function GetCustomers: String;
  end;

implementation

uses
  System.StrUtils, System.SysUtils, MVCFramework.Logger, MVCFramework.Utils, System.DateUtils,
  JsonDataObjects;


function TMyController.GetCustomers: String;
begin
  var lDS := ToFree<TDataSet>(GetPeople(15)); //15 random people we consider as customers
  var lJSON := ToFree<TJSONObject>(TJSONObject.Create);

  //calculate customers avg age
  lDS.First;
  var lAgeCumulative := 0;
  _.ForEachRecord(lDS,
    procedure(const DS: TDataSet)
    begin
      lAgeCumulative := lAgeCumulative + YearsBetween(Now, lDS.FieldByName('dob').AsDateTime);
    end);
  var lAvgAge := lAgeCumulative / lDS.RecordCount;
  lDS.First;
  // end - stats

  lJSON.A['customers'] := lDS.AsJDOJSONArray();
  lJSON.F['avg_age'] := lAvgAge;
  ViewData['data'] := lJSON;
  Result := RenderView('index');
end;

end.
