unit PeopleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type

  [MVCPath('/api')]
  TPeopleController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); override;
  public
    // Sample CRUD Actions for a "Customer" entity
    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPeople;

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPerson(id: Integer);

    [MVCPath('/people')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreatePerson;

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdatePerson(id: Integer);

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeletePerson(id: Integer);

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils,
  MVCFramework.ActiveRecord,
  FireDAC.Comp.Client, Entities, System.Generics.Collections;

// Sample CRUD Actions for a "Customer" entity
procedure TPeopleController.GetPeople;
begin
  Render(ObjectDict().Add('data', TMVCActiveRecord.All<TPerson>));
end;

procedure TPeopleController.GetPerson(id: Integer);
begin
  Render(ObjectDict().Add('data', TMVCActiveRecord.GetByPK<TPerson>(id)));
end;

procedure TPeopleController.OnAfterAction(AContext: TWebContext; const AActionName: string);
begin
  inherited;
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
end;

procedure TPeopleController.OnBeforeAction(AContext: TWebContext; const AActionName: string;
  var AHandled: Boolean);
var
  lConn: TFDConnection;
begin
  inherited;
  lConn := TFDConnection.Create(nil);
  lConn.ConnectionDefName := 'sampledb';
  ActiveRecordConnectionsRegistry.AddDefaultConnection(lConn, True);
end;

procedure TPeopleController.CreatePerson;
var
  lPerson: TPerson;
begin
  lPerson := Context.Request.BodyAs<TPerson>();
  try
    lPerson.Insert;
    Render201Created('/api/people/' + lPerson.id.value.ToString);
  finally
    lPerson.Free;
  end;
end;

// procedure TPeopleController.UpdatePerson(id: Integer);
// var
// lPerson: TPerson;
// lPhones: TObjectList<TPhone>;
// begin
// TMVCActiveRecord.CurrentConnection.StartTransaction;
// try
// lPerson := TMVCActiveRecord.GetByPK<TPerson>(id);
// try
// // "Phones" property is not deserialized
// // This call updates only the main object TPerson
// Context.Request.BodyFor<TPerson>(lPerson);
//
// // Now, deserialize only the Phones
// lPhones := Context.Request.BodyAsListOf<TPhone>('phones');
// try
// // Merge the TPerson.Phones data retrieved from the database
// // with the new Phone list deserialized from the request
// lPerson.MergePhones(lPhones);
// finally
// lPhones.Free;
// end;
// lPerson.Update; // update the while object
// Render204NoContent();
// finally
// lPerson.Free;
// end;
// TMVCActiveRecord.CurrentConnection.Commit;
// except
// TMVCActiveRecord.CurrentConnection.Rollback;
// raise;
// end;
// end;

procedure TPeopleController.UpdatePerson(id: Integer);
var
  lPerson: TPerson;
  lNewPerson: TPerson;
begin
  TMVCActiveRecord.CurrentConnection.StartTransaction;
  try
    lPerson := TMVCActiveRecord.GetByPK<TPerson>(id);
    try
      lNewPerson := Context.Request.BodyAs<TPerson>();
      try
        //implement your "merge" mechanism
        lPerson.Merge(lNewPerson);
        lPerson.Store;
      finally
        lNewPerson.Free;
      end;
      Render204NoContent();
    finally
      lPerson.Free;
    end;
    TMVCActiveRecord.CurrentConnection.Commit;
  except
    TMVCActiveRecord.CurrentConnection.Rollback;
    raise;
  end;
end;

procedure TPeopleController.DeletePerson(id: Integer);
var
  lPerson: TPerson;
begin
  TMVCActiveRecord.CurrentConnection.StartTransaction;
  try
    lPerson := TMVCActiveRecord.GetByPK<TPerson>(id);
    try
      lPerson.Delete;
      Render204NoContent();
    finally
      lPerson.Free;
    end;
    TMVCActiveRecord.CurrentConnection.Commit;
  except
    TMVCActiveRecord.CurrentConnection.Rollback;
    raise;
  end;
end;

end.
