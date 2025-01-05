/// Framework Core Low-Level Data Processing Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.data;

{
  *****************************************************************************

   Low-Level Data Processing Functions shared by all framework units
    - RTL TPersistent / TInterfacedObject with Custom Constructor
    - TSynPersistent* TSyn*List TSynLocker classes
    - TSynPersistentStore with proper Binary Serialization
    - INI Files and In-memory Access
    - Efficient RTTI Values Binary Serialization and Comparison
    - TDynArray and TDynArrayHashed Wrappers
    - Integer Arrays Extended Process
    - RawUtf8 String Values Interning and TRawUtf8List
    - Abstract Radix Tree Classes

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  classes,
  contnrs,
  types,
  sysutils,
  {$ifdef ISDELPHI}
  typinfo,  // circumvent Delphi inlining issues
  {$endif ISDELPHI}
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers;


{ ************ RTL TPersistent / TInterfacedObject with Custom Constructor }

type
    /// abstract parent class with a virtual constructor, ready to be overridden
  // to initialize the instance
  // - you can specify such a class if you need an object including published
  // properties (like TPersistent) with a virtual constructor (e.g. to
  // initialize some nested class properties)
  TPersistentWithCustomCreate = class(TPersistent)
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
  end;

  {$M+}
  /// abstract parent class with threadsafe implementation of IInterface and
  // a virtual constructor
  // - you can specify e.g. such a class to TRestServer.ServiceRegister() if
  // you need an interfaced object with a virtual constructor, ready to be
  // overridden to initialize the instance
  TInterfacedObjectWithCustomCreate = class(TInterfacedObject)
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
    /// used to mimic TInterfacedObject reference counting
    // - Release=true will call TInterfacedObject._Release
    // - Release=false will call TInterfacedObject._AddRef
    // - could be used to emulate proper reference counting of the instance
    // via interfaces variables, but still storing plain class instances
    // (e.g. in a global list of instances) - warning: use with extreme caution!
    procedure RefCountUpdate(Release: boolean); virtual;
  end;
  {$M-}


  /// an abstract ancestor, for implementing a custom TInterfacedObject like class
  // - by default, will do nothing: no instance would be retrieved by
  // QueryInterface unless the VirtualQueryInterface protected method is
  // overriden, and _AddRef/_Release methods would call VirtualAddRef and
  // VirtualRelease pure abstract methods
  // - using this class will leverage the signature difference between Delphi
  // and FPC, among all supported platforms
  // - the class includes a RefCount integer field
  TSynInterfacedObject = class(TObject, IUnknown)
  protected
    fRefCount: integer;
    // returns E_NOINTERFACE by default
    function VirtualQueryInterface(IID: PGuid; out Obj): TIntQry; virtual;
    // always return 1 for a "non allocated" instance (0 triggers release)
    function VirtualAddRef: integer;  virtual; abstract;
    function VirtualRelease: integer; virtual; abstract;
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      IID: TGuid; out Obj): TIntQry; {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
    function _AddRef: TIntCnt;       {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: TIntCnt;      {$ifdef OSWINDOWS}stdcall{$else}cdecl{$endif};
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
    /// the associated reference count
    property RefCount: integer
      read fRefCount write fRefCount;
  end;

  /// any TCollection used between client and server shall inherit from this class
  // - you should override the GetClass virtual method to provide the
  // expected collection item class to be used on server side
  // - another possibility is to register a TCollection/TCollectionItem pair
  // via a call to Rtti.RegisterCollection()
  TInterfacedCollection = class(TCollection)
  public
    /// you shall override this abstract method
    class function GetClass: TCollectionItemClass; virtual; abstract;
    /// this constructor will call GetClass to initialize the collection
    constructor Create; reintroduce; virtual;
  end;

  /// used to determine the exact class type of a TInterfacedObjectWithCustomCreate
  // - could be used to create instances using its virtual constructor
  TInterfacedObjectWithCustomCreateClass = class of TInterfacedObjectWithCustomCreate;

  /// used to determine the exact class type of a TPersistentWithCustomCreateClass
  // - could be used to create instances using its virtual constructor
  TPersistentWithCustomCreateClass = class of TPersistentWithCustomCreate;

  /// class-reference type (metaclass) of a TInterfacedCollection kind
  TInterfacedCollectionClass = class of TInterfacedCollection;


  /// interface for TAutoFree to register another TObject instance
  // to an existing IAutoFree local variable
  // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
  // up to the end-of-method -> you should not use TAutoFree for new projects :(
  IAutoFree = interface
    procedure Another(var objVar; obj: TObject);
    /// do-nothing method to circumvent the Delphi 10.4 IAutoFree early release
    procedure ForMethod;
  end;

  /// simple reference-counted storage for local objects
  // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
  // up to the end-of-method -> you should not use TAutoFree for new projects :(
  // - be aware that it won't implement a full ARC memory model, but may be
  // just used to avoid writing some try ... finally blocks on local variables
  // - use with caution, only on well defined local scope
  TAutoFree = class(TInterfacedObject, IAutoFree)
  protected
    fObject: TObject;
    fObjectList: array of TObject;
    // do-nothing method to circumvent the Delphi 10.4 IAutoFree early release
    procedure ForMethod;
  public
    /// initialize the TAutoFree class for one local variable
    // - do not call this constructor, but class function One() instead
    constructor Create(var localVariable; obj: TObject); reintroduce; overload;
    /// initialize the TAutoFree class for several local variables
    // - do not call this constructor, but class function Several() instead
    constructor Create(const varObjPairs: array of pointer); reintroduce; overload;
    /// protect one local TObject variable instance life time
    // - for instance, instead of writing:
    // !var
    // !  myVar: TMyClass;
    // !begin
    // !  myVar := TMyClass.Create;
    // !  try
    // !    ... use myVar
    // !  finally
    // !    myVar.Free;
    // !  end;
    // !end;
    // - you may write:
    // !var
    // !  myVar: TMyClass;
    // !begin
    // !  TAutoFree.One(myVar,TMyClass.Create);
    // !  ... use myVar
    // !end; // here myVar will be released
    // - warning: under FPC, you should assign the result of this method to a local
    // IAutoFree variable - see bug http://bugs.freepascal.org/view.php?id=26602
    // - Delphi 10.4 also did change it and release the IAutoFree before the
    // end of the current method, so we inlined a void method call trying to
    // circumvent this problem - https://quality.embarcadero.com/browse/RSP-30050
    // - for both Delphi 10.4+ and FPC, you may use with TAutoFree.One() do
    class function One(var localVariable; obj: TObject): IAutoFree;
      {$ifdef ISDELPHI104} inline; {$endif}
    /// protect several local TObject variable instances life time
    // - specified as localVariable/objectInstance pairs
    // - you may write:
    // !var
    // !  var1, var2: TMyClass;
    // !begin
    // !  TAutoFree.Several([
    // !    @var1,TMyClass.Create,
    // !    @var2,TMyClass.Create]);
    // !  ... use var1 and var2
    // !end; // here var1 and var2 will be released
    // - warning: under FPC, you should assign the result of this method to a local
    // IAutoFree variable - see bug http://bugs.freepascal.org/view.php?id=26602
    // - Delphi 10.4 also did change it and release the IAutoFree before the
    // end of the current method, and an "array of pointer" cannot be inlined
    // by the Delphi compiler, so you should explicitly call ForMethod:
    // !  TAutoFree.Several([
    // !    @var1,TMyClass.Create,
    // !    @var2,TMyClass.Create]).ForMethod;
    class function Several(const varObjPairs: array of pointer): IAutoFree;
    /// protect another TObject variable to an existing IAutoFree instance life time
    // - you may write:
    // !var
    // !  var1, var2: TMyClass;
    // !  auto: IAutoFree;
    // !begin
    // !  auto := TAutoFree.One(var1,TMyClass.Create);,
    // !  .... do something
    // !  auto.Another(var2,TMyClass.Create);
    // !  ... use var1 and var2
    // !end; // here var1 and var2 will be released
    procedure Another(var localVariable; obj: TObject);
    /// will finalize the associated TObject instances
    // - note that releasing the TObject instances won't be protected, so
    // any exception here may induce a memory leak: use only with "safe"
    // simple objects, e.g. mORMot's TOrm
    destructor Destroy; override;
  end;


  /// an interface used by TAutoLocker to protect multi-thread execution
  IAutoLocker = interface
    ['{97559643-6474-4AD3-AF72-B9BB84B4955D}']
    /// enter the mutex
    // - any call to Enter should be ended with a call to Leave, and
    // protected by a try..finally block, as such:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.Enter;
    // !  try
    // !    ... // thread-safe code
    // !  finally
    // !    fSharedAutoLocker.Leave;
    // !  end;
    // !end;
    procedure Enter;
    /// leave the mutex
    // - any call to Leave should be preceded with a call to Enter
    procedure Leave;
    /// will enter the mutex until the IUnknown reference is released
    // - using an IUnknown interface to let the compiler auto-generate a
    // try..finally block statement to release the lock for the code block
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var
    // !  LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with fSharedAutoLocker.ProtectMethod do
    // !  begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// gives an access to the internal low-level TSynLocker instance used
    function Safe: PSynLocker;
  end;

  /// reference-counted block code critical section
  // - you can use one instance of this to protect multi-threaded execution
  // - the main class may initialize a IAutoLocker property in Create, then call
  // IAutoLocker.ProtectMethod in any method to make its execution thread safe
  // - this class inherits from TInterfacedObjectWithCustomCreate so you
  // could define one published property of a mormot.core.interface.pas
  // TInjectableObject as IAutoLocker so that this class may be automatically
  // injected
  // - consider inherit from high-level TSynPersistentLock or call low-level
  // fSafe := NewSynLocker / fSafe^.DoneAndFreemem instead
  TAutoLocker = class(TInterfacedObjectWithCustomCreate, IAutoLocker)
  protected
    fSafe: TSynLocker;
  public
    /// initialize the mutex
    constructor Create; override;
    /// finalize the mutex
    destructor Destroy; override;
    /// will enter the mutex until the IUnknown reference is released
    // - as expected by IAutoLocker interface
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var
    // !  LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with fSharedAutoLocker.ProtectMethod do
    // !  begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// enter the mutex
    // - as expected by IAutoLocker interface
    // - any call to Enter should be ended with a call to Leave, and
    // protected by a try..finally block, as such:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.Enter;
    // !  try
    // !    ... // thread-safe code
    // !  finally
    // !    fSharedAutoLocker.Leave;
    // !  end;
    // !end;
    procedure Enter; virtual;
    /// leave the mutex
    // - as expected by IAutoLocker interface
    procedure Leave; virtual;
    /// access to the locking methods of this instance
    // - as expected by IAutoLocker interface
    function Safe: PSynLocker;
    /// direct access to the locking methods of this instance
    // - faster than IAutoLocker.Safe function
    property Locker: TSynLocker
      read fSafe;
  end;



{ ************ TSynPersistent* TSyn*List TSynLocker classes }

type
  /// our own empowered TPersistent-like parent class
  // - TPersistent has an unexpected speed overhead due a giant lock introduced
  // to manage property name fixup resolution (which we won't use outside the UI)
  // - this class has a virtual constructor, so is a preferred alternative
  // to both TPersistent and TPersistentWithCustomCreate classes
  // - features some protected methods to customize its JSON serialization
  // - for best performance, any type inheriting from this class will bypass
  // some regular steps: do not implement interfaces or use TMonitor with them!
  TSynPersistent = class(TObjectWithCustomCreate)
  protected
    // this default implementation will call AssignError()
    procedure AssignTo(Dest: TSynPersistent); virtual;
    procedure AssignError(Source: TSynPersistent);
  public
    /// allows to implement a TPersistent-like assignement mechanism
    // - inherited class should override AssignTo() protected method
    // to implement the proper assignment
    procedure Assign(Source: TSynPersistent); virtual;
  end;

  /// used to determine the exact class type of a TSynPersistent
  TSynPersistentClass = class of TSynPersistent;


  {$ifdef HASITERATORS}
  /// abstract pointer Enumerator
  TPointerEnumerator = record
  private
    Curr, After: PPointer;
    function GetCurrent: pointer; inline;
  public
    procedure Init(Values: PPointerArray; Count: PtrUInt); inline;
    function MoveNext: Boolean; inline;
    function GetEnumerator: TPointerEnumerator; inline;
    /// returns the current pointer value
    property Current: pointer
      read GetCurrent;
  end;
  {$endif HASITERATORS}

  {$M+}
  /// simple and efficient TList, without any notification
  // - regular TList has an internal notification mechanism which slows down
  // basic process, and can't be easily inherited
  // - stateless methods (like Add/Clear/Exists/Remove) are defined as virtual
  // since can be overriden e.g. by TSynObjectListLocked to add a TSynLocker
  TSynList = class(TObject)
  protected
    fCount: integer;
    fList: TPointerDynArray;
    function Get(index: integer): pointer;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// virtual constructor called at instance creation
    constructor Create; virtual;
    /// add one item to the list
    function Add(item: pointer): PtrInt; virtual;
    /// insert one item to the list at a given position
    function Insert(item: pointer; index: PtrInt): PtrInt;
    /// delete all items of the list
    procedure Clear; virtual;
    /// delete one item from the list
    procedure Delete(index: integer; dontfree: boolean = false); virtual;
    /// fast retrieve one item in the list
    function IndexOf(item: pointer): PtrInt; virtual;
    /// fast check if one item exists in the list
    function Exists(item: pointer): boolean; virtual;
    /// fast delete one item in the list
    function Remove(item: pointer): PtrInt; virtual;
    {$ifdef HASITERATORS}
    /// an enumerator able to compile "for .. in list do" statements
    function GetEnumerator: TPointerEnumerator;
    {$endif HASITERATORS}
    /// how many items are stored in this TList instance
    property Count: integer
      read fCount;
    /// low-level access to the items stored in this TList instance
    property List: TPointerDynArray
      read fList;
    /// low-level array-like access to the items stored in this TList instance
    // - warning: if index is out of range, will return nil and won't raise
    // any exception
    property Items[index: integer]: pointer
      read Get; default;
  end;
  {$M-}
  PSynList = ^TSynList;

  /// simple and efficient TObjectList, without any notification
  TSynObjectList = class(TSynList)
  protected
    fOwnObjects: boolean;
    fItemClass: TClass;
  public
    /// initialize the object list
    // - can optionally specify an item class for efficient JSON serialization
    constructor Create(aOwnObjects: boolean = true;
      aItemClass: TClass = nil); reintroduce; virtual;
    /// delete one object from the list
    // - will also Free the item if OwnObjects was set, and dontfree is false
    procedure Delete(index: integer; dontfree: boolean = false); override;
    /// delete all objects of the list
    procedure Clear; override;
    /// delete all objects of the list in reverse order
    // - for some kind of processes, owned objects should be removed from the
    // last added to the first
    // - will use slower but safer FreeAndNilSafe() instead of plain Free
    procedure ClearFromLast; virtual;
    /// finalize the store items
    destructor Destroy; override;
    /// create a new ItemClass instance, Add() it and return it
    function NewItem: pointer;
    /// optional class of the stored items
    // - used e.g. by _JL_TSynObjectList() when unserializing from JSON
    property ItemClass: TClass
      read fItemClass write fItemClass;
    /// flag set if this list will Free its items on Delete/Clear/Destroy
    property OwnObjects: boolean
      read fOwnObjects write fOwnObjects;
  end;
  PSynObjectList = ^TSynObjectList;

  /// meta-class of TSynObjectList type
  TSynObjectListClass = class of TSynObjectList;

  /// adding locking methods to a TSynPersistent with virtual constructor
  // - you may use this class instead of the RTL TCriticalSection, since it
  // would use a TSynLocker which does not suffer from CPU cache line conflit,
  // and is cross-compiler whereas TMonitor is Delphi-specific and buggy (at
  // least before XE5)
  // - if you don't need TSynPersistent overhead, consider plain TSynLocked class
  TSynPersistentLock = class(TSynPersistent)
  protected
    // TSynLocker would increase inherited fields offset -> managed PSynLocker
    fSafe: PSynLocker;
    // will lock/unlock the instance during JSON serialization of its properties
    function RttiBeforeWriteObject(W: TTextWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
    procedure RttiAfterWriteObject(W: TTextWriter;
      Options: TTextWriterWriteObjectOptions); override;
    // set the rcfHookWrite flag to call RttiBeforeWriteObject
    class procedure RttiCustomSetParser(Rtti: TRttiCustom); override;
  public
    /// initialize the instance, and its associated lock
    constructor Create; override;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker
      read fSafe;
    /// could be used as a short-cut to Safe.Lock
    procedure Lock;
      {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Safe.UnLock
    procedure Unlock;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// adding light non-upgradable multiple Read / exclusive Write locking
  // methods to a TSynPersistent with virtual constructor
  TSynPersistentRWLightLock = class(TSynPersistent)
  protected
    fSafe: TRWLightLock;
  public
    /// access to the associated non-upgradable TRWLightLock instance
    // - call Safe methods to protect multi-thread access on this storage
    property Safe: TRWLightLock
      read fSafe;
  end;

  /// adding light upgradable multiple Read / exclusive Write locking methods
  // to a TSynPersistent with virtual constructor
  TSynPersistentRWLock = class(TSynPersistent)
  protected
    fSafe: TRWLock;
  public
    /// access to the associated upgradable TRWLock instance
    // - call Safe methods to protect multi-thread access on this storage
    property Safe: TRWLock
      read fSafe;
  end;

  {$ifndef PUREMORMOT2}

  /// used for backward compatibility only with existing code
  TSynPersistentLocked = class(TSynPersistentLock);

  {$endif PUREMORMOT2}

  /// adding locking methods to a TInterfacedObject with virtual constructor
  TInterfacedObjectLocked = class(TInterfacedObjectWithCustomCreate)
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the object instance, and its associated lock
    constructor Create; override;
    /// release the instance (including the locking resource)
    destructor Destroy; override;
    /// access to the locking methods of this instance
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block
    property Safe: PSynLocker
      read fSafe;
  end;

  /// adding light locking methods to a TInterfacedObject with virtual constructor
  TInterfacedObjectRWLocked = class(TInterfacedObjectWithCustomCreate)
  protected
    fSafe: TRWLock;
  public
    /// access to the multiple Read / exclusive Write locking methods of this instance
    property Safe: TRWLock
      read fSafe;
  end;

  /// add TRWLightLock non-upgradable methods to a TSynObjectList
  // - this class expands the regular TSynObjectList to include a TRWLightLock
  // - you need to call the Safe locking methods by hand to protect the
  // execution of all methods, since even Add/Clear/ClearFromLast/Remove/Exists
  // have not been overriden because TRWLighLock.WriteLock is not reentrant
  TSynObjectListLightLocked = class(TSynObjectList)
  protected
    fSafe: TRWLightLock;
  public
    /// the light single Read / exclusive Write LightLock associated to this list
    // - could be used to protect shared resources within the internal process,
    // for index-oriented methods like Delete/Items/Count...
    // - use Safe LightLock methods with a try ... finally bLightLock
    property Safe: TRWLightLock
      read fSafe;
  end;

  /// add TRWLock upgradable methods to a TSynObjectList
  // - this class expands the regular TSynObjectList to include a TRWLock
  // - you need to call the Safe locking methods by hand to protect the
  // execution of index-oriented methods (like Delete/Items/Count...): the
  // list content may change in the background, so using indexes is thread-safe
  // - on the other hand, Add/Clear/ClearFromLast/Remove stateless methods have
  // been overriden in this class to call Safe lock methods, and therefore are
  // thread-safe and protected to any background change
  TSynObjectListLocked = class(TSynObjectList)
  protected
    fSafe: TRWLock;
  public
    /// add one item to the list using Safe.WriteLock
    function Add(item: pointer): PtrInt; override;
    /// delete all items of the list using Safe.WriteLock
    procedure Clear; override;
    /// delete all items of the list in reverse order, using Safe.WriteLock
    procedure ClearFromLast; override;
    /// fast delete one item in the list, using Safe.WriteLock
    function Remove(item: pointer): PtrInt; override;
    /// check an item using Safe.ReadOnlyLock
    function Exists(item: pointer): boolean; override;
    /// the light single Read / exclusive Write lock associated to this list
    // - could be used to protect shared resources within the internal process,
    // for index-oriented methods like Delete/Items/Count...
    // - use Safe lock methods within a try ... finally block
    property Safe: TRWLock
      read fSafe;
  end;

  /// event used by TSynObjectListSorted to compare its instances
  TOnObjectCompare = function(A, B: TObject): integer;

  /// an ordered thread-safe TSynObjectList
  // - items will be stored in order, for O(log(n)) fast search
  TSynObjectListSorted = class(TSynObjectListLocked)
  protected
    fCompare: TOnObjectCompare;
    // returns TRUE and the index of existing Item, or FALSE and the index
    // where the Item is to be inserted so that the array remains sorted
    function Locate(item: pointer; out index: PtrInt): boolean;
  public
    /// initialize the object list to be sorted with the supplied function
    constructor Create(const aCompare: TOnObjectCompare;
      aOwnsObjects: boolean = true); reintroduce;
    /// add in-order one item to the list using Safe.WriteLock
    // - returns the sorted index when item was inserted
    // - returns < 0 if item was found, as -(existingindex + 1)
    function Add(item: pointer): PtrInt; override;
    /// fast retrieve one item in the list using O(log(n)) binary search
    // - this overriden version won't search for the item pointer itself,
    // but will use the Compare() function until it is 0
    function IndexOf(item: pointer): PtrInt; override;
    /// fast retrieve one item in the list using O(log(n)) binary search
    // - supplied item should have enough information for fCompare to work
    function Find(item: TObject): TObject;
    /// how two stored objects are stored
    property Compare: TOnObjectCompare
      read fCompare write fCompare;
  end;


{ ************ TSynPersistentStore with proper Binary Serialization }

type
  /// abstract high-level handling of (SynLZ-)compressed persisted storage
  // - LoadFromReader/SaveToWriter abstract methods should be overriden
  // with proper binary persistence implementation
  TSynPersistentStore = class(TSynPersistentRWLock)
  protected
    fName: RawUtf8;
    fReader: TFastReader;
    fReaderTemp: PRawByteString;
    fLoadFromLastUncompressed, fSaveToLastUncompressed: integer;
    fLoadFromLastAlgo: TAlgoCompress;
    /// low-level virtual methods implementing the persistence reading
    procedure LoadFromReader; virtual;
    procedure SaveToWriter(aWriter: TBufferWriter); virtual;
  public
    /// initialize a void storage with the supplied name
    constructor Create(const aName: RawUtf8); reintroduce; overload; virtual;
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromBuffer(aBuffer: pointer; aBufferLen: integer;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// fill the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal); overload;
    /// initialize the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(aBuffer: pointer; aBufferLen: integer;
      aLoad: TAlgoCompressLoad = aclNormal); overload; virtual;
    /// initialize the storage from a SaveToFile content
    // - actually call the LoadFromReader() virtual method for persistence
    // - returns false if the file is not found, true if the file was loaded
    // without any problem, or raise a EFastReader exception on decoding error
    function LoadFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal): boolean;
    /// persist the content as a SynLZ-compressed binary blob
    // - to be retrieved later on via LoadFrom method
    // - actually call the SaveToWriter() protected virtual method for persistence
    // - you can specify ForcedAlgo if you want to override the default AlgoSynLZ
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    procedure SaveTo(out aBuffer: RawByteString; nocompression: boolean = false;
      BufLen: integer = 65536; ForcedAlgo: TAlgoCompress = nil;
      BufferOffset: integer = 0); overload; virtual;
    /// persist the content as a SynLZ-compressed binary blob
    // - just an overloaded wrapper
    function SaveTo(nocompression: boolean = false; BufLen: integer = 65536;
      ForcedAlgo: TAlgoCompress = nil; BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// persist the content as a SynLZ-compressed binary file
    // - to be retrieved later on via LoadFromFile method
    // - returns the number of bytes of the resulting file
    // - actually call the SaveTo method for persistence
    function SaveToFile(const aFileName: TFileName; nocompression: boolean = false;
      BufLen: integer = 65536; ForcedAlgo: TAlgoCompress = nil): PtrUInt;
    /// one optional text associated with this storage
    // - you can define this field as published to serialize its value in log/JSON
    property Name: RawUtf8
      read fName;
    /// after a LoadFrom(), contains the uncompressed data size read
    property LoadFromLastUncompressed: integer
      read fLoadFromLastUncompressed;
    /// after a SaveTo(), contains the uncompressed data size written
    property SaveToLastUncompressed: integer
      read fSaveToLastUncompressed;
  end;



{ ********** Efficient RTTI Values Binary Serialization and Comparison }

type
  /// possible options for a TDocVariant JSON/BSON document storage
  // - defined in this unit to avoid circular reference with mormot.core.variants
  // - dvoIsArray and dvoIsObject will store the "Kind: TDocVariantKind" state -
  // you should never have to define these two options directly
  // - dvoNameCaseSensitive will be used for every name lookup - here
  // case-insensitivity is restricted to a-z A-Z 0-9 and _ characters
  // - dvoCheckForDuplicatedNames will be used for method
  // TDocVariantData.AddValue(), but not when setting properties at
  // variant level: for consistency, "aVariant.AB := aValue" will replace
  // any previous value for the name "AB"
  // - dvoReturnNullForUnknownProperty will be used when retrieving any value
  // from its name (for dvObject kind of instance), or index (for dvArray or
  // dvObject kind of instance)
  // - by default, internal values will be copied by-value from one variant
  // instance to another, to ensure proper safety - but it may be too slow:
  // if you set dvoValueCopiedByReference, the internal
  // TDocVariantData.VValue/VName instances will be copied by-reference,
  // to avoid memory allocations, BUT it may break internal process if you change
  // some values in place (since VValue/VName and VCount won't match) - as such,
  // if you set this option, ensure that you use the content as read-only
  // - any registered custom types may have an extended JSON syntax (e.g.
  // TBsonVariant does for MongoDB types), and will be searched during JSON
  // parsing, unless dvoJsonParseDoNotTryCustomVariants is set (slightly faster)
  // - the parser will try to guess the array or object size by pre-fetching
  // some content: you can set dvoJsonParseDoNotGuessCount if your input has
  // a lot of nested documents, and manual resize is preferred - this option
  // will be forced by InitJson if a huge nest of objects is detected
  // - by default, it will only handle direct JSON [array] of {object}: but if
  // you define dvoJsonObjectParseWithinString, it will also try to un-escape
  // a JSON string first, i.e. handle "[array]" or "{object}" content (may be
  // used e.g. when JSON has been retrieved from a database TEXT column) - is
  // used for instance by VariantLoadJson()
  // - JSON serialization will follow the standard layout, unless
  // dvoSerializeAsExtendedJson is set so that the property names would not
  // be escaped with double quotes, writing '{name:"John",age:123}' instead of
  // '{"name":"John","age":123}': this extended json layout is compatible with
  // http://docs.mongodb.org/manual/reference/mongodb-extended-json and with
  // TDocVariant JSON unserialization, also our SynCrossPlatformJSON unit, but
  // NOT recognized by most JSON clients, like AJAX/JavaScript or C#/Java
  // - by default, only integer/Int64/currency number values are allowed, unless
  // dvoAllowDoubleValue is set and 32-bit floating-point conversion is tried,
  // with potential loss of precision during the conversion
  // - dvoInternNames and dvoInternValues will use shared TRawUtf8Interning
  // instances to maintain a list of RawUtf8 names/values for all TDocVariant,
  // so that redundant text content will be allocated only once on heap
  // - see JSON_[TDocVariantModel] and all JSON_* constants as useful sets
  TDocVariantOption = (
    dvoIsArray,
    dvoIsObject,
    dvoNameCaseSensitive,
    dvoCheckForDuplicatedNames,
    dvoReturnNullForUnknownProperty,
    dvoValueCopiedByReference,
    dvoJsonParseDoNotTryCustomVariants,
    dvoJsonParseDoNotGuessCount,
    dvoJsonObjectParseWithinString,
    dvoSerializeAsExtendedJson,
    dvoAllowDoubleValue,
    dvoInternNames,
    dvoInternValues);

  /// set of options for a TDocVariant storage
  // - defined in this unit to avoid circular reference with mormot.core.variants
  // - see JSON_[TDocVariantModel] and all JSON_* constants (e.g. JSON_FAST or
  // JSON_FAST_FLOAT) as potential values
  // - when specifying the options, you should not include dvoIsArray nor
  // dvoIsObject directly in the set, but explicitly define TDocVariantDataKind
  TDocVariantOptions = set of TDocVariantOption;

  /// pointer to a set of options for a TDocVariant storage
  // - defined in this unit to avoid circular reference with mormot.core.variants
  // - use e.g. @JSON_[mFast], @JSON_[mDefault], or any other TDocVariantModel
  PDocVariantOptions = ^TDocVariantOptions;

  /// a boolean array of TDocVariant storage options
  TDocVariantOptionsBool = array[boolean] of TDocVariantOptions;
  PDocVariantOptionsBool = ^TDocVariantOptionsBool;


type
  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via RTTI_BINARYSAVE[] lookup table
  // - work with managed and unmanaged types
  // - persist Data^ into Dest, returning the size in Data^ as bytes
  TRttiBinarySave = function(Data: pointer; Dest: TBufferWriter;
    Info: PRttiInfo): PtrInt;

  /// the type of RTTI_BINARYSAVE[] efficient lookup table
  TRttiBinarySaves = array[TRttiKind] of TRttiBinarySave;
  PRttiBinarySaves = ^TRttiBinarySaves;

  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via RTTI_BINARYLOAD[] lookup table
  // - work with managed and unmanaged types
  // - fill Data^ from Source, returning the size in Data^ as bytes
  TRttiBinaryLoad = function(Data: pointer; var Source: TFastReader;
    Info: PRttiInfo): PtrInt;

  /// the type of RTTI_BINARYLOAD[] efficient lookup table
  TRttiBinaryLoads = array[TRttiKind] of TRttiBinaryLoad;
  PRttiBinaryLoads = ^TRttiBinaryLoads;

  /// internal function handler for fast comparison of any RTTI type value
  // - i.e. the kind of functions called via RTTI_COMPARE[] lookup table
  // - work with managed and unmanaged types
  // - returns the size in Data1/Data2^ as bytes, and the result in Compared
  TRttiCompare = function(Data1, Data2: pointer; Info: PRttiInfo;
    out Compared: integer): PtrInt;

  /// the type of RTTI_COMPARE[] efficient lookup table
  TRttiCompares = array[TRttiKind] of TRttiCompare;
  PRttiCompares = ^TRttiCompares;

  TRttiComparers = array[{CaseInSensitive=}boolean] of TRttiCompares;

var
  /// lookup table for binary persistence of any RTTI type value
  // - for efficient persistence into binary of managed and unmanaged types
  RTTI_BINARYSAVE: TRttiBinarySaves;

  /// lookup table for binary persistence of any RTTI type value
  // - for efficient retrieval from binary of managed and unmanaged types
  RTTI_BINARYLOAD: TRttiBinaryLoads;

  /// lookup table for comparison of any RTTI type value
  // - for efficient search or sorting of managed and unmanaged types
  // - RTTI_COMPARE[false] for case-sensitive comparison
  // - RTTI_COMPARE[true] for case-insensitive comparison
  RTTI_COMPARE: TRttiComparers;

  /// lookup table for comparison of ordinal RTTI type values
  // - slightly faster alternative to RTTI_COMPARE[rkOrdinalTypes]
  RTTI_ORD_COMPARE: array[TRttiOrd] of TRttiCompare;

  /// lookup table for comparison of floating-point RTTI type values
  // - slightly faster alternative to RTTI_COMPARE[rkFloat]
  RTTI_FLOAT_COMPARE: array[TRttiFloat] of TRttiCompare;

/// raw binary serialization of a dynamic array
// - as called e.g. by TDynArray.SaveTo, using ExternalCount optional parameter
// - RTTI_BINARYSAVE[rkDynArray] is a wrapper to this function, with ExternalCount=nil
procedure DynArraySave(Data: PAnsiChar; ExternalCount: PInteger;
  Dest: TBufferWriter; Info: PRttiInfo); overload;

/// serialize a dynamic array content as binary, ready to be loaded by
// DynArrayLoad() / TDynArray.Load()
// - Value shall be set to the source dynamic arry field
// - is a wrapper around BinarySave(rkDynArray)
function DynArraySave(var Value; TypeInfo: PRttiInfo): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a dynamic array content from a binary serialization as saved by
// DynArraySave() / TDynArray.Save()
// - Value shall be set to the target dynamic array field
// - is a wrapper around BinaryLoad(rkDynArray)
function DynArrayLoad(var Value; Source: PAnsiChar; TypeInfo: PRttiInfo;
  {$ifdef PUREMORMOT2} // SourceMax is manadatory for safety
  TryCustomVariants: PDocVariantOptions; SourceMax: PAnsiChar): PAnsiChar;
  {$else}
  TryCustomVariants: PDocVariantOptions = nil; SourceMax: PAnsiChar = nil): PAnsiChar;
  {$endif PUREMORMOT2}
  {$ifdef HASINLINE}inline;{$endif}

/// low-level binary unserialization as saved by DynArraySave/TDynArray.Save
// - as used by DynArrayLoad() and TDynArrayLoadFrom
// - returns the stored length() of the dynamic array, and Source points to
// the stored binary data itself
function DynArrayLoadHeader(var Source: TFastReader;
  ArrayInfo, ItemInfo: PRttiInfo): integer;

/// raw comparison of two dynamic arrays
// - as called e.g. by TDynArray.Equals, using ExternalCountA/B optional parameter
// - RTTI_COMPARE[true/false,rkDynArray] are wrappers to this, with ExternalCount=nil
// - if Info=TypeInfo(TObjectDynArray) then will compare any T*ObjArray
function DynArrayCompare(A, B: PAnsiChar; ExternalCountA, ExternalCountB: PInteger;
  Info: PRttiInfo; CaseInSensitive: boolean): integer; overload;

/// wrapper around TDynArray.Add
// - warning: the Item type is not checked at runtime, so should be as expected
// - not very fast, but could be useful for simple code
function DynArrayAdd(TypeInfo: PRttiInfo; var DynArray; const Item): integer; overload;

/// wrapper around TDynArray.Delete
// - not very fast, but could be useful for simple code
function DynArrayDelete(TypeInfo: PRttiInfo; var DynArray; Index: PtrInt): boolean; overload;

/// compare two dynamic arrays by calling TDynArray.Equals
// - if Info=TypeInfo(TObjectDynArray) then will compare any T*ObjArray
function DynArrayEquals(TypeInfo: PRttiInfo; var Array1, Array2;
  Array1Count: PInteger = nil; Array2Count: PInteger = nil;
  CaseInsensitive: boolean = false): boolean;
  {$ifdef HASINLINE}inline;{$endif}

{$ifdef FPCGENERICS}
/// wrapper around TDynArray.Add
// - warning: the Item type is not checked at runtime, so should be as expected
// - not very fast, but could be useful for simple code
function DynArrayAdd<TArray>(var DynArray: TArray; const Item): integer; overload;

/// wrapper around TDynArray.Delete
// - not very fast, but could be useful for simple code
function DynArrayDelete<TArray>(var DynArray: TArray; Index: PtrInt): boolean; overload;

/// compare two dynamic arrays values
function DynArrayCompare<TArray>(var Array1, Array2: TArray;
  CaseInSensitive: boolean = false): integer; overload;
{$endif FPCGENERICS}

// some low-level comparison methods used by mormot.core.json
function _BC_SQWord(A, B: PInt64; Info: PRttiInfo; out Compared: integer): PtrInt;
function _BC_UQWord(A, B: PQWord; Info: PRttiInfo; out Compared: integer): PtrInt;
function _BC_ObjArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
function _BCI_ObjArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;

/// check equality of two values by content, using RTTI
// - optionally returns the known in-memory PSize of the value
function BinaryEquals(A, B: pointer; Info: PRttiInfo; PSize: PInteger;
  Kinds: TRttiKinds; CaseInSensitive: boolean): boolean;

/// comparison of two values by content, using RTTI
function BinaryCompare(A, B: pointer; Info: PRttiInfo;
  CaseInSensitive: boolean): integer; overload;

/// comparison of two arrays of values by content, using RTTI
function BinaryCompare(A, B: pointer; Info: PRttiInfo; Count: PtrInt;
  CaseInSensitive: boolean): integer; overload;

/// comparison of two TObject published properties, using RTTI
function ObjectCompare(A, B: TObject; CaseInSensitive: boolean): integer; overload;

/// comparison of published properties of several TObject instances, using RTTI
function ObjectCompare(A, B: PObject; Count: PtrInt;
  CaseInsensitive: boolean = false): integer; overload;

/// case-sensitive comparison of two TObject published properties, using RTTI
function ObjectEquals(A, B: TObject): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// case-insensitive comparison of two TObject published properties, using RTTI
function ObjectEqualsI(A, B: TObject): boolean;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef PUREMORMOT2}

/// how many bytes a BinarySave() may return
// - deprecated function - use overloaded BinarySave() functions instead
function BinarySaveLength(Data: pointer; Info: PRttiInfo; Len: PInteger;
  Kinds: TRttiKinds): integer; deprecated;

/// binary persistence of any value using RTTI, into a memory buffer
// - deprecated function - use overloaded BinarySave() functions instead
function BinarySave(Data: pointer; Dest: PAnsiChar; Info: PRttiInfo;
  out Len: integer; Kinds: TRttiKinds): PAnsiChar; overload; deprecated;

{$endif PUREMORMOT2}

/// binary persistence of any value using RTTI, into a RawByteString buffer
function BinarySave(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds;
  WithCrc: boolean = false): RawByteString; overload;

/// binary persistence of any value using RTTI, into a TBytes buffer
function BinarySaveBytes(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds): TBytes;

/// binary persistence of any value using RTTI, into a TBufferWriter stream
procedure BinarySave(Data: pointer; Info: PRttiInfo; Dest: TBufferWriter); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// binary persistence of any value using RTTI, into a TSynTempBuffer buffer
procedure BinarySave(Data: pointer; var Dest: TSynTempBuffer;
  Info: PRttiInfo; Kinds: TRttiKinds; WithCrc: boolean = false); overload;

/// binary persistence of any value using RTTI, into a Base64-encoded text
// - contains a trailing crc32c hash before the actual data
function BinarySaveBase64(Data: pointer; Info: PRttiInfo; UriCompatible: boolean;
  Kinds: TRttiKinds; WithCrc: boolean = true): RawUtf8;

/// unserialize any value from BinarySave() memory buffer, using RTTI
function BinaryLoad(Data: pointer; Source: PAnsiChar; Info: PRttiInfo;
  Len: PInteger; SourceMax: PAnsiChar; Kinds: TRttiKinds;
  TryCustomVariants: PDocVariantOptions = nil): PAnsiChar; overload;

/// unserialize any value from BinarySave() RawByteString, using RTTI
function BinaryLoad(Data: pointer; const Source: RawByteString; Info: PRttiInfo;
  Kinds: TRttiKinds; TryCustomVariants: PDocVariantOptions = nil): boolean; overload;

/// unserialize any value from BinarySaveBase64() encoding, using RTTI
// - optionally contains a trailing crc32c hash before the actual data
function BinaryLoadBase64(Source: PAnsiChar; Len: PtrInt; Data: pointer;
  Info: PRttiInfo; UriCompatible: boolean; Kinds: TRttiKinds;
  WithCrc: boolean = true; TryCustomVariants: PDocVariantOptions = nil): boolean;


/// check equality of two records by content
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties
// - will use binary-level comparison: it could fail to match two floating-point
// values because of rounding issues (Currency won't have this problem)
// - is a wrapper around BinaryEquals(rkRecordTypes)
function RecordEquals(const RecA, RecB; TypeInfo: PRttiInfo;
  PRecSize: PInteger = nil; CaseInSensitive: boolean = false): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a RawByteString
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties (but not with internal raw pointers, of course)
// - will use a proprietary binary format, with some variable-length encoding
// of the string length - note that if you change the type definition, any
// previously-serialized content will fail, maybe triggering unexpected GPF: you
// may use TypeInfoToHash() if you share this binary data accross executables
// - warning: will encode RTL string fields as AnsiString (one byte per char)
// prior to Delphi 2009, and as UnicodeString (two bytes per char) since Delphi
// 2009: if you want to use this function between UNICODE and NOT UNICODE
// versions of Delphi, you should use some explicit types like RawUtf8,
// WinAnsiString, SynUnicode or even RawUnicode/WideString
// - is a wrapper around BinarySave(rkRecordTypes)
function RecordSave(const Rec; TypeInfo: PRttiInfo): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a TBytes dynamic array
// - could be used as an alternative to RawByteString's RecordSave()
// - is a wrapper around BinarySaveBytes(rkRecordTypes)
function RecordSaveBytes(const Rec; TypeInfo: PRttiInfo): TBytes;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef PUREMORMOT2}

/// compute the number of bytes needed to save a record content
// using the RecordSave() function
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSaveLength(const Rec; TypeInfo: PRttiInfo;
  Len: PInteger = nil): integer; deprecated;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a destination memory buffer
// - Dest must be at least RecordSaveLength() bytes long
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: PRttiInfo;
  out Len: integer): PAnsiChar; overload; deprecated;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a destination memory buffer
// - Dest must be at least RecordSaveLength() bytes long
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: PRttiInfo): PAnsiChar;
  overload; deprecated; {$ifdef HASINLINE}inline;{$endif}

{$endif PUREMORMOT2}

/// save a record content into a destination memory buffer
// - caller should make Dest.Done once finished with Dest.buf/Dest.len buffer
// - is a wrapper around BinarySave(rkRecordTypes)
procedure RecordSave(const Rec; var Dest: TSynTempBuffer; TypeInfo: PRttiInfo); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a Base64 encoded UTF-8 text content
// - will use RecordSave() format, with a left-sided binary CRC
// - is a wrapper around BinarySaveBase64(rkRecordTypes)
function RecordSaveBase64(const Rec; TypeInfo: PRttiInfo;
  UriCompatible: boolean = false): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a record content from a memory buffer as saved by RecordSave()
// - return nil if the Source buffer is incorrect
// - in case of success, return the memory buffer pointer just after the
// read content, and set the Rec size, in bytes, into Len reference variable
// - will use a proprietary binary format, with some variable-length encoding
// of the string length - note that if you change the type definition, any
// previously-serialized content will fail, maybe triggering unexpected GPF: you
// may use TypeInfoToHash() if you share this binary data accross executables
// - you should provide in SourceMax the first byte after the Source memory
// buffer, which will be used to avoid any unexpected buffer overflow - clearly
// mandatory when decoding the content from any external process (e.g. a
// maybe-forged client) - with no performance penalty
// - is a wrapper around BinaryLoad(rkRecordTypes)
function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: PRttiInfo;
  {$ifdef PUREMORMOT2} // SourceMax is manadatory for safety
  Len: PInteger; SourceMax: PAnsiChar;
  {$else}              // mORMot 1 compatibility mode
  Len: PInteger = nil; SourceMax: PAnsiChar = nil;
  {$endif PUREMORMOT2}
  TryCustomVariants: PDocVariantOptions = nil): PAnsiChar; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a record content from a memory buffer as saved by RecordSave()
// - will use the Source length to detect and avoid any buffer overlow
// - returns false if the Source buffer was incorrect, true on success
// - is a wrapper around BinaryLoad(rkRecordTypes)
function RecordLoad(var Rec; const Source: RawByteString;
  TypeInfo: PRttiInfo; TryCustomVariants: PDocVariantOptions = nil): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// read a record content from a Base64 encoded content
// - expects RecordSaveBase64() format, with a left-sided binary CRC32C
// - is a wrapper around BinaryLoadBase64(rkRecordTypes)
function RecordLoadBase64(Source: PAnsiChar; Len: PtrInt; var Rec; TypeInfo: PRttiInfo;
  UriCompatible: boolean = false; TryCustomVariants: PDocVariantOptions = nil): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// crc32c-based hash of a variant value
// - complex string types will make up to 255 uppercase characters conversion
// if CaseInsensitive is true
// - you can specify your own hashing function if crc32c is not what you expect
function VariantHash(const value: variant; CaseInsensitive: boolean;
  Hasher: THasher = nil): cardinal;


{ ************ TDynArray and TDynArrayHashed Wrappers }

type
  /// function prototype to be used for TDynArray Sort and Find method
  // - common functions exist for base types: see e.g. SortDynArrayBoolean,
  // SortDynArrayByte, SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal,
  // SortDynArrayInt64, SortDynArrayQWord, SordDynArraySingle, SortDynArrayDouble,
  // SortDynArrayAnsiString, SortDynArrayAnsiStringI, SortDynArrayUnicodeString,
  // SortDynArrayUnicodeStringI, SortDynArrayString, SortDynArrayStringI
  // - any custom type (even records) can be compared then sort by defining
  // such a custom function
  // - must return 0 if A=B, -1 if A<B, 1 if A>B
  TDynArraySortCompare = function(const A, B): integer;

  /// event oriented version of TDynArraySortCompare
  TOnDynArraySortCompare = function(const A, B): integer of object;

  /// defined here as forward definition of the TRawUtf8Interning final class
  TRawUtf8InterningAbstract = class(TSynPersistent);

const
  /// redirect to the proper SortDynArrayAnsiString/SortDynArrayAnsiStringI
  SORT_LSTRING: array[{caseins=}boolean] of TDynArraySortCompare = (
    {$ifdef CPUINTEL}
    SortDynArrayAnsiString,
    {$else}
    SortDynArrayRawByteString,
    {$endif CPUINTEL}
    SortDynArrayAnsiStringI);

{$ifndef PUREMORMOT2}

type
  /// internal enumeration used to specify some standard arrays
  // - mORMot 1.18 did have two serialization engines - we unified it
  // - defined only for backward compatible code; use TRttiParserType instead
  TDynArrayKind = TRttiParserType;
  TDynArrayKinds = TRttiParserTypes;

const
  /// deprecated TDynArrayKind enumerate mapping
  // - defined only for backward compatible code; use TRttiParserType instead
  djNone = ptNone;
  djboolean = ptboolean;
  djByte = ptByte;
  djWord = ptWord;
  djInteger = ptInteger;
  djCardinal = ptCardinal;
  djSingle = ptSingle;
  djInt64 = ptInt64;
  djQWord = ptQWord;
  djDouble = ptDouble;
  djCurrency = ptCurrency;
  djTimeLog = ptTimeLog;
  djDateTime = ptDateTime;
  djDateTimeMS = ptDateTimeMS;
  djRawUtf8 = ptRawUtf8;
  djRawJson = ptRawJson;
  djWinAnsi = ptWinAnsi;
  djString = ptString;
  djRawByteString = ptRawByteString;
  djWideString = ptWideString;
  djSynUnicode = ptSynUnicode;
  djHash128 = ptHash128;
  djHash256 = ptHash256;
  djHash512 = ptHash512;
  djVariant = ptVariant;
  djCustom = ptCustom;
  djPointer = ptPtrInt;
  djObject = ptPtrInt;
  djUnmanagedTypes = ptUnmanagedTypes;
  djStringTypes = ptStringTypes;

{$endif PUREMORMOT2}

type
  /// the kind of exceptions raised during TDynArray/TDynArrayHashed process
  EDynArray = class(ESynException);

  /// a pointer to a TDynArray Wrapper instance
  PDynArray = ^TDynArray;

  /// a wrapper around a dynamic array with one dimension
  // - provide TList-like methods using fast RTTI information
  // - can be used to fast save/retrieve all memory content to a TStream
  // - note that the "const Item" is not checked at compile time nor runtime:
  // you must ensure that Item matchs the element type of the dynamic array;
  // all Item*() methods will use pointers for safety
  // - can use external Count storage to make Add() and Delete() much faster
  // (avoid most reallocation of the memory buffer)
  // - Note that TDynArray is just a wrapper around an existing dynamic array:
  // methods can modify the content of the associated variable but the TDynArray
  // doesn't contain any data by itself. It is therefore aimed to initialize
  // a TDynArray wrapper on need, to access any existing dynamic array.
  // - is defined as an object or as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNDIRECTDYNARRAY}
  TDynArray = record
  {$else}
  TDynArray = object
  {$endif UNDIRECTDYNARRAY}
  private
    fValue: PPointer;
    fInfo: TRttiCustom;
    fCountP: PInteger;
    fCompare: TDynArraySortCompare;
    fSorted, fNoFinalize: boolean;
    function GetCount: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetCount(aCount: PtrInt);
    function GetCapacity: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetCapacity(aCapacity: PtrInt);
    procedure SetCompare(const aCompare: TDynArraySortCompare);
      {$ifdef HASINLINE}inline;{$endif}
    function FindIndex(const Item; aIndex: PIntegerDynArray;
      aCompare: TDynArraySortCompare): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// faster than RTL + handle T*ObjArray + ensure unique
    procedure InternalSetLength(OldLength, NewLength: PtrUInt);
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - the dynamic array must have been defined with its own type
    // (e.g. TIntegerDynArray = array of integer)
    // - if aCountPointer is set, it will be used instead of length() to store
    // the dynamic array items count - it will be much faster when adding
    // items to the array, because the dynamic array won't need to be
    // resized each time - but in this case, you should use the Count property
    // instead of length(array) or high(array) when accessing the data: in fact
    // length(array) will store the memory size reserved, not the items count
    // - if aCountPointer is set, its content will be set to 0, whatever the
    // array length is, or the current aCountPointer^ value is - to bypass this
    // behavior and keep an existing Count, call UseExternalCount() after Init()
    // - a sample usage may be:
    // !var
    // !  DA: TDynArray;
    // !  A: TIntegerDynArray;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray), A);
    // ! (...)
    // - a sample usage may be (using a count variable):
    // !var
    // !  DA: TDynArray;
    // !  A: TIntegerDynArray;
    // !  ACount: integer;
    // !  i: integer;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray), A, @ACount);
    // !  for i := 1 to 100000 do
    // !    DA.Add(i); // MUCH faster using the ACount variable
    // ! (...)   // now you should use DA.Count or Count instead of length(A)
    procedure Init(aTypeInfo: PRttiInfo; var aValue; aCountPointer: PInteger = nil);
    /// initialize the wrapper with a one-dimension dynamic array
    // - also set the Compare() function from a supplied TRttiParserType
    // - aKind=ptNone will guess the type from Info.ArrayRtti/ArrayFirstField
    // - will raise an exception if there is not enough RTTI available
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that the aKind parameter matches at least the first field of
    // the dynamic array item definition
    // - aCaseInsensitive will be used for ptStringTypes
    function InitSpecific(aTypeInfo: PRttiInfo; var aValue; aKind: TRttiParserType;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false): TRttiParserType;
    /// set a specific TRttiParserType for this dynamic array
    // - could be called after InitRtti() to set the Compare() function
    // - as used by InitSpecific() after InitRtti(Rtti.RegisterType(aTypeInfo))
    function SetParserType(aKind: TRttiParserType; aCaseInsensitive: boolean): TRttiParserType;
    /// initialize the wrapper with a one-dimension dynamic array
    // - low-level method, as called by Init() and InitSpecific()
    // - can be called directly for a very fast TDynArray initialization
    // - warning: caller should check that aInfo.Kind=rkDynArray
    procedure InitRtti(aInfo: TRttiCustom; var aValue; aCountPointer: PInteger); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the wrapper with a one-dimension dynamic array
    // - low-level method, as called by Init() and InitSpecific()
    // - can be called directly for a very fast TDynArray initialization
    // - warning: caller should check that aInfo.Kind=rkDynArray
    procedure InitRtti(aInfo: TRttiCustom; var aValue); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast initialize a wrapper for an existing dynamic array of the same type
    // - is slightly faster than
    // ! InitRtti(aAnother.Info, aValue, nil);
    procedure InitFrom(aAnother: PDynArray; var aValue);
      {$ifdef HASINLINE}inline;{$endif}
    /// define the reference to an external count integer variable
    // - Init and InitSpecific methods will reset the aCountPointer to 0: you
    // can use this method to set the external count variable without overriding
    // the current value
    procedure UseExternalCount(aCountPointer: PInteger);
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the wrapper to point to no dynamic array
    // - it won't clear the wrapped array, just reset the fValue internal pointer
    // - in practice, will disable the other methods
    procedure Void;
    /// check if the wrapper points to a dynamic array
    // - i.e. if Void has been called before
    function IsVoid: boolean;
    /// add an element to the dynamic array
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Add(i+10) e.g.)
    // - returns the index of the added element in the dynamic array
    // - note that because of dynamic array internal memory managment, adding
    // may reallocate the list every time a record is added, unless an external
    // count variable has been specified in Init(...,@Count) method
    function Add(const Item): PtrInt;
    /// add an element to the dynamic array, returning its index
    // - note: if you use this method to add a new item with a reference to the
    // dynamic array, be aware that the following trigger a GPF on FPC:
    // !    with Values[DynArray.New] do // otherwise Values is nil -> GPF
    // !    begin
    // !      Field1 := 1;
    // !      ...
    // - so you should either use a local variable:
    // !    i := DynArray.New;
    // !    with Values[i] do // otherwise Values is nil -> GPF
    // !    begin
    // - or even better, don't use the dubious "with Values[...] do" but NewPtr
    function New: PtrInt;
    /// add an element to the dynamic array, returning its pointer
    // - a slightly faster alternative to ItemPtr(New)
    function NewPtr: pointer;
    /// add an element to the dynamic array at the position specified by Index
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Insert(10,i+10) e.g.)
    procedure Insert(Index: PtrInt; const Item);
    /// get and remove the last element stored in the dynamic array
    // - Add + Pop/Peek will implement a LIFO (Last-In-First-Out) stack
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied and removed
    // - use Peek() if you don't want to remove the item, but just get its value
    function Pop(var Dest): boolean;
    /// get the last element stored in the dynamic array
    // - Add + Pop/Peek will implement a LIFO (Last-In-First-Out) stack
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied into Dest
    // - use Pop() if you also want to remove the item
    function Peek(var Dest): boolean;
    /// get and remove the first element stored in the dynamic array
    // - Add + PopHead/PeekHead will implement a FIFO (First-In-First-Out) stack
    // - removing from head will move all items so TSynQueue is faster
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied and removed
    // - use PeekHead() if you don't want to remove the item, but get its value
    // - first slot will be deleted and all content moved, so may take some time
    function PopHead(var Dest): boolean;
    /// get the first element stored in the dynamic array
    // - Add + PopHead/PeekHead will implement a FIFO (First-In-First-Out) stack
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied and removed
    // - use PopHead() if you also want to remove the item
    function PeekHead(var Dest): boolean;
    /// delete the whole dynamic array content
    // - this method will recognize T*ObjArray types and free all instances
    procedure Clear;
      {$ifdef HASINLINE}inline;{$endif}
    /// delete the whole dynamic array content, ignoring exceptions
    // - returns true if no exception occurred when calling Clear, false otherwise
    // - you should better not call this method, which will catch and ignore
    // all exceptions - but it may somewhat make sense in a destructor
    // - this method will recognize T*ObjArray types and free all instances
    function ClearSafe: boolean;
    /// delete one item inside the dynamic array
    // - the deleted element is finalized if necessary
    // - this method will recognize T*ObjArray types and free all instances
    function Delete(aIndex: PtrInt): boolean;
    /// search for an element inside the dynamic array using RTTI
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - will search for all properties content of Item: TList.IndexOf()
    // searches by address, this method searches by content using the RTTI
    // element description (and not the Compare property function)
    // - use the Find() method if you want the search via the Compare property
    // function, or e.g. to search only with some part of the element content
    // - will work with simple types: binaries (byte, word, integer, Int64,
    // Currency, array[0..255] of byte, packed records with no reference-counted
    // type within...), string types (e.g. array of string), and packed records
    // with binary and string types within (like TFileVersion)
    // - won't work with not packed types (like a shorstring, or a record
    // with byte or word fields with {$A+}): in this case, the padding data
    // (i.e. the bytes between the aligned fields) can be filled as random, and
    // there is no way with standard RTTI to identify randomness from values
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write IndexOf(i+10) e.g.)
    function IndexOf(const Item; CaseInSensitive: boolean = true): PtrInt;
    /// search for an element inside the dynamic array using the Compare function
    // - this method will use the Compare property function, or the supplied
    // aCompare for the search; if none of them are set, it will fallback to
    // IndexOf() to perform a default case-sensitive RTTI search
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function Find(const Item; aCompare: TDynArraySortCompare = nil): PtrInt; overload;
    /// search for an element value inside the dynamic array, from an external
    // aIndex[] lookup table - e.g. created by CreateOrderedIndex()
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if an indexed lookup is supplied, it must already be sorted:
    // this function will then use fast O(log(n)) binary search over aCompare
    // - if the indexed lookup is not correct (e.g. aIndex=nil), iterate O(n)
    // using aCompare - it won't fallback to IndexOf() RTTI search
    // - warning: the lookup aIndex[] should be synchronized if array content
    // is modified (in case of addition or deletion)
    function Find(const Item; const aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare): PtrInt; overload;
    /// search for an element value, then fill all properties if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function,
    // and fallback to case-sensitive RTTI search if none is defined
    // - if Item content matches, all Item fields will be filled with the record
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndFill(var Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then delete it if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function,
    // and fallback to case-sensitive RTTI search if none is defined
    // - if Item content matches, this item will be deleted from the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index deleted (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndDelete(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then update the item if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function,
    // and fallback to case-sensitive RTTI search if none is defined
    // - if Item content matches, this item will be updated with the supplied value
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndUpdate(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then add it if none matched
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function,
    // and fallback to case-sensitive RTTI search if none is defined
    // - if no Item content matches, the item will added to the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found and
    // the supplied element has been successfully added
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndAddIfNotExisting(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// sort the dynamic array items, using the Compare property function
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function
    procedure Sort(aCompare: TDynArraySortCompare = nil); overload;
    /// sort some dynamic array items, using the Compare property function
    // - this method allows to sort only some part of the items
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function
    procedure SortRange(aStart, aStop: integer;
      aCompare: TDynArraySortCompare = nil);
    /// will check all items against aCompare
    function IsSorted(aCompare: TDynArraySortCompare = nil): boolean;
    /// will check all items against aCompare, calling Sort() if needed
    // - faster than plain Sort() if the array is likely to be already sorted
    procedure EnsureSorted(aCompare: TDynArraySortCompare = nil);
    /// sort the dynamic array items, using a Compare method (not function)
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function,
    // unless aReverse is true
    // - it won't mark the array as Sorted, since the comparer is local
    procedure Sort(const aCompare: TOnDynArraySortCompare;
      aReverse: boolean = false); overload;
    /// search the items range which match a given value in a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - returns TRUE and the matching indexes, or FALSE if none found
    // - if the array is not sorted, returns FALSE
    // - warning: FirstIndex/LastIndex parameters should be integer, not PtrInt
    function FindAllSorted(const Item;
      out FirstIndex, LastIndex: integer): boolean; overload;
    /// search the item pointers which match a given value in a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - return nil and FindCount = 0 if no matching item was found
    // - return the a pointer to the first matching item, and FindCount >=1
    // - warning: FindCount out parameter should be integer, not PtrInt
    function FindAllSorted(const Item; out FindCount: integer): pointer; overload;
    /// search for an element value inside a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - will be faster than a manual FindAndAddIfNotExisting+Sort process
    // - returns TRUE and the index of existing Item, or FALSE and the index
    // where the Item is to be inserted so that the array remains sorted
    // - you should then call FastAddSorted() later with the returned Index
    // - if the array is not sorted, returns FALSE and Index=-1
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (no FastLocateSorted(i+10) e.g.)
    // - warning: Index out parameter should be integer, not PtrInt
    function FastLocateSorted(const Item; out Index: integer): boolean;
    /// insert a sorted element value at the proper place
    // - the index should have been computed by FastLocateSorted(): false
    // - you may consider using FastLocateOrAddSorted() instead
    procedure FastAddSorted(Index: PtrInt; const Item);
    /// search and add an element value inside a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - will be faster than a manual FindAndAddIfNotExisting+Sort process
    // - returns the index of the existing Item and wasAdded^=false
    // - returns the sorted index of the inserted Item and wasAdded^=true
    // - if the array is not sorted, returns -1 and wasAdded^=false
    // - is just a wrapper around FastLocateSorted+FastAddSorted
    function FastLocateOrAddSorted(const Item; wasAdded: PBoolean = nil): integer;
    /// delete a sorted element value at the proper place
    // - plain Delete(Index) would reset the fSorted flag to FALSE, so use
    // this method with a FastLocateSorted/FastAddSorted array
    procedure FastDeleteSorted(Index: PtrInt);
    /// will reverse all array items, in place
    procedure Reverse;
    /// will call FillZero() on all items, mainly binaries and strings
    // - could be used on a dynamic array to avoid memory forensic after release
    procedure FillZero;
    /// sort the dynamic array items using a lookup array of indexes
    // - in comparison to the Sort method, this CreateOrderedIndex won't change
    // the dynamic array content, but only create (or update) the supplied
    // integer lookup array, using the specified comparison function
    // - if aCompare is not supplied, the method will use fCompare (if defined)
    // - you should provide either a void either a valid lookup table, that is
    // a table with one to one lookup (e.g. created with FillIncreasing)
    // - if the lookup table has less items than the main dynamic array,
    // its content will be recreated
    procedure CreateOrderedIndex(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare); overload;
    /// sort the dynamic array items using a lookup array of indexes
    // - this overloaded method will use the supplied TSynTempBuffer for
    // index storage, so use PIntegerArray(aIndex.buf) to access the values
    // - caller should always make aIndex.Done once done
    procedure CreateOrderedIndex(out aIndex: TSynTempBuffer;
      aCompare: TDynArraySortCompare); overload;
    /// sort using a lookup array of indexes, after a Add()
    // - will resize aIndex if necessary, and set aIndex[Count-1] := Count-1
    procedure CreateOrderedIndexAfterAdd(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare);
    /// save the dynamic array content into a (memory) stream
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    // - Stream position will be set just after the added data
    // - is optimized for memory streams, but will work with any kind of TStream
    procedure SaveToStream(Stream: TStream);
    /// load the dynamic array content from a (memory) stream
    // - stream content must have been created using SaveToStream method
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    procedure LoadFromStream(Stream: TCustomMemoryStream);
    /// save the dynamic array content using our binary serialization
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF
    // - this method will raise an ESynException for T*ObjArray types
    // - use TDynArray.LoadFrom to decode the saved buffer
    // - warning: legacy Hash32 checksum will be stored as 0, so may be refused
    // by mORMot TDynArray.LoadFrom before 1.18.5966
    procedure SaveTo(W: TBufferWriter); overload;
    /// save the dynamic array content into a RawByteString
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    // - this method will raise an ESynException for T*ObjArray types
    // - use TDynArray.LoadFrom to decode the saved buffer
    // - warning: legacy Hash32 checksum will be stored as 0, so may be refused
    // by mORMot TDynArray.LoadFrom before 1.18.5966
    function SaveTo: RawByteString; overload;
    /// unserialize dynamic array content from binary written by TDynArray.SaveTo
    // - return nil if the Source buffer is incorrect: invalid type, wrong
    // checksum, or SourceMax overflow
    // - return a non nil pointer just after the Source content on success
    // - this method will raise an ESynException for T*ObjArray types
    function LoadFrom(Source: PAnsiChar;
      {$ifdef PUREMORMOT2} // SourceMax is manadatory for safety
      SourceMax: PAnsiChar): PAnsiChar;
      {$else}             // mORMot 1 compatibility mode
      SourceMax: PAnsiChar = nil): PAnsiChar;
      {$endif PUREMORMOT2}
    /// unserialize dynamic array content from binary written by TDynArray.SaveTo
    procedure LoadFromReader(var Read: TFastReader);
    /// unserialize the dynamic array content from a TDynArray.SaveTo binary string
    // - same as LoadFrom, and will check for any buffer overflow since we
    // know the actual end of input buffer
    // - will read mORMot 1.18 binary content, but will ignore the Hash32
    // stored checksum which is not needed any more
    function LoadFromBinary(const Buffer: RawByteString): boolean;
    /// serialize the dynamic array content as JSON
    function SaveToJson(EnumSetsAsText: boolean = false;
      reformat: TTextWriterJsonFormat = jsonCompact): RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// serialize the dynamic array content as JSON
    procedure SaveToJson(out result: RawUtf8; EnumSetsAsText: boolean = false;
      reformat: TTextWriterJsonFormat = jsonCompact); overload;
    /// serialize the dynamic array content as JSON
    // - is just a wrapper around TTextWriter.AddTypedJson()
    // - this method will therefore recognize T*ObjArray types
    procedure SaveToJson(out result: RawUtf8; Options: TTextWriterOptions;
      ObjectOptions: TTextWriterWriteObjectOptions = [];
      reformat: TTextWriterJsonFormat = jsonCompact); overload;
    /// serialize the dynamic array content as JSON
    // - is just a wrapper around TTextDateWTTextWriterriter.AddTypedJson()
    // - this method will therefore recognize T*ObjArray types
    procedure SaveToJson(W: TTextWriter;
      ObjectOptions: TTextWriterWriteObjectOptions = []); overload;
    /// load the dynamic array content from an UTF-8 encoded JSON buffer
    // - expect the format as saved by TTextWriter.AddDynArrayJson method, i.e.
    // handling TbooleanDynArray, TIntegerDynArray, TInt64DynArray, TCardinalDynArray,
    // TDoubleDynArray, TCurrencyDynArray, TWordDynArray, TByteDynArray,
    // TRawUtf8DynArray, TWinAnsiDynArray, TRawByteStringDynArray,
    // TStringDynArray, TWideStringDynArray, TSynUnicodeDynArray,
    // TTimeLogDynArray and TDateTimeDynArray as JSON array - or any customized
    // Rtti.RegisterFromText/TRttiJson.RegisterCustomSerializer format
    // - or any other kind of array as Base64 encoded binary stream precessed
    // via JSON_BASE64_MAGIC_C (UTF-8 encoded \uFFF0 special code)
    // - typical handled content could be
    // ! '[1,2,3,4]' or '["\uFFF0base64encodedbinary"]'
    // - return a pointer at the end of the data read from P, nil in case
    // of an invalid input buffer
    // - this method will recognize T*ObjArray types, and will first free
    // any existing instance before unserializing, to avoid memory leak
    // - set e.g. @JSON_[mFast] as CustomVariantOptions parameter to handle
    // complex JSON object or arrays as TDocVariant into variant fields
    // - can use an associated TRawUtf8Interning instance for RawUtf8 values
    // - warning: the content of P^ will be modified during parsing: make a
    // local copy if it will be needed later (using e.g. the overloaded method)
    function LoadFromJson(P: PUtf8Char; EndOfObject: PUtf8Char = nil;
      CustomVariantOptions: PDocVariantOptions = nil; Tolerant: boolean = false;
      Interning: TRawUtf8InterningAbstract = nil): PUtf8Char; overload;
    /// load the dynamic array content from an UTF-8 encoded JSON buffer
    // - this method will make a private copy of the JSON for in-place parsing
    // - returns false in case of invalid input buffer, true on success
    function LoadFromJson(const Json: RawUtf8;
      CustomVariantOptions: PDocVariantOptions = nil; Tolerant: boolean = false;
      Interning: TRawUtf8InterningAbstract = nil): boolean; overload;
    ///  select a sub-section (slice) of a dynamic array content
    procedure Slice(var Dest; Limit: cardinal; Offset: cardinal = 0);
    /// assign the current dynamic array content into a variable
    // - by default (Offset=Limit=0), the whole array is set with no memory
    // (re)allocation, just finalize the Dest slot, then make Inc(RefCnt) and
    // force the internal length/Capacity to equal Count
    // - Offset/Limit could be used to create a new dynamic array with some part
    // of the existing content (Offset<0 meaning from the end):
    // ! SliceAsDynArray(DA);         // items 0..Count-1 (assign with refcount)
    // ! SliceAsDynArray(DA, 10);     // items 10..Count-1
    // ! SliceAsDynArray(DA, 0, 10);  // first 0..9 items
    // ! SliceAsDynArray(DA, 10, 20); // items 10..29 - truncated if Count < 20
    // ! SliceAsDynArray(DA, -10);    // last Count-10..Count-1 items
    procedure SliceAsDynArray(Dest: PPointer; Offset: integer = 0;
      Limit: integer = 0);
    /// add items from a given dynamic array variable
    // - the supplied source DynArray MUST be of the same exact type as the
    // current used for this TDynArray - warning: pass here a reference to
    // a "array of ..." variable, not another TDynArray instance; if you
    // want to add another TDynArray, use AddDynArray() method
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    // - returns the number of items added to the array
    function AddArray(const DynArrayVar; aStartIndex: integer = 0;
      aCount: integer = -1): integer;
    /// add items from a given TDynArray
    // - the supplied source TDynArray MUST be of the same exact type as the
    // current used for this TDynArray, otherwise it won't do anything
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    procedure AddDynArray(aSource: PDynArray; aStartIndex: integer = 0;
      aCount: integer = -1);
    /// compare the content of the two arrays, returning TRUE if both match
    // - use any supplied Compare property (unless ignorecompare=true), or
    // following the RTTI element description on all array items
    // - T*ObjArray kind of arrays will properly compare their properties
    function Equals(B: PDynArray; IgnoreCompare: boolean = false;
      CaseSensitive: boolean = true): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare the content of the two arrays
    // - use any supplied Compare property (unless ignorecompare=true), or
    // following the RTTI element description on all array items
    // - T*ObjArray kind of arrays will properly compare their properties
    function Compares(B: PDynArray; IgnoreCompare: boolean = false;
      CaseSensitive: boolean = true): integer;
    /// set all content of one dynamic array to the current array
    // - both must be of the same exact type
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure Copy(Source: PDynArray; ObjArrayByRef: boolean = false);
    /// set all content of one dynamic array to the current array
    // - both must be of the same exact type
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure CopyFrom(const Source; MaxItem: integer;
      ObjArrayByRef: boolean = false);
    /// set all content of the current dynamic array to another array variable
    // - both must be of the same exact type
    // - resulting length(Dest) will match the exact items count, even if an
    // external Count integer variable is used by this instance
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure CopyTo(out Dest; ObjArrayByRef: boolean = false);
    /// returns a pointer to an element of the array
    // - returns nil if aIndex is out of range
    // - since TDynArray is just a wrapper around an existing array, you should
    // better use direct access to its wrapped variable, and not this (slightly)
    // slower and more error prone method (such pointer access lacks of strong
    // typing abilities), which is designed for TDynArray abstract/internal use
    function ItemPtr(index: PtrInt): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// just a convenient wrapper of Info.Cache.ItemSize
    function ItemSize: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// will copy one element content from its index into another variable
    // - do nothing and return false if index is out of range or Dest is nil
    function ItemCopyAt(index: PtrInt; Dest: pointer): boolean;
      {$ifdef FPC}inline;{$endif}
    /// will move one element content from its index into another variable
    // - will erase the internal item after copy
    // - do nothing and return false if index is out of range or Dest is nil
    function ItemMoveTo(index: PtrInt; Dest: pointer): boolean;
    /// will copy one variable content into an indexed element
    // - do nothing if index is out of range
    // - ClearBeforeCopy will call ItemClear() before the copy, which may be safer
    // if the source item is a copy of Values[index] with some dynamic arrays
    procedure ItemCopyFrom(Source: pointer; index: PtrInt;
      ClearBeforeCopy: boolean = false);
      {$ifdef HASINLINE}inline;{$endif}
    /// compare the content of two items, returning TRUE if both values equal
    // - use the Compare() property function (if set) or using Info.Cache.ItemInfo
    // if available - and fallbacks to binary comparison
    function ItemEquals(A, B: pointer; CaseInSensitive: boolean = false): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare the content of two items, returning -1, 0 or +1s
    // - use the Compare() property function (if set) or using Info.Cache.ItemInfo
    // if available - and fallbacks to binary comparison
    function ItemCompare(A, B: pointer; CaseInSensitive: boolean = false): integer;
    /// will reset the element content
    // - i.e. release any managed type memory, and fill Item with zeros
    procedure ItemClear(Item: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// will fill the element with some random content
    // - this method is thread-safe using Rtti.DoLock/DoUnLock
    procedure ItemRandom(Item: pointer);
    /// will copy one element content
    procedure ItemCopy(Source, Dest: pointer);
      {$ifdef HASINLINE}{$ifndef ISDELPHI2009}inline;{$endif}{$endif}
    /// will copy the first field value of an array element
    // - will use the array KnownType to guess the copy routine to use
    // - returns false if the type information is not enough for a safe copy
    function ItemCopyFirstField(Source, Dest: Pointer): boolean;
    /// save an array element into a serialized binary content
    // - use the same layout as TDynArray.SaveTo, but for a single item
    // - you can use ItemLoad method later to retrieve its content
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write ItemSave(i+10) e.g.)
    function ItemSave(Item: pointer): RawByteString;
    /// load an array element as saved by the ItemSave method into Item variable
    // - warning: Item must be of the same exact type than the dynamic array
    procedure ItemLoad(Source, SourceMax: PAnsiChar; Item: pointer);
    /// load an array element as saved by the ItemSave method
    // - this overloaded method will retrieve the element as a memory buffer,
    // which should be cleared by ItemLoadMemClear() before release
    function ItemLoadMem(Source, SourceMax: PAnsiChar): RawByteString;
    /// search for an array element as saved by the ItemSave method
    // - same as ItemLoad() + Find()/IndexOf() + ItemLoadClear()
    // - will call Find() method if Compare property is set
    // - will call generic IndexOf() method if no Compare property is set
    function ItemLoadFind(Source, SourceMax: PAnsiChar): integer;
    /// finalize a temporary buffer used to store an element via ItemLoadMem()
    // - will release any managed type referenced inside the RawByteString,
    // then void the variable
    // - is just a wrapper around ItemClear(pointer(ItemTemp)) + ItemTemp := ''
    procedure ItemLoadMemClear(var ItemTemp: RawByteString);

    /// retrieve or set the number of items of the dynamic array
    // - same as length(DynArray) or SetLength(DynArray)
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Count: PtrInt
      read GetCount write SetCount;
    /// the internal buffer capacity
    // - if no external Count pointer was set with Init, is the same as Count
    // - if an external Count pointer is set, you can set a value to this
    // property before a massive use of the Add() method e.g.
    // - if no external Count pointer is set, set a value to this property
    // will affect the Count value, i.e. Add() will append after this count
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Capacity: PtrInt
      read GetCapacity write SetCapacity;
    /// the compare function to be used for Sort and Find methods
    // - by default, no comparison function is set
    // - common functions exist for base types: e.g. SortDynArrayByte, SortDynArrayBoolean,
    // SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal, SortDynArraySingle,
    // SortDynArrayInt64, SortDynArrayDouble, SortDynArrayAnsiString,
    // SortDynArrayAnsiStringI, SortDynArrayString, SortDynArrayStringI,
    // SortDynArrayUnicodeString, SortDynArrayUnicodeStringI
    property Compare: TDynArraySortCompare
      read fCompare write SetCompare;
    /// must be TRUE if the array is currently in sorted order according to
    // the compare function
    // - Add/Delete/Insert/Load* methods will reset this property to false
    // - Sort method will set this property to true
    // - you MUST set this property to false if you modify the dynamic array
    // content in your code, so that Find() won't try to wrongly use binary
    // search in an unsorted array, and miss its purpose
    property Sorted: boolean
      read fSorted write fSorted;
    /// can be set to TRUE to avoid any item finalization
    // -  e.g. with T*ObjArray - handle with care to avoid memory leaks
    property NoFinalize: boolean
      read fNoFinalize write fNoFinalize;

    /// low-level direct access to the storage variable
    property Value: PPointer
      read fValue;
    /// low-level extended RTTI access
    // - use e.g. Info.ArrayRtti to access the item RTTI, or Info.Cache.ItemInfo
    // to get the managed item TypeInfo()
    property Info: TRttiCustom
      read fInfo;
    /// low-level direct access to the external count (if defined at Init)
    // - use UseExternalCount() after Init to avoid resetting the count to 0
    property CountExternal: PInteger
      read fCountP;
  end;

  /// just a wrapper record to join a TDynArray, its Count and a TRWLightLock
  TDynArrayLocked = record
    /// lightweight multiple Reads / exclusive Write non-upgradable lock
    Safe: TRWLightLock;
    /// the wrapper to a dynamic array
    DynArray: TDynArray;
    /// will store the length of the TDynArray
    Count: integer;
  end;


{.$define DYNARRAYHASHCOLLISIONCOUNT} // to be defined also in test.core.base

{$ifndef CPU32DELPHI} // Delphi Win32 compiler doesn't like Lemire algorithm

  {$define DYNARRAYHASH_LEMIRE}
  // use the Lemire 64-bit multiplication for faster hash reduction
  // see https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction
  // - generate more collisions with crc32c, but is always faster -> enabled

{$endif CPU32DELPHI}

// use Power-Of-Two sizes for smallest HashTables[], to reduce the hash with AND
// - and Delphi Win32 is not efficient at 64-bit multiplication, anyway
{$define DYNARRAYHASH_PO2}

// use 16-bit Hash table when indexes fit in a word (array Capacity < 65535)
// - to reduce memory consumption and slightly enhance CPU cache efficiency
// - e.g. arrays of size 1..127 use only 256*2=512 bytes for their hash table
{$define DYNARRAYHASH_16BIT}

{$ifdef DYNARRAYHASH_PO2}
const
  /// defined for inlining bitwise division in TDynArrayHasher.HashTableIndex
  // - HashTableSize<=HASH_PO2 is expected to be a power of two (fast binary op);
  // limit is set to 262,144 hash table slots (=1MB), for Capacity=131,072 items
  // - above this limit, a set of increasing primes is used; using a prime as
  // hashtable modulo enhances its distribution, especially for a weak hash function
  // - 64-bit CPU and FPC can efficiently compute a prime reduction using Lemire
  // algorithm, but power of two sizes still have a better practical performance
  // for lower (and most common) content until it consumes too much memory
  HASH_PO2 = 1 shl 18;
{$endif DYNARRAYHASH_PO2}



type
  /// function prototype to be used for hashing of a dynamic array element
  // - this function must use the supplied hasher on the Item data
  TDynArrayHashOne = function(const Item; Hasher: THasher): cardinal;

  /// event handler to be used for hashing of a dynamic array element
  // - can be set as an alternative to TDynArrayHashOne
  TOnDynArrayHashOne = function(const Item): cardinal of object;

  TDynArrayHasherState = set of (
    hasHasher
    {$ifdef DYNARRAYHASH_16BIT} , hash16bit {$endif} );

  /// implements O(1) lookup to any dynamic array content
  // - this won't handle the storage process (like add/update), just efficiently
  // maintain a hash table over an existing dynamic array: several TDynArrayHasher
  // could be applied to a single TDynArray wrapper
  // - TDynArrayHashed will use a TDynArrayHasher on its own storage
  {$ifdef USERECORDWITHMETHODS}
  TDynArrayHasher = record
  {$else}
  TDynArrayHasher = object
  {$endif USERECORDWITHMETHODS}
  private
    fDynArray: PDynArray;
    fHashItem: TDynArrayHashOne;       // function
    fEventHash: TOnDynArrayHashOne;    // function of object
    fHashTableStore: TIntegerDynArray; // store 0 for void entry, or Index+1
    fHashTableSize: integer;
    fState: TDynArrayHasherState;
    fCompare: TDynArraySortCompare;        // function
    fEventCompare: TOnDynArraySortCompare; // function of object
    fHasher: THasher;
    function HashTableIndex(aHashCode: PtrUInt): PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    function HashTableIndexToIndex(aHashTableIndex: PtrInt): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    procedure HashAdd(aHashCode: cardinal; var result: PtrInt);
    procedure HashDelete(aArrayIndex, aHashTableIndex: PtrInt; aHashCode: cardinal);
    procedure RaiseFatalCollision(const caller: shortstring; aHashCode: cardinal);
    procedure HashTableInit(aHasher: THasher);
    procedure SetEventCompare(const Value: TOnDynArraySortCompare);
    procedure SetEventHash(const Value: TOnDynArrayHashOne);
  public
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    /// low-level access to an hash collisions counter for all instance live
    CountCollisions: cardinal;
    /// low-level access to an hash collisions counter for the last HashTable[]
    CountCollisionsCurrent: cardinal;
    /// low-level access to the size of the internal HashTable[]
    HashTableSize: integer;
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    /// initialize the hash table for a given dynamic array storage
    // - you can call this method several times, e.g. if aCaseInsensitive changed
    procedure Init(aDynArray: PDynArray; aHashItem: TDynArrayHashOne;
      const aEventHash: TOnDynArrayHashOne; aHasher: THasher; aCompare: TDynArraySortCompare;
      const aEventCompare: TOnDynArraySortCompare; aCaseInsensitive: boolean);
    /// initialize a known hash table for a given dynamic array storage
    // - you can call this method several times, e.g. if aCaseInsensitive changed
    procedure InitSpecific(aDynArray: PDynArray; aKind: TRttiParserType;
      aCaseInsensitive: boolean; aHasher: THasher);
    /// search for an element value inside the dynamic array without hashing
    function Scan(Item: pointer): PtrInt;
    /// search for an element value inside the dynamic array with hashing
    function Find(Item: pointer): PtrInt; overload;
    /// search for a hashed element value inside the dynamic array with hashing
    function Find(Item: pointer; aHashCode: cardinal): PtrInt; overload;
    /// search for a hash position inside the dynamic array with hashing
    function Find(aHashCode: cardinal; aForAdd: boolean): PtrInt; overload;
    /// returns position in array, or next void index in HashTable[] as -(index+1)
    function FindOrNew(aHashCode: cardinal; Item: pointer; aHashTableIndex: PPtrInt): PtrInt;
    /// returns position in array, or -1 if not found with a custom comparer
    function FindOrNewComp(aHashCode: cardinal; Item: pointer;
      Comp: TDynArraySortCompare = nil): PtrInt;
    /// search an hashed element value for adding, updating the internal hash table
    // - trigger hashing if Count reaches CountTrigger
    function FindBeforeAdd(Item: pointer; out wasAdded: boolean;
      aHashCode: cardinal): PtrInt;
    /// search and delete an element value, updating the internal hash table
    function FindBeforeDelete(Item: pointer): PtrInt;
    /// full computation of the internal hash table
    // - to be called after items have been manually updated - e.g. after Clear
    // - can return the number of duplicated values found (likely to be 0)
    procedure ForceReHash(duplicates: PInteger = nil);
    {$ifndef PUREMORMOT2}
    function ReHash(forced: boolean = false): integer;
    {$endif PUREMORMOT2}
    /// compute the hash of a given item
    function HashOne(Item: pointer): cardinal;
      {$ifdef FPC_OR_DELPHIXE4}inline;{$endif}
      { not inlined to circumvent Delphi 2007=C1632, 2010=C1872, XE3=C2130 }
    /// compare one given item from its index with a value
    // - using either EventCompare() or Compare() functions
    function Equals(Item: pointer; ndx: PtrInt): boolean;
       {$ifdef FPC_OR_DELPHIXE4}inline;{$endif}
    /// retrieve the low-level hash of a given item
    function GetHashFromIndex(aIndex: PtrInt): cardinal;
    /// associated item comparison - may differ from DynArray^.Compare
    property Compare: TDynArraySortCompare
      read fCompare;
    /// custom method-based comparison function
    // - should be set just after Init, when no item has been stored
    property EventCompare: TOnDynArraySortCompare
      read fEventCompare write SetEventCompare;
    /// custom method-based hashing function
    // - should be set just after Init, when no item has been stored
    property EventHash: TOnDynArrayHashOne
      read fEventHash write SetEventHash;
    /// associated item hasher
    property Hasher: THasher
      read fHasher;
  end;

  /// pointer to a TDynArrayHasher instance
  PDynArrayHasher = ^TDynArrayHasher;

type
  /// used to access any dynamic arrray items using fast hash
  // - by default, binary sort could be used for searching items for TDynArray:
  // using a hash is faster on huge arrays for implementing a dictionary
  // - in this current implementation, modification (update or delete) of an
  // element is not handled yet: you should rehash all content - only
  // TDynArrayHashed.FindHashedForAdding / FindHashedAndUpdate /
  // FindHashedAndDelete will refresh the internal hash
  // - this object extends the TDynArray type, since presence of Hashs[] dynamic
  // array will increase code size if using TDynArrayHashed instead of TDynArray
  // - in order to have the better performance, you should use an external Count
  // variable, AND set the Capacity property to the expected maximum count (this
  // will avoid most re-hashing for FindHashedForAdding+FindHashedAndUpdate)
  // - consider using TSynDictionary from mormot.core.json for a thread-safe
  // stand-alone storage of key/value pairs
  {$ifdef UNDIRECTDYNARRAY}
  TDynArrayHashed = record
  // pseudo inheritance for most used methods
  private
    function GetCount: PtrInt; inline;
    procedure SetCount(aCount: PtrInt); inline;
    procedure SetCapacity(aCapacity: PtrInt); inline;
    function GetCapacity: PtrInt; inline;
  public
    InternalDynArray: TDynArray;
    function Value: PPointer; inline;
    function ItemSize: PtrUInt; inline;
    function Info: TRttiCustom; inline;
    procedure Clear; inline;
    procedure ItemCopy(Source, Dest: pointer); inline;
    function ItemPtr(index: PtrInt): pointer; inline;
    function ItemCopyAt(index: PtrInt; Dest: pointer): boolean; inline;
    function Add(const Item): PtrInt; inline;
    procedure Delete(aIndex: PtrInt); inline;
    function SaveTo: RawByteString; overload; inline;
    procedure SaveTo(W: TBufferWriter); overload; inline;
    procedure Sort(aCompare: TDynArraySortCompare = nil); inline;
    function SaveToJson(EnumSetsAsText: boolean = false;
      reformat: TTextWriterJsonFormat = jsonCompact): RawUtf8; overload; inline;
    procedure SaveToJson(out result: RawUtf8; EnumSetsAsText: boolean = false;
      reformat: TTextWriterJsonFormat = jsonCompact); overload; inline;
    procedure SaveToJson(W: TTextWriter); overload; inline;
    function LoadFromJson(P: PUtf8Char; aEndOfObject: PUtf8Char = nil;
      CustomVariantOptions: PDocVariantOptions = nil): PUtf8Char; inline;
    function LoadFrom(Source: PAnsiChar; SourceMax: PAnsiChar
      {$ifndef PUREMORMOT2} = nil{$endif}): PAnsiChar; inline;
    function LoadFromBinary(const Buffer: RawByteString): boolean; inline;
    procedure CreateOrderedIndex(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare);
    property Count: PtrInt read GetCount write SetCount;
    property Capacity: PtrInt read GetCapacity write SetCapacity;
  private
  {$else UNDIRECTDYNARRAY}
  TDynArrayHashed = object(TDynArray)
  protected
  {$endif UNDIRECTDYNARRAY}
    fHash: TDynArrayHasher;
    function GetHashFromIndex(aIndex: PtrInt): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetEventCompare(const cmp: TOnDynArraySortCompare);
    procedure SetEventHash(const hsh: TOnDynArrayHashOne);
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts some hash-dedicated parameters: aHashItem to
    // set how to hash each element, aCompare to handle hash collision
    // - if no aHashItem is supplied, it will hash according to the RTTI, i.e.
    // strings or binary types, and the first field for records (strings included)
    // - if no aCompare is supplied, it will use default Equals() method
    // - if no THasher function is supplied, it will use the one supplied in
    // DefaultHasher global variable, set to crc32c() by default - using
    // SSE4.2 instruction if available
    // - if CaseInsensitive is set to TRUE, it will ignore difference in 7-bit
    // alphabetic characters (e.g. compare 'a' and 'A' as equal)
    procedure Init(aTypeInfo: PRttiInfo; var aValue; aHashItem: TDynArrayHashOne = nil;
      aCompare: TDynArraySortCompare = nil; aHasher: THasher = nil;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false);
    /// initialize the wrapper with a one-dimension dynamic array from our RTTI
    procedure InitRtti(aRtti: TRttiCustom; var aValue; aHashItem: TDynArrayHashOne = nil;
      aCompare: TDynArraySortCompare = nil; aHasher: THasher = nil;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false);
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts to specify how both hashing and comparison should
    // occur, setting the TRttiParserType kind of first/hashed field
    // - djNone and djCustom are too vague, and will raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that aKind matches the dynamic array element definition
    // - aCaseInsensitive will be used for djRawUtf8..djHash512 text comparison
    procedure InitSpecific(aTypeInfo: PRttiInfo; var aValue; aKind: TRttiParserType;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false;
      aHasher: THasher = nil);
    /// will recompute all hash from the current items of the dynamic array
    // - can be called on purpose, when modifications have been performed on
    // the dynamic array content (e.g. in case of element deletion or update,
    // or after calling LoadFrom/Clear method) - this is not necessary after
    // FindHashedForAdding / FindHashedAndUpdate / FindHashedAndDelete methods
    // - returns the number of duplicated items found - which should be 0
    procedure ForceReHash;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifndef PUREMORMOT2}
    function ReHash(forced: boolean = false): integer;
    {$endif PUREMORMOT2}
    /// search for an element value inside the dynamic array using hashing
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods: e.g. if the searched/hashed field in a record is
    // a string as first field, you can safely use a string variable as Item
    // - Item must refer to a variable: e.g. you can't write FindHashed(i+10)
    // - will call fHashItem(Item,fHasher) to compute the needed hash
    // - returns -1 if not found, or the index in the dynamic array if found
    function FindHashed(const Item): PtrInt;
      {$ifdef FPC} inline; {$endif}
    /// search for an element value inside the dynamic array using its hash
    // - returns -1 if not found, or the index in the dynamic array if found
    // - aHashCode parameter constains an already hashed value of the item,
    // to be used e.g. after a call to HashFind()
    function FindFromHash(const Item; aHashCode: cardinal): PtrInt;
    /// search for an element value inside the dynamic array using hashing, and
    // fill ItemToFill with the found content
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - ItemToFill should be of the type expected by the dynamic array, since
    // all its fields will be set on match
    function FindHashedAndFill(var ItemToFill): PtrInt;
    /// search for an element value inside the dynamic array using hashing, and
    // add a void entry to the array if was not found (unless noAddEntry is set)
    // - this method will use hashing for fast retrieval
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods: e.g. if the searched/hashed field in a record is
    // a string as first field, you can safely use a string variable as Item
    // - returns either the index in the dynamic array if found (and set wasAdded
    // to false), either the newly created index in the dynamic array (and set
    // wasAdded to true)
    // - for faster process (avoid rehash), please set the Capacity property
    // - warning: in contrast to the Add() method, if an entry is added to the
    // array (wasAdded=true), the entry is left VOID: you must set the field
    // content to expecting value - in short, Item is used only for searching,
    // not copied to the newly created entry in the array  - check
    // FindHashedAndUpdate() for a method actually copying Item fields
    function FindHashedForAdding(const Item; out wasAdded: boolean;
      noAddEntry: boolean = false): PtrInt; overload;
    /// search for an element value inside the dynamic array using hashing, and
    // add a void entry to the array if was not found (unless noAddEntry is set)
    // - overloaded method accepting an already hashed value of the item, to be
    // used e.g. after a call to HashFind()
    function FindHashedForAdding(const Item; out wasAdded: boolean;
      aHashCode: cardinal; noAddEntry: boolean = false): PtrInt; overload;
    /// ensure a given element name is unique, then add it to the array
    // - expected element layout is to have a RawUtf8 field at first position
    // - the aName is searched (using hashing) to be unique, and if not the case,
    // an ESynException.CreateUtf8() is raised with the supplied arguments
    // - use internally FindHashedForAdding method
    // - this version will set the field content with the unique value
    // - returns a pointer to the newly added element (to set other fields)
    function AddUniqueName(const aName: RawUtf8; const ExceptionMsg: RawUtf8;
      const ExceptionArgs: array of const;
      aNewIndex: PPtrInt = nil): pointer; overload;
    /// ensure a given element name is unique, then add it to the array
    // - just a wrapper to AddUniqueName(aName,'',[],aNewIndex)
    function AddUniqueName(const aName: RawUtf8;
      aNewIndex: PPtrInt = nil): pointer; overload;
    /// search for a given element name, make it unique, and add it to the array
    // - expected element layout is to have a RawUtf8 field at first position
    // - the aName is searched (using hashing) to be unique, and if not the case,
    // some suffix is added to make it unique, counting from _1 to _999
    // - use internally FindHashedForAdding method
    // - this version will set the field content with the unique value
    // - returns a pointer to the newly added element (to set other fields)
    function AddAndMakeUniqueName(aName: RawUtf8): pointer;
    /// search for an element value inside the dynamic array using hashing, then
    // update any matching item, or add the item if none matched
    // - by design, hashed field shouldn't have been modified by this update,
    // otherwise the method won't be able to find and update the old hash: in
    // this case, you should first call FindHashedAndDelete(OldItem) then
    // FindHashedForAdding(NewItem) to properly handle the internal hash table
    // - if AddIfNotExisting is FALSE, returns the index found (0..Count-1),
    // or -1 if Item was not found - Update will force slow rehash all content
    // - if AddIfNotExisting is TRUE, returns the index found (0..Count-1),
    // or the index newly created/added is the Item value was not matching -
    // add won't rehash all content - for even faster process (avoid rehash),
    // please set the Capacity property
    // - Item should be of the type expected by the dynamic array, since its
    // content will be copied into the dynamic array, and it must refer to a
    // variable: e.g. you can't write FindHashedAndUpdate(i+10)
    function FindHashedAndUpdate(const Item; AddIfNotExisting: boolean): PtrInt;
    /// search for an element value inside the dynamic array using hashing, and
    // delete it if matchs
    // - return the index deleted (0..Count-1), or -1 if Item was not found
    // - can optionally copy the deleted item to FillDeleted^ before erased
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods, and must refer to a variable: e.g. you can't
    // write FindHashedAndDelete(i+10)
    // - it won't call slow ForceReHash but refresh the hash table as needed
    function FindHashedAndDelete(const Item; FillDeleted: pointer = nil;
      noDeleteEntry: boolean = false): PtrInt;
    /// search for an element value inside the dynamic array without hashing
    // - is preferred to Find(), since EventCompare would be used if defined
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods, and must refer to a variable: e.g. you can't
    // write Scan(i+10)
    // - returns -1 if not found, or the index in the dynamic array if found
    function Scan(const Item): PtrInt;
    /// retrieve the hash value of a given item, from its index
    property Hash[aIndex: PtrInt]: cardinal
      read GetHashFromIndex;
    /// alternative event-oriented Compare function to be used for Sort and Find
    // - will be used instead of Compare, to allow object-oriented callbacks
    // - should be set just after Init, when not item has been stored
    property EventCompare: TOnDynArraySortCompare
      read fHash.fEventCompare write SetEventCompare;
    /// custom hash function used for hashing of a dynamic array element
    property HashItem: TDynArrayHashOne
      read fHash.fHashItem;
    /// alternative event-oriented Hash function
    // - this object-oriented callback will be used instead of HashItem()
    // on each dynamic array entries - HashItem will still be used on
    // const Item values, since they may be just a sub part of the stored entry
    // - should be set just after Init, when not item has been stored
    property EventHash: TOnDynArrayHashOne
      read fHash.fEventHash write SetEventHash;
    /// access to the internal hash table
    // - you can call e.g. Hasher.Clear to invalidate the whole hash table
    property Hasher: TDynArrayHasher
      read fHash;
  end;


/// initialize the structure with a one-dimension dynamic array
// - the dynamic array must have been defined with its own type
// (e.g. TIntegerDynArray = array of integer)
// - if aCountPointer is set, it will be used instead of length() to store
// the dynamic array items count - it will be much faster when adding
// elements to the array, because the dynamic array won't need to be
// resized each time - but in this case, you should use the Count property
// instead of length(array) or high(array) when accessing the data: in fact
// length(array) will store the memory size reserved, not the items count
// - if aCountPointer is set, its content will be set to 0, whatever the
// array length is, or the current aCountPointer^ value is
// - a typical usage could be:
// !var
// !  IntArray: TIntegerDynArray;
// !begin
// !  with DynArray(TypeInfo(TIntegerDynArray), IntArray) do
// !  begin
// !    (...)
// !  end;
// ! (...)
// ! bin := DynArray(TypeInfo(TIntegerDynArray), IntArray).SaveTo;
function DynArray(aTypeInfo: PRttiInfo; var aValue;
  aCountPointer: PInteger = nil): TDynArray;
  {$ifdef HASINLINE}inline;{$endif}

/// get the hash function corresponding to a given standard array type
// - as used e.g. internally by TDynArrayHasher.Init
function DynArrayHashOne(Kind: TRttiParserType;
  CaseInsensitive: boolean = false): TDynArrayHashOne;

/// sort any dynamic array, generating an external array of indexes
// - this function will use the supplied TSynTempBuffer for index storage,
// so use PIntegerArray(Indexes.buf) to access the values
// - caller should always make Indexes.Done once finshed
procedure DynArraySortIndexed(Values: pointer; ItemSize, Count: integer;
  out Indexes: TSynTempBuffer; Compare: TDynArraySortCompare); overload;

/// sort any dynamic array, via a supplied array of indexes
// - this function expects Indexes[] to be already allocated and filled
procedure DynArraySortIndexed(Values: pointer; ItemSize, Count: integer;
  Indexes: PCardinalArray; Compare: TDynArraySortCompare); overload;

/// get the comparison function corresponding to a given standard array type
// - as used e.g. internally by TDynArray
function DynArraySortOne(Kind: TRttiParserType; CaseInsensitive: boolean): TDynArraySortCompare;

/// sort any TObjArray with a given comparison function
procedure ObjArraySort(var aValue; Compare: TDynArraySortCompare;
  CountPointer: PInteger = nil);


{ *************** Integer Arrays Extended Process }

type
  /// event handler called by NotifySortedIntegerChanges()
  // - Sender is an opaque const value, maybe a TObject or any pointer
  TOnNotifySortedIntegerChange = procedure(const Sender; Value: integer) of object;

/// compares two 32-bit signed sorted integer arrays, and call event handlers
// to notify the corresponding modifications in an O(n) time
// - items in both old[] and new[] arrays are required to be sorted
procedure NotifySortedIntegerChanges(old, new: PIntegerArray; oldn, newn: PtrInt;
  const added, deleted: TOnNotifySortedIntegerChange; const sender);

/// copy an integer array, then sort it, low values first
procedure CopyAndSortInteger(Values: PIntegerArray; ValuesCount: integer;
  var Dest: TIntegerDynArray);

/// copy an integer array, then sort it, low values first
procedure CopyAndSortInt64(Values: PInt64Array; ValuesCount: integer;
  var Dest: TInt64DynArray);

/// remove some 32-bit integer from Values[]
// - Excluded is declared as var, since it will be sorted in-place during process
// if it contains more than ExcludedSortSize items (i.e. if the sort is worth it)
procedure ExcludeInteger(var Values, Excluded: TIntegerDynArray;
  ExcludedSortSize: integer = 32);

/// ensure some 32-bit integer from Values[] will only contain Included[]
// - Included is declared as var, since it will be sorted in-place during process
// if it contains more than IncludedSortSize items (i.e. if the sort is worth it)
procedure IncludeInteger(var Values, Included: TIntegerDynArray;
  IncludedSortSize: integer = 32);

/// sort and remove any 32-bit duplicated integer from Values[]
procedure DeduplicateInteger(var Values: TIntegerDynArray); overload;

/// sort and remove any 32-bit duplicated integer from Values[]
// - returns the new Values[] length
function DeduplicateInteger(var Values: TIntegerDynArray; Count: PtrInt): PtrInt; overload;

/// low-level function called by DeduplicateInteger()
function DeduplicateIntegerSorted(val: PIntegerArray; last: PtrInt): PtrInt;

/// create a new 32-bit integer dynamic array with the values from another one
procedure CopyInteger(const Source: TIntegerDynArray; out Dest: TIntegerDynArray);

/// remove some 64-bit integer from Values[]
// - Excluded is declared as var, since it will be sorted in-place during process
// if it contains more than ExcludedSortSize items (i.e. if the sort is worth it)
procedure ExcludeInt64(var Values, Excluded: TInt64DynArray;
  ExcludedSortSize: integer = 32);

/// ensure some 64-bit integer from Values[] will only contain Included[]
// - Included is declared as var, since it will be sorted in-place during process
// if it contains more than IncludedSortSize items (i.e. if the sort is worth it)
procedure IncludeInt64(var Values, Included: TInt64DynArray;
  IncludedSortSize: integer = 32);

/// sort and remove any 64-bit duplicated integer from Values[]
procedure DeduplicateInt64(var Values: TInt64DynArray); overload;

/// sort and remove any 64-bit duplicated integer from Values[]
// - returns the new Values[] length
function DeduplicateInt64(var Values: TInt64DynArray; Count: PtrInt): PtrInt; overload;

/// low-level function called by DeduplicateInt64()
// - warning: caller should ensure that last>0
function DeduplicateInt64Sorted(val: PInt64Array; last: PtrInt): PtrInt;

/// create a new 64-bit integer dynamic array with the values from another one
procedure CopyInt64(const Source: TInt64DynArray; out Dest: TInt64DynArray);

/// find the maximum 32-bit integer in Values[]
function MaxInteger(const Values: TIntegerDynArray; ValuesCount: PtrInt;
  MaxStart: integer = -1): integer;

/// sum all 32-bit integers in Values[]
function SumInteger(const Values: TIntegerDynArray; ValuesCount: PtrInt): integer;

/// fill already allocated Reversed[] so that Reversed[Values[i]]=i
procedure Reverse(const Values: TIntegerDynArray; ValuesCount: PtrInt;
  Reversed: PIntegerArray);

/// copy some Int64 values into an unsigned integer array
procedure Int64ToUInt32(Values64: PInt64Array; Values32: PCardinalArray; Count: PtrInt);

type
  /// comparison function as expected by MedianQuickSelect()
  // - should return TRUE if Values[IndexA]>Values[IndexB]
  TOnValueGreater = function(IndexA, IndexB: PtrInt): boolean of object;

/// compute the median of a serie of values, using "Quickselect"
// - based on the algorithm described in "Numerical recipes in C", Second Edition
// - expect the values information to be available from a comparison callback
// - this version will use a temporary index list to exchange items order
// (supplied as a TSynTempBuffer), so won't change the supplied values themself
// - see also function MedianQuickSelectInteger() for PIntegerArray values
// - returns the index of the median Value
function MedianQuickSelect(const OnCompare: TOnValueGreater; n: integer;
  var TempBuffer: TSynTempBuffer): integer;

/// compute the median of an integer serie of values, using "Quickselect"
// - based on the algorithm described in "Numerical recipes in C", Second Edition,
// translated from Nicolas Devillard's C code: http://ndevilla.free.fr/median/median
// - warning: the supplied integer array is modified in-place during the process,
// and won't be fully sorted on output (this is no QuickSort alternative)
function MedianQuickSelectInteger(Values: PIntegerArray; n: integer): integer;


/// fast search of a binary value position in a fixed-size array
// - Count is the number of entries in P^[]
// - return index of P^[index]=V^, comparing VSize bytes
// - return -1 if Value was not found
function AnyScanIndex(P, V: pointer; Count, VSize: PtrInt): PtrInt;

/// fast search of a binary value position in a fixed-size array
// - Count is the number of entries in P^[]
function AnyScanExists(P, V: pointer; Count, VSize: PtrInt): boolean;
  {$ifdef HASINLINE} inline; {$endif}


{ ************ INI Files and In-memory Access }

/// find a Name= Value in a [Section] of a INI RawUtf8 Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntry(const Content, Section, Name: RawUtf8;
  const DefaultValue: RawUtf8 = ''): RawUtf8;

/// find a Name= Value in a [Section] of a INI WinAnsi Content
// - same as FindIniEntry(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniEntry(const Content, Section, Name: RawUtf8): RawUtf8;

/// find a Name= numeric Value in a [Section] of a INI RawUtf8 Content and
// return it as an integer, or 0 if not found
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryInteger(const Content, Section, Name: RawUtf8): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// find a Name= Value in a [Section] of a .INI file
// - if Section equals '', find the Name= value before any [Section]
// - use internally fast FindIniEntry() function above
function FindIniEntryFile(const FileName: TFileName;
  const Section, Name: RawUtf8; const DefaultValue: RawUtf8 = ''): RawUtf8;

/// update a Name= Value in a [Section] of a INI RawUtf8 Content
// - this function scans and update the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', update the Name= value before any [Section]
procedure UpdateIniEntry(var Content: RawUtf8; const Section, Name, Value: RawUtf8);

/// update a Name= Value in a [Section] of a .INI file
// - if Section equals '', update the Name= value before any [Section]
// - use internally fast UpdateIniEntry() function above
procedure UpdateIniEntryFile(const FileName: TFileName; const Section, Name, Value: RawUtf8);

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
function FindSectionFirstLine(var source: PUtf8Char; search: PAnsiChar): boolean;

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
// - this version expects source^ to point to an Unicode char array
function FindSectionFirstLineW(var source: PWideChar; search: PUtf8Char): boolean;

/// retrieve the whole content of a section as a string
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function GetSectionContent(SectionFirstLine: PUtf8Char): RawUtf8; overload;

/// retrieve the whole content of a section as a string
// - use SectionFirstLine() then previous GetSectionContent()
function GetSectionContent(const Content, SectionName: RawUtf8): RawUtf8; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
function DeleteSection(var Content: RawUtf8; const SectionName: RawUtf8;
  EraseSectionHeader: boolean = true): boolean; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function DeleteSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  EraseSectionHeader: boolean = true): boolean; overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
procedure ReplaceSection(var Content: RawUtf8; const SectionName,
  NewSectionContent: RawUtf8); overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
procedure ReplaceSection(SectionFirstLine: PUtf8Char;
  var Content: RawUtf8; const NewSectionContent: RawUtf8); overload;

/// return TRUE if Value of UpperName does exist in P, till end of current section
// - expect UpperName as 'NAME='
function ExistsIniName(P: PUtf8Char; UpperName: PAnsiChar): boolean;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
function FindIniNameValue(P: PUtf8Char; UpperName: PAnsiChar;
  const DefaultValue: RawUtf8 = ''): RawUtf8;

/// return TRUE if one of the Value of UpperName exists in P, till end of
// current section
// - expect UpperName e.g. as 'CONTENT-TYPE: '
// - expect UpperValues to be an array of upper values with left side matching,
// and ending with nil - as expected by IdemPPChar(), i.e. with at least 2 chars
function ExistsIniNameValue(P: PUtf8Char; const UpperName: RawUtf8;
  UpperValues: PPAnsiChar): boolean;

/// find the integer Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - return 0 if no NAME= entry was found
function FindIniNameValueInteger(P: PUtf8Char; const UpperName: RawUtf8): PtrInt;

/// replace a value from a given set of name=value lines
// - expect UpperName as 'UPPERNAME=', otherwise returns false
// - if no UPPERNAME= entry was found, then Name+NewValue is added to Content
// - a typical use may be:
// ! UpdateIniNameValue(headers,HEADER_CONTENT_TYPE,HEADER_CONTENT_TYPE_UPPER,contenttype);
function UpdateIniNameValue(var Content: RawUtf8;
  const Name, UpperName, NewValue: RawUtf8): boolean;

/// fill a class Instance properties from an .ini content
// - the class property fields are searched in the supplied main SectionName
// - nested objects and multi-line text values are searched in their own section,
// named from their section level and property (e.g. [mainprop.nested1.nested2])
// - returns true if at least one property has been identified
function IniToObject(const Ini: RawUtf8; Instance: TObject;
  const SectionName: RawUtf8 = 'Main'; DocVariantOptions: PDocVariantOptions = nil;
  Level: integer = 0): boolean;

/// serialize a class Instance properties into an .ini content
// - the class property fields are written in the supplied main SectionName
// - nested objects and multi-line text values are written in their own section,
// named from their section level and property (e.g. [mainprop.nested1.nested2])
function ObjectToIni(const Instance: TObject; const SectionName: RawUtf8 = 'Main';
  Options: TTextWriterWriteObjectOptions =
    [woEnumSetsAsText, woRawBlobAsBase64, woHumanReadableEnumSetAsComment];
    Level: integer = 0): RawUtf8;

/// returns TRUE if the supplied HTML Headers contains 'Content-Type: text/...',
// 'Content-Type: application/json' or 'Content-Type: application/xml'
function IsHtmlContentTypeTextual(Headers: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// search if the WebSocketUpgrade() header is present
// - consider checking the hsrConnectionUpgrade flag instead
function IsWebSocketUpgrade(headers: PUtf8Char): boolean;


{ ************ RawUtf8 String Values Interning and TRawUtf8List }

type
  /// store a TRawUtf8DynArray with its efficient hash table
  {$ifdef USERECORDWITHMETHODS}
  TRawUtf8Hashed = record
  {$else}
  TRawUtf8Hashed = object
  {$endif USERECORDWITHMETHODS}
  public
    Count: integer;
    Value: TRawUtf8DynArray;
    Values: TDynArrayHashed;
    /// initialize the RawUtf8 dynamic array and hasher
    procedure Init;
  end;

  /// used to store one list of hashed RawUtf8 in TRawUtf8Interning pool
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  // - each slot has its own TRWLightLock for efficient concurrent reads
  {$ifdef USERECORDWITHMETHODS}
  TRawUtf8InterningSlot = record
  {$else}
  TRawUtf8InterningSlot = object
  {$endif USERECORDWITHMETHODS}
  private
    fSafe: TRWLightLock;
    fHash: TRawUtf8Hashed;
  public
    /// initialize the RawUtf8 slot (and its Safe mutex)
    procedure Init;
    /// returns the interned RawUtf8 value
    procedure Unique(var aResult: RawUtf8; const aText: RawUtf8;
      aTextHash: cardinal);
    /// returns the interned RawUtf8 value
    // - only allocates new aResult string if needed
    procedure UniqueFromBuffer(var aResult: RawUtf8;
      aText: PUtf8Char; aTextLen: PtrInt; aTextHash: cardinal);
    /// ensure the supplied RawUtf8 value is interned
    procedure UniqueText(var aText: RawUtf8; aTextHash: cardinal);
    /// return the interned value, if any
    function Existing(const aText: RawUtf8; aTextHash: cardinal): pointer;
    /// delete all stored RawUtf8 values
    procedure Clear;
    /// reclaim any unique RawUtf8 values
    // - any string with an usage count <= aMaxRefCount will be removed
    function Clean(aMaxRefCount: TStrCnt): integer;
    /// how many items are currently stored in Value[]
    property Count: integer
      read fHash.Count;
  end;
  PRawUtf8InterningSlot = ^TRawUtf8InterningSlot;

  /// allow to store only one copy of distinct RawUtf8 values
  // - thanks to the Copy-On-Write feature of string variables, this may
  // reduce a lot the memory overhead of duplicated text content
  // - this class is thread-safe and optimized for performance
  TRawUtf8Interning = class(TRawUtf8InterningAbstract)
  protected
    fPool: array of TRawUtf8InterningSlot;
    fPoolLast: integer;
  public
    /// initialize the storage and its internal hash pools
    // - aHashTables is the pool size, and should be a power of two <= 512
    // (1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
    constructor Create(aHashTables: integer = 4); reintroduce;
    /// return a RawUtf8 variable stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    function Unique(const aText: RawUtf8): RawUtf8; overload;
    /// check if a RawUtf8 value is already stored within this class
    // - if not existing, returns nil and don't add it to the pool
    // - if existing, returns pointer(fValue[i]) of the unique stored RawUtf8
    // - use e.g. for very fast per-pointer lookup of interned property names
    function Existing(const aText: RawUtf8): pointer;
    /// return a RawUtf8 variable stored within this class from a text buffer
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    function Unique(aText: PUtf8Char; aTextLen: PtrInt): RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return a RawUtf8 variable stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    procedure Unique(var aResult: RawUtf8; const aText: RawUtf8); overload;
    /// return a RawUtf8 variable stored within this class from a text buffer
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    // - this method won't allocate any memory if aText is already interned
    procedure Unique(var aResult: RawUtf8; aText: PUtf8Char; aTextLen: PtrInt); overload;
    /// ensure a RawUtf8 variable is stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, set the shared
    // instance (with its reference counter increased), to reduce memory usage
    procedure UniqueText(var aText: RawUtf8);
    /// return a variant containing a RawUtf8 stored within this class
    // - similar to RawUtf8ToVariant(), but with string interning
    // - see also UniqueVariant() from mormot.core.variants if you want to
    // intern only non-numerical values
    procedure UniqueVariant(var aResult: variant; const aText: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return a variant containing a RawUtf8 stored within this class
    // - similar to RawUtf8ToVariant(StringToUtf8()), but with string interning
    // - this method expects the text to be supplied as a RTL string, which will
    // be converted into a variant containing a RawUtf8 varString instance
    procedure UniqueVariantString(var aResult: variant; const aText: string);
    /// ensure a variant contains only RawUtf8 stored within this class
    // - supplied variant should be a varString containing a RawUtf8 value
    procedure UniqueVariant(var aResult: variant); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// delete any previous storage pool
    procedure Clear;
    /// reclaim any unique RawUtf8 values
    // - i.e. run a garbage collection process of all values with RefCount=1
    // by default, i.e. all string which are not used any more; you may set
    // aMaxRefCount to a higher value, depending on your expecations, i.e. 2 to
    // delete all string which are referenced only once outside of the pool
    // - returns the number of unique RawUtf8 cleaned from the internal pool
    // - to be executed on a regular basis - but not too often, since the
    // process can be time consumming, and void the benefit of interning
    function Clean(aMaxRefCount: TStrCnt = 1): integer;
    /// how many items are currently stored in this instance
    function Count: integer;
  end;

  /// possible values used by TRawUtf8List.Flags
  TRawUtf8ListFlags = set of (
    fObjectsOwned,
    fCaseSensitive,
    fNoDuplicate,
    fOnChangeTrigerred,
    fThreadSafe);

  /// thread-safe TStringList-class optimized for our native UTF-8 string type
  // - can optionally store associated some TObject instances
  // - high-level methods of this class are thread-safe
  // - if fNoDuplicate flag is defined, an internal hash table will be
  // maintained to perform IndexOf() lookups in O(1) linear way
  // - not thread-safe by default, unless fThreadSafe is set to use the TRWLock
  TRawUtf8List = class(TSynPersistentRWLock)
  protected
    fCount: PtrInt;
    fValue: TRawUtf8DynArray;
    fValues: TDynArrayHashed;
    fObjects: TObjectDynArray;
    fFlags: TRawUtf8ListFlags;
    fNameValueSep: AnsiChar;
    fOnChange, fOnChangeBackupForBeginUpdate: TNotifyEvent;
    fOnChangeLevel: integer;
    function GetCount: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetCapacity(const capa: PtrInt);
    function GetCapacity: PtrInt;
    function Get(Index: PtrInt): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Put(Index: PtrInt; const Value: RawUtf8);
    function GetS(Index: PtrInt): string;
    procedure PutS(Index: PtrInt; const Value: string);
    function GetObject(Index: PtrInt): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    procedure PutObject(Index: PtrInt; Value: pointer);
    function GetName(Index: PtrInt): RawUtf8;
    function GetValue(const Name: RawUtf8): RawUtf8;
    procedure SetValue(const Name, Value: RawUtf8);
    function GetTextCRLF: RawUtf8;
    procedure SetTextCRLF(const Value: RawUtf8);
    procedure SetTextPtr(P, PEnd: PUtf8Char; const Delimiter: RawUtf8);
    function GetTextPtr: PPUtf8CharArray;
      {$ifdef HASINLINE}inline;{$endif}
    function GetNoDuplicate: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function GetObjectPtr: PPointerArray;
      {$ifdef HASINLINE}inline;{$endif}
    function GetCaseSensitive: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetCaseSensitive(Value: boolean); virtual;
    procedure Changed; virtual;
    procedure InternalDelete(Index: PtrInt);
    procedure OnChangeHidden(Sender: TObject);
    {$ifndef PUREMORMOT2}
    procedure SetDefaultFlags; virtual;
    {$endif PUREMORMOT2}
  public
    /// initialize the RawUtf8/Objects storage with [fCaseSensitive] flags
    constructor Create; overload; override;
    /// initialize the RawUtf8/Objects storage with extended flags
    // - by default, any associated Objects[] are just weak references;
    // you may supply fOwnObjects flag to force object instance management
    // - if you want the stored text items to be unique, set fNoDuplicate
    // and then an internal hash table will be maintained for fast IndexOf()
    // - you can set fCaseSensitive to let the UTF-8 lookup be case-sensitive
    // - not thread-safe by default, unless fThreadSafe is set to use a R/W lock
    // - is defined as CreateEx instead of overload Create to avoid weird Delphi
    // compilation issues, especially within packages
    constructor CreateEx(aFlags: TRawUtf8ListFlags);
    {$ifndef PUREMORMOT2}
    /// backward compatiliby overloaded constructor
    // - please rather use the overloaded CreateEx(TRawUtf8ListFlags)
    // - for instance, Create(true) is CreateEx([fObjectsOwned, fCaseSensitive]);
    constructor Create(aOwnObjects: boolean; aNoDuplicate: boolean = false;
      aCaseSensitive: boolean = true); reintroduce; overload;
    {$endif PUREMORMOT2}
    /// finalize the internal objects stored
    // - if instance was created with fOwnObjects flag
    destructor Destroy; override;
    /// get a stored Object item by its associated UTF-8 text
    // - returns nil and raise no exception if aText doesn't exist
    // - thread-safe method, unless returned TObject is deleted in the background
    function GetObjectFrom(const aText: RawUtf8): pointer;
    /// store a new RawUtf8 item
    // - without the fNoDuplicate flag, it will always add the supplied value
    // - if fNoDuplicate was set and aText already exists (using the internal
    // hash table), it will return -1 unless aRaiseExceptionIfExisting is forced
    // - thread-safe method
    function Add(const aText: RawUtf8;
      aRaiseExceptionIfExisting: boolean = false): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// store a new RawUtf8 item, and its associated TObject
    // - without the fNoDuplicate flag, it will always add the supplied value
    // - if fNoDuplicate was set and aText already exists (using the internal hash
    // table), it will return -1 unless aRaiseExceptionIfExisting is forced;
    // optionally freeing the supplied aObject if aFreeAndReturnExistingObject
    // is set, in which pointer the existing Objects[] is copied (see
    // AddObjectUnique as a convenient wrapper around this behavior);
    // if aFreeAndReturnExistingObject is nil, and aReplaceExistingObject is
    // true, the existing object is freed and replaced by aObject
    // - thread-safe method
    function AddObject(const aText: RawUtf8; aObject: TObject;
      aRaiseExceptionIfExisting: boolean = false;
      aFreeAndReturnExistingObject: PPointer = nil;
      aReplaceExistingObject: boolean = false): PtrInt;
    /// try to store a new RawUtf8 item and its associated TObject
    // - fNoDuplicate should have been specified in the list flags
    // - if aText doesn't exist, will add the values
    // - if aText exist, will call aObjectToAddOrFree.Free and set the value
    // already stored in Objects[] into aObjectToAddOrFree - allowing dual
    // commit thread-safe update of the list, e.g. after a previous unsuccessful
    // call to GetObjectFrom(aText)
    // - thread-safe method, using an internal Hash Table to speedup IndexOf()
    // - in fact, this method is just a wrapper around
    // ! AddObject(aText,aObjectToAddOrFree^,false,@aObjectToAddOrFree);
    procedure AddObjectUnique(const aText: RawUtf8; aObjectToAddOrFree: PPointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// force the storage of a RawUtf8 item, and its associated TObject
    // - without the fNoDuplicate flag, it will always add the supplied value
    // - if fNoDuplicate was set and aText already exists (using the internal hash
    // table), it will free any existing Objects[] and put aObject in its place
    // - thread-safe method, using an internal Hash Table to speedup IndexOf()
    function AddOrReplaceObject(const aText: RawUtf8; aObject: TObject): PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a specified list to the current content
    // - thread-safe method
    procedure AddRawUtf8List(List: TRawUtf8List);
    /// delete a stored RawUtf8 item, and its associated TObject
    // - raise no exception in case of out of range supplied index
    // - this method is not thread-safe: use Safe.Lock/UnLock if needed
    procedure Delete(Index: PtrInt); overload;
    /// delete a stored RawUtf8 item, and its associated TObject
    // - will search for the value using IndexOf(aText), and returns its index
    // - returns -1 if no entry was found and deleted
    // - thread-safe method, using the internal Hash Table if fNoDuplicate is set
    function Delete(const aText: RawUtf8): PtrInt; overload;
    /// delete a stored RawUtf8 item, and its associated TObject, from
    // a given Name when stored as 'Name=Value' pairs
    // - raise no exception in case of out of range supplied index
    // - thread-safe method, but not using the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function DeleteFromName(const Name: RawUtf8): PtrInt; virtual;
    /// find the index of a given Name when stored as 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    // - this method is not thread-safe, and won't use the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function IndexOfName(const Name: RawUtf8): PtrInt;
    /// access to the Value of a given 'Name=Value' pair at a given position
    // - this method is not thread-safe
    // - consider using TSynNameValue if you expect efficient name/value process
    function GetValueAt(Index: PtrInt): RawUtf8;
    /// compare a Value with some RawUtf8 text
    // - this method is not thread-safe
    function EqualValueAt(Index: PtrInt; const aText: RawUtf8): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve Value from an existing Name=Value, then optinally delete the entry
    // - if Name is found, will fill Value with the stored content and return true
    // - if Name is not found, Value is not modified, and false is returned
    // - thread-safe method, but not using the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function UpdateValue(const Name: RawUtf8; var Value: RawUtf8;
      ThenDelete: boolean): boolean;
    /// retrieve and delete the first RawUtf8 item in the list
    // - could be used as a FIFO, calling Add() as a "push" method
    // - thread-safe method
    function PopFirst(out aText: RawUtf8; aObject: PObject = nil): boolean;
    /// retrieve and delete the last RawUtf8 item in the list
    // - could be used as a FILO, calling Add() as a "push" method
    // - thread-safe method
    function PopLast(out aText: RawUtf8; aObject: PObject = nil): boolean;
    /// erase all stored RawUtf8 items
    // - and corresponding objects (if aOwnObjects was true at constructor)
    // - thread-safe method, also clearing the internal Hash Table
    procedure Clear; virtual;
    /// find a RawUtf8 item in the stored Strings[] list
    // - this search is case sensitive if fCaseSensitive flag was set (which
    // is the default)
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - see also Exists() and GetObjectFrom() method
    // - uses the internal Hash Table if fNoDuplicate was set
    function IndexOf(const aText: RawUtf8): PtrInt;
    /// find a RawUtf8 item in the stored Strings[] list
    // - search is case sensitive if fCaseSensitive flag was set (default)
    // - this method is thread-safe
    // - uses the internal Hash Table if fNoDuplicate was set
    function Exists(const aText: RawUtf8): boolean;
    /// find a TObject item index in the stored Objects[] list
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - aObject lookup won't use the internal Hash Table
    function IndexOfObject(aObject: TObject): PtrInt;
    /// search for any RawUtf8 item containing some text
    // - uses PosEx() on the stored lines
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - by design, aText lookup can't use the internal Hash Table
    function Contains(const aText: RawUtf8; aFirstIndex: integer = 0): PtrInt;
    /// retrieve the all lines, separated by the supplied delimiter
    // - this method is thread-safe
    function GetText(const Delimiter: RawUtf8 = #13#10): RawUtf8;
    /// the OnChange event will be raised only when EndUpdate will be called
    // - this method will also call Safe.Lock for thread-safety
    procedure BeginUpdate;
    /// call the OnChange event if changes occurred
    // - this method will also call Safe.UnLock for thread-safety
    procedure EndUpdate;
    /// set low-level text and objects from existing arrays
    procedure SetFrom(const aText: TRawUtf8DynArray; const aObject: TObjectDynArray);
    /// set all lines, separated by the supplied delimiter
    // - this method is thread-safe
    procedure SetText(const aText: RawUtf8; const Delimiter: RawUtf8 = #13#10);
    /// set all lines from a text file
    // - will assume text file with no BOM is already UTF-8 encoded
    // - this method is thread-safe
    procedure LoadFromFile(const FileName: TFileName);
    /// write all lines into the supplied stream
    // - this method is thread-safe
    procedure SaveToStream(Dest: TStream; const Delimiter: RawUtf8 = #13#10);
    /// write all lines into a new UTF-8 file
    // - this method is thread-safe
    procedure SaveToFile(const FileName: TFileName; const Delimiter: RawUtf8 = #13#10);
    /// return the count of stored RawUtf8
    // - reading this property is not thread-safe, since size may change
    property Count: PtrInt
      read GetCount;
    /// set or retrieve the current memory capacity of the RawUtf8 list
    // - reading this property is not thread-safe, since size may change
    property Capacity: PtrInt
      read GetCapacity write SetCapacity;
    /// set if IndexOf() shall be case sensitive or not
    // - default is TRUE
    // - matches fCaseSensitive in Flags
    property CaseSensitive: boolean
      read GetCaseSensitive write SetCaseSensitive;
    /// set if the list doesn't allow duplicated UTF-8 text
    // - if true, an internal hash table is maintained for faster IndexOf()
    // - matches fNoDuplicate in Flags
    property NoDuplicate: boolean
      read GetNoDuplicate;
    /// access to the low-level flags of this list
    property Flags: TRawUtf8ListFlags
      read fFlags write fFlags;
    /// get or set a RawUtf8 item
    // - returns '' and raise no exception in case of out of range supplied index
    // - if you want to use it with the UI, use Utf8ToString() function
    // - reading this property is not thread-safe, since content may change
    property Strings[Index: PtrInt]: RawUtf8
      read Get write Put; default;
    /// get or set an item as RTL string, ready to be used with the UI
    // - returns '' and raise no exception in case of out of range supplied index
    // - wrap Strings[] with Utf8ToString/StringToUtf8 functions
    // - reading this property is not thread-safe, since content may change
    property Str[Index: PtrInt]: string
      read GetS write PutS;
    /// get or set a Object item
    // - returns nil and raise no exception in case of out of range supplied index
    // - reading this property is not thread-safe, since content may change
    property Objects[Index: PtrInt]: pointer
      read GetObject write PutObject;
    /// retrieve the corresponding Name when stored as 'Name=Value' pairs
    // - reading this property is not thread-safe, since content may change
    // - consider TSynNameValue if you expect more efficient name/value process
    property Names[Index: PtrInt]: RawUtf8
      read GetName;
    /// access to the corresponding 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    // - reading this property is thread-safe, but won't use the hash table
    // - consider TSynNameValue if you expect more efficient name/value process
    property Values[const Name: RawUtf8]: RawUtf8
      read GetValue write SetValue;
    /// the char separator between 'Name=Value' pairs
    // - equals '=' by default
    // - consider TSynNameValue if you expect more efficient name/value process
    property NameValueSep: AnsiChar
      read fNameValueSep write fNameValueSep;
    /// set or retrieve all items as text lines
    // - lines are separated by #13#10 (CRLF) by default; use GetText and
    // SetText methods if you want to use another line delimiter (even a comma)
    // - this property is thread-safe
    property Text: RawUtf8
      read GetTextCRLF write SetTextCRLF;
    /// Event triggered when an entry is modified
    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;
    /// direct access to the memory of the TRawUtf8DynArray items
    // - reading this property is not thread-safe, since content may change
    property TextPtr: PPUtf8CharArray
      read GetTextPtr;
    /// direct access to the memory of the TObjectDynArray items
    // - reading this property is not thread-safe, since content may change
    property ObjectPtr: PPointerArray
      read GetObjectPtr;
    /// direct access to the TRawUtf8DynArray instance
    // - reading this property is not thread-safe, since content may change
    property ValuePtr: TRawUtf8DynArray
      read fValue;
    /// direct access to the TRawUtf8DynArray items dynamic array wrapper
    // - using this property is not thread-safe, since content may change
    property ValuesArray: TDynArrayHashed
      read fValues;
  end;

  PRawUtf8List = ^TRawUtf8List;

{$ifndef PUREMORMOT2}

  // some declarations used for backward compatibility only
  TRawUtf8ListLocked = class(TRawUtf8List)
    protected procedure SetDefaultFlags; override; end;
  TRawUtf8ListHashed = class(TRawUtf8List)
    protected procedure SetDefaultFlags; override; end;
  TRawUtf8ListHashedLocked = class(TRawUtf8ListHashed)
    protected procedure SetDefaultFlags; override; end;
  // deprecated TRawUtf8MethodList should be replaced by a TSynDictionary

{$endif PUREMORMOT2}

/// sort a dynamic array of PUtf8Char items, via an external array of indexes
// - you can use FastFindIndexedPUtf8Char() for fast O(log(n)) binary search
procedure QuickSortIndexedPUtf8Char(Values: PPUtf8CharArray; Count: integer;
  var SortedIndexes: TCardinalDynArray; CaseSensitive: boolean = false);

var
  /// low-level JSON unserialization function
  // - defined in this unit to avoid circular reference with mormot.core.json,
  // but to publish the TDynArray.LoadFromJson overloaded methods
  // - this unit will just set a wrapper raising an ERttiException
  // - link mormot.core.json.pas to have a working implementation
  // - rather call LoadJson() from mormot.core.json than this low-level function
  GetDataFromJson: procedure(Data: pointer; var Json: PUtf8Char;
    EndOfObject: PUtf8Char; Rtti: TRttiCustom;
    CustomVariantOptions: PDocVariantOptions; Tolerant: boolean;
    Interning: TRawUtf8InterningAbstract);


{ ************ Abstract Radix Tree Classes }

type
  TRadixTree = class;

  /// refine the TRadixTreeNode content
  // - rtfParam is <param> node, i.e. a TRadixTreeNodeParams with Names <> nil
  // - rtfParamInteger is for a rtfParam which value should be only an integer,
  // either from rtoIntegerParams global flag, or individually as <int:###>
  // - rtfParamPath is for a rtfParam which value should be the whole path,
  // until the end of the URI or the beginning of the parameters (i.e. at '?'),
  // set individually as <path:###> parameter - * being synonymous to <path:path>
  TRadixTreeNodeFlags = set of (
    rtfParam,
    rtfParamInteger,
    rtfParamPath);

  /// implement an abstract Radix Tree node
  TRadixTreeNode = class
  protected
    function ComputeDepth: integer;
    procedure SortChildren;
  public
    /// the main Tree holding this node
    Owner: TRadixTree;
    /// the characters to be compared at this level
    Chars: RawUtf8;
    /// how many branches are within this node - used to sort by priority
    Depth: integer;
    /// describe the content of this node
    Flags: TRadixTreeNodeFlags;
    /// the nested nodes
    Child: array of TRadixTreeNode;
    /// the whole text up to this level
    FullText: RawUtf8;
    /// initialize this node instance
    constructor Create(aOwner: TRadixTree); reintroduce;
    /// instantiate a new node with the same class and properties
    function Split(const Text: RawUtf8): TRadixTreeNode; virtual;
    /// finalize this Radix Tree node
    destructor Destroy; override;
    /// search for the node corresponding to a given text
    function Find(P: PUtf8Char): TRadixTreeNode;
    /// internal debugging/testing method
    procedure ToText(var Result: RawUtf8; Level: integer);
  end;

  /// our TRadixTree works on dynamic/custom types of node classes
  TRadixTreeNodeClass = class of TRadixTreeNode;

  /// allow to customize TRadixTree process
  // - e.g. if static text matching should be case-insensitive (but <params> are
  // always case-sensitive, because they are user-specific runtime variables)
  // - if <param> values should be only plain integers, never alphabetical text -
  // you may also specify int:xxx for a single parameter, e.g. as <int:id>
  TRadixTreeOptions = set of (
    rtoCaseInsensitiveUri,
    rtoIntegerParams);

  /// implement an abstract Radix Tree over UTF-8 case-insensitive text
  // - as such, this class is not very useful if you just need to lookup for
  // a text value: a TDynArrayHasher/TDictionary is faster and uses less RAM
  // - but, once extended e.g. as TUriTree, it can very efficiently parse
  // some text with variants parts (e.g. parameters)
  TRadixTree = class
  protected
    fRoot: TRadixTreeNode;
    fDefaultNodeClass: TRadixTreeNodeClass;
    fOptions: TRadixTreeOptions;
    fNormTable: PNormTable; // for efficient rtoCaseInsensitiveUri
  public
    /// initialize the Radix Tree
    constructor Create(aNodeClass: TRadixTreeNodeClass;
      aOptions: TRadixTreeOptions = []); reintroduce;
    /// finalize this Radix Tree
    destructor Destroy; override;
    /// define how TRadixTreeNode.Lookup() will process this node
    // - as set with this class constructor
    property Options: TRadixTreeOptions
      read fOptions;
    /// finalize this Radix Tree node
    procedure Clear;
    /// low-level insertion of a given Text entry as a given child
    // - may return an existing node instance, if Text was already inserted
    function Insert(Text: RawUtf8; Node: TRadixTreeNode = nil;
      NodeClass: TRadixTreeNodeClass = nil): TRadixTreeNode;
    /// to be called after Insert() to consolidate the internal tree state
    // - nodes will be sorted by search priority, i.e. the longest depths first
    // - as called e.g. by TUriTree.Setup()
    procedure AfterInsert;
    /// search for the node corresponding to a given text
    // - more than 6 million lookups per second, with 1000 items stored
    function Find(const Text: RawUtf8): TRadixTreeNode;
    /// internal debugging/testing method
    function ToText: RawUtf8;
    /// low-level access to the root node of the Radix Tree
    property Root: TRadixTreeNode
      read fRoot;
  end;

  /// implement an abstract Radix Tree static or <param> node
  TRadixTreeNodeParams = class(TRadixTreeNode)
  protected
    /// is called for each <param> as Pos/Len pair
    // - called eventually with Pos^='?' and Len=-1 for the inlined parameters
    // - should return true on success, false to abort
    function LookupParam(Ctxt: TObject; Pos: PUtf8Char; Len: integer): boolean;
      virtual; abstract;
  public
    /// all the <param1> <param2> names, in order, up to this parameter
    // - equals nil for static nodes
    // - is referenced as pointer into THttpServerRequestAbstract.fRouteName
    Names: TRawUtf8DynArray;
    /// overriden to support the additional Names fields
    function Split(const Text: RawUtf8): TRadixTreeNode; override;
    /// main search method, recognizing static or <param> patterns
    function Lookup(P: PUtf8Char; Ctxt: TObject): TRadixTreeNodeParams;
  end;

  /// implement an abstract Radix Tree with static or <param> nodes
  TRadixTreeParams = class(TRadixTree)
  public
    /// low-level registration of a new URI path, with <param> support
    // - returns the node matching the given URI
    // - called e.g. from TUriRouter.Rewrite/Run methods
    // - will recognize <param> alphanumerical and <int:id> integer parameters
    function Setup(const aFromUri: RawUtf8; out aNames: TRawUtf8DynArray): TRadixTreeNodeParams;
  end;

  ERadixTree = class(ESynException);


implementation


{ ************ RTL TPersistent / TInterfacedObject with Custom Constructor }

{ TPersistentWithCustomCreate }

constructor TPersistentWithCustomCreate.Create;
begin
  // nothing to do by default - overridden constructor may add custom code
end;


{ TInterfacedObjectWithCustomCreate }

constructor TInterfacedObjectWithCustomCreate.Create;
begin
  // nothing to do by default - overridden constructor may add custom code
end;

procedure TInterfacedObjectWithCustomCreate.RefCountUpdate(Release: boolean);
begin
  if Release then
    _Release
  else
    _AddRef;
end;


{ TInterfacedCollection }

constructor TInterfacedCollection.Create;
begin
  inherited Create(GetClass);
end;


{ TSynInterfacedObject }

constructor TSynInterfacedObject.Create;
begin
  // do-nothing virtual constructor
end;

function TSynInterfacedObject._AddRef: TIntCnt;
begin
  result := VirtualAddRef;
end;

function TSynInterfacedObject._Release: TIntCnt;
begin
  result := VirtualRelease;
end;

function TSynInterfacedObject.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} IID: TGuid;
  out Obj): TIntQry;
begin
  result := VirtualQueryInterface(@IID, Obj);
end;

function TSynInterfacedObject.VirtualQueryInterface(IID: PGuid; out Obj): TIntQry;
begin
  result := E_NOINTERFACE;
end;


{ TAutoFree }

constructor TAutoFree.Create(var localVariable; obj: TObject);
begin
  fObject := obj;
  TObject(localVariable) := obj;
end;

constructor TAutoFree.Create(const varObjPairs: array of pointer);
var
  n, i: PtrInt;
begin
  n := length(varObjPairs);
  if (n = 0) or
     (n and 1 = 1) then
    exit;
  n := n shr 1;
  if n = 0 then
    exit;
  if n = 1 then
  begin
    fObject := varObjPairs[1];
    PPointer(varObjPairs[0])^ := fObject;
    exit;
  end;
  SetLength(fObjectList, n);
  for i := 0 to n - 1 do
  begin
    fObjectList[i] := varObjPairs[i * 2 + 1];
    PPointer(varObjPairs[i * 2])^ := fObjectList[i];
  end;
end;

procedure TAutoFree.ForMethod;
begin
  // do-nothing method to circumvent the Delphi 10.4 IAutoFree early release
end;

class function TAutoFree.One(var localVariable; obj: TObject): IAutoFree;
begin
  result := Create(localVariable,obj);
  {$ifdef ISDELPHI104}
  result.ForMethod;
  {$endif ISDELPHI104}
end;

class function TAutoFree.Several(const varObjPairs: array of pointer): IAutoFree;
begin
  result := Create(varObjPairs);
  // inlining is not possible on Delphi -> Delphi 10.4 caller should run ForMethod :(
end;

procedure TAutoFree.Another(var localVariable; obj: TObject);
var
  n: PtrInt;
begin
  n := length(fObjectList);
  SetLength(fObjectList, n + 1);
  fObjectList[n] := obj;
  TObject(localVariable) := obj;
end;

destructor TAutoFree.Destroy;
var
  i: PtrInt;
begin
  if fObjectList <> nil then
    for i := length(fObjectList) - 1 downto 0 do // release FILO
      fObjectList[i].Free;
  fObject.Free;
  inherited;
end;


{ TAutoLocker }

constructor TAutoLocker.Create;
begin
  fSafe.Init;
end;

destructor TAutoLocker.Destroy;
begin
  fSafe.Done;
  inherited Destroy;
end;

function TAutoLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@fSafe);
end;

procedure TAutoLocker.Enter;
begin
  fSafe.Lock;
end;

procedure TAutoLocker.Leave;
begin
  fSafe.UnLock;
end;

function TAutoLocker.Safe: PSynLocker;
begin
  result := @fSafe;
end;


{ ************ TSynPersistent* / TSyn*List / TSynLocker classes }

{ TSynPersistent }

procedure TSynPersistent.AssignError(Source: TSynPersistent);
begin
  raise EConvertError.CreateFmt('Cannot assign a %s to a %s',
    [ClassNameShort(Source)^, ClassNameShort(self)^]);
end;

procedure TSynPersistent.AssignTo(Dest: TSynPersistent);
begin
  Dest.AssignError(Self);
end;

procedure TSynPersistent.Assign(Source: TSynPersistent);
begin
  if Source <> nil then
    Source.AssignTo(Self)
  else
    AssignError(nil);
end;

{$ifdef HASITERATORS}

{ TPointerEnumerator }

procedure TPointerEnumerator.Init(Values: PPointerArray; Count: PtrUInt);
begin
  if Count = 0 then
  begin
    Curr := nil;
    After := nil;
  end
  else
  begin
    Curr := pointer(Values);
    After := @Values[Count];
    dec(Curr);
  end;
end;

function TPointerEnumerator.MoveNext: Boolean;
begin
  inc(Curr);
  result := PtrUInt(Curr) < PtrUInt(After);
end;

function TPointerEnumerator.GetCurrent: pointer;
begin
  result := Curr^;
end;

function TPointerEnumerator.GetEnumerator: TPointerEnumerator;
begin
  result := self;
end;

{$endif HASITERATORS}

{ TSynList }

constructor TSynList.Create;
begin
  // nothing to do
end;

function TSynList.Add(item: pointer): PtrInt;
begin
  // inlined result := ObjArrayAddCount(fList, item, fCount);
  result := fCount;
  if result = length(fList) then
    SetLength(fList, NextGrow(result));
  fList[result] := item;
  inc(fCount);
end;

function TSynList.Insert(item: pointer; index: PtrInt): PtrInt;
begin
  result := PtrArrayInsert(fList, item, index, fCount);
end;

procedure TSynList.Clear;
begin
  fList := nil;
  fCount := 0;
end;

procedure TSynList.Delete(index: integer; dontfree: boolean);
begin
  PtrArrayDelete(fList, index, @fCount);
  if (fCount > 64) and
     (length(fList) > fCount * 2) then
    SetLength(fList, fCount); // reduce capacity when half list is void
end;

function TSynList.Exists(item: pointer): boolean;
begin
  result := IndexOf(item) >= 0;
end;

function TSynList.Get(index: integer): pointer;
begin
  if cardinal(index) < cardinal(fCount) then
    result := fList[index]
  else
    result := nil;
end;

function TSynList.IndexOf(item: pointer): PtrInt;
begin
  result := PtrUIntScanIndex(pointer(fList), fCount, PtrUInt(item));
end;

function TSynList.Remove(item: pointer): PtrInt;
begin
  result := IndexOf(item);
  if result >= 0 then
    Delete(result);
end;

{$ifdef HASITERATORS}

function TSynList.GetEnumerator: TPointerEnumerator;
begin
  result.Init(pointer(fList), fCount);
end;

{$endif HASITERATORS}


{ TSynObjectList }

constructor TSynObjectList.Create(aOwnObjects: boolean; aItemClass: TClass);
begin
  fOwnObjects := aOwnObjects;
  fItemClass := aItemClass;
  inherited Create;
end;

procedure TSynObjectList.Delete(index: integer; dontfree: boolean);
begin
  if cardinal(index) >= cardinal(fCount) then
    exit;
  if fOwnObjects and
     not dontfree then
    TObject(fList[index]).Free;
  inherited Delete(index);
end;

procedure TSynObjectList.Clear;
begin
  if fOwnObjects then
    RawObjectsClear(pointer(fList), fCount);
  inherited Clear;
end;

procedure TSynObjectList.ClearFromLast;
var
  i: PtrInt;
begin
  if fOwnObjects then
    for i := fCount - 1 downto 0 do // call Free in reverse order
      FreeAndNilSafe(fList[i]);     // safer
  inherited Clear;
end;

destructor TSynObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSynObjectList.NewItem: pointer;
begin
  result := nil;
  if fItemClass = nil then
    exit;
  result := Rtti.RegisterClass(fItemClass).ClassNewInstance;
  Add(result);
end;


{ TSynPersistentLock }

constructor TSynPersistentLock.Create;
begin
  inherited Create; // may have been overriden
  fSafe := NewSynLocker;
end;

destructor TSynPersistentLock.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;

procedure TSynPersistentLock.Lock;
begin
  if self <> nil then
    fSafe^.Lock;
end;

procedure TSynPersistentLock.Unlock;
begin
  if self <> nil then
    fSafe^.UnLock;
end;

class procedure TSynPersistentLock.RttiCustomSetParser(Rtti: TRttiCustom);
begin
  // let's call our overriden RttiBeforeWriteObject and RttiAfterWriteObject
  Rtti.Flags := Rtti.Flags + [rcfHookWrite];
end;

function TSynPersistentLock.RttiBeforeWriteObject(W: TTextWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  if woPersistentLock in Options then
    fSafe.Lock;
  result := false; // continue with default JSON serialization
end;

procedure TSynPersistentLock.RttiAfterWriteObject(W: TTextWriter;
  Options: TTextWriterWriteObjectOptions);
begin
  if woPersistentLock in Options then
    fSafe.UnLock;
end;

{ TInterfacedObjectLocked }

constructor TInterfacedObjectLocked.Create;
begin
  inherited Create;
  fSafe := NewSynLocker;
end;

destructor TInterfacedObjectLocked.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;


{ TSynObjectListLocked }

function TSynObjectListLocked.Add(item: pointer): PtrInt;
begin
  Safe.WriteLock;
  try
    result := inherited Add(item);
  finally
    Safe.WriteUnLock;
  end;
end;

function TSynObjectListLocked.Remove(item: pointer): PtrInt;
begin
  Safe.WriteLock;
  try
    result := inherited Remove(item);
  finally
    Safe.WriteUnLock;
  end;
end;

function TSynObjectListLocked.Exists(item: pointer): boolean;
begin
  Safe.ReadOnlyLock;
  try
    result := inherited Exists(item);
  finally
    Safe.ReadOnlyUnLock;
  end;
end;

procedure TSynObjectListLocked.Clear;
begin
  Safe.WriteLock;
  try
    inherited Clear;
  finally
    Safe.WriteUnLock;
  end;
end;

procedure TSynObjectListLocked.ClearFromLast;
begin
  Safe.WriteLock;
  try
    inherited ClearFromLast;
  finally
    Safe.WriteUnLock;
  end;
end;


{ TSynObjectListSorted }

constructor TSynObjectListSorted.Create(const aCompare: TOnObjectCompare;
  aOwnsObjects: boolean);
begin
  inherited Create(aOwnsObjects);
  fCompare := aCompare;
end;

function TSynObjectListSorted.Locate(item: pointer; out index: PtrInt): boolean;
var
  n, l, i: PtrInt;
  cmp: integer;
begin // see TDynArray.FastLocateSorted below
  result := false;
  n := fCount;
  if n = 0 then // a void array is always sorted
    index := 0
  else
  begin
    dec(n);
    cmp := fCompare(fList[n], item);
    if cmp <= 0 then
    begin
      // greater than last sorted item (may be a common case)
      if cmp = 0 then
        // returns true + index of existing item
        result := true
      else
        // returns false + insert after last position
        inc(n);
      index := n;
      exit;
    end;
    l := 0;
    repeat
      // O(log(n)) binary search of the sorted position
      i := (l + n) shr 1;
      cmp := fCompare(fList[i], item);
      if cmp = 0 then
      begin
        // returns true + index of existing item
        index := i;
        result := true;
        exit;
      end
      else if cmp < 0 then
        l := i + 1
      else
        n := i - 1;
    until l > n;
    // item not found: returns false + the index where to insert
    index := l;
  end;
end;

function TSynObjectListSorted.Add(item: pointer): PtrInt;
begin
  Safe.WriteLock;
  try
    if Locate(item, result) then // O(log(n)) binary search
      result := -(result + 1)
    else
      Insert(item, result);
  finally
    Safe.WriteUnLock;
  end;
end;

function TSynObjectListSorted.IndexOf(item: pointer): PtrInt;
begin
  if not Locate(item, result) then // O(log(n)) binary search
    result := -1;
end;

function TSynObjectListSorted.Find(item: TObject): TObject;
var
  i: PtrInt;
begin
  if Locate(item, i) then
    result := fList[i]
  else
    result := nil;
end;


{ ************ TSynPersistentStore with proper Binary Serialization }

{ TSynPersistentStore }

constructor TSynPersistentStore.Create(const aName: RawUtf8);
begin
  inherited Create; // may have been overriden
  fName := aName;
end;

constructor TSynPersistentStore.CreateFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  CreateFromBuffer(pointer(aBuffer), length(aBuffer), aLoad);
end;

constructor TSynPersistentStore.CreateFromBuffer(
  aBuffer: pointer; aBufferLen: integer; aLoad: TAlgoCompressLoad);
begin
  inherited Create; // may have been overriden
  LoadFrom(aBuffer, aBufferLen, aLoad);
end;

constructor TSynPersistentStore.CreateFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad);
begin
  inherited Create; // may have been overriden
  LoadFromFile(aFileName, aLoad);
end;

procedure TSynPersistentStore.LoadFromReader;
begin
  fReader.VarUtf8(fName);
end;

procedure TSynPersistentStore.SaveToWriter(aWriter: TBufferWriter);
begin
  aWriter.Write(fName);
end;

procedure TSynPersistentStore.LoadFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  if aBuffer <> '' then
    LoadFrom(pointer(aBuffer), length(aBuffer), aLoad);
end;

procedure TSynPersistentStore.LoadFrom(aBuffer: pointer; aBufferLen: integer;
  aLoad: TAlgoCompressLoad);
var
  localtemp: RawByteString;
  p: pointer;
  temp: PRawByteString;
begin
  if (aBuffer = nil) or
     (aBufferLen <= 0) then
    exit; // nothing to load
  fLoadFromLastAlgo := TAlgoCompress.Algo(aBuffer, aBufferLen);
  if fLoadFromLastAlgo = nil then
    fReader.ErrorData('%.LoadFrom unknown TAlgoCompress AlgoID=%',
      [self, PByteArray(aBuffer)[4]]);
  temp := fReaderTemp;
  if temp = nil then
    temp := @localtemp;
  p := fLoadFromLastAlgo.Decompress(aBuffer, aBufferLen,
    fLoadFromLastUncompressed, temp^, aLoad);
  if p = nil then
    fReader.ErrorData('%.LoadFrom %.Decompress failed',
      [self, fLoadFromLastAlgo]);
  fReader.Init(p, fLoadFromLastUncompressed);
  LoadFromReader;
end;

function TSynPersistentStore.LoadFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad): boolean;
var
  temp: RawByteString;
begin
  temp := StringFromFile(aFileName);
  result := temp <> '';
  if result then
    LoadFrom(temp, aLoad);
end;

procedure TSynPersistentStore.SaveTo(out aBuffer: RawByteString;
  nocompression: boolean; BufLen: integer; ForcedAlgo: TAlgoCompress;
  BufferOffset: integer);
var
  writer: TBufferWriter;
  temp: array[word] of byte;
begin
  if BufLen <= SizeOf(temp) then
    writer := TBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
  else
    writer := TBufferWriter.Create(TRawByteStringStream, BufLen);
  try
    SaveToWriter(writer);
    fSaveToLastUncompressed := writer.TotalWritten;
    aBuffer := writer.FlushAndCompress(nocompression, ForcedAlgo, BufferOffset);
  finally
    writer.Free;
  end;
end;

function TSynPersistentStore.SaveTo(nocompression: boolean; BufLen: integer;
  ForcedAlgo: TAlgoCompress; BufferOffset: integer): RawByteString;
begin
  SaveTo(result, nocompression, BufLen, ForcedAlgo, BufferOffset);
end;

function TSynPersistentStore.SaveToFile(const aFileName: TFileName;
  nocompression: boolean; BufLen: integer; ForcedAlgo: TAlgoCompress): PtrUInt;
var
  temp: RawByteString;
begin
  SaveTo(temp, nocompression, BufLen, ForcedAlgo);
  if FileFromString(temp, aFileName) then
    result := length(temp)
  else
    result := 0;
end;




{ ************ INI Files and In-memory Access }

function IdemPChar2(table: PNormTable; p: PUtf8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  u: AnsiChar;
begin
  // here p and up are expected to be <> nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    u := up^;
    if u = #0 then
      break;
    if table^[up[PtrUInt(p)]] <> u then
      exit;
    inc(up);
  until false;
  result := true;
end;

function FindSectionFirstLine(var source: PUtf8Char; search: PAnsiChar): boolean;
var
  table: PNormTable;
  charset: PTextCharSet;
begin
  result := false;
  if (source = nil) or
     (search = nil) then
    exit;
  table := @NormToUpperAnsi7;
  charset := @TEXT_CHARS;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPChar2(table, source, search);
    end;
    while tcNot01013 in charset[source^] do
      inc(source);
    while tc1013 in charset[source^] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindSectionFirstLineW(var source: PWideChar; search: PUtf8Char): boolean;
begin
  result := false;
  if source = nil then
    exit;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPCharW(source, search);
    end;
    while not (cardinal(source^) in [0, 10, 13]) do
      inc(source);
    while cardinal(source^) in [10, 13] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindIniNameValue(P: PUtf8Char; UpperName: PAnsiChar;
  const DefaultValue: RawUtf8): RawUtf8;
var
  u, PBeg: PUtf8Char;
  by4: cardinal;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif CPUX86NOTPIC}
begin
  // expect UpperName as 'NAME='
  if (P <> nil) and
     (P^ <> '[') and
     (UpperName <> nil) then
  begin
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7;
    {$endif CPUX86NOTPIC}
    PBeg := nil;
    u := P;
    repeat
      while u^ = ' ' do
        inc(u); // trim left ' '
      if u^ = #0 then
        break;
      if table[u^] = UpperName[0] then
        PBeg := u;
      repeat
        by4 := PCardinal(u)^;
        if ToByte(by4) > 13 then
          if ToByte(by4 shr 8) > 13 then
            if ToByte(by4 shr 16) > 13 then
              if ToByte(by4 shr 24) > 13 then
              begin
                inc(u, 4);
                continue;
              end
              else
                inc(u, 3)
            else
              inc(u, 2)
          else
            inc(u);
        if u^ in [#0, #10, #13] then
          break;
        inc(u);
      until false;
      if PBeg <> nil then
      begin
        inc(PBeg);
        P := u;
        u := pointer(UpperName + 1);
        repeat
          if u^ <> #0 then
            if table[PBeg^] <> u^ then
              break
            else
            begin
              inc(u);
              inc(PBeg);
            end
          else
          begin
            FastSetString(result, PBeg, P - PBeg);
            exit;
          end;
        until false;
        PBeg := nil;
        u := P;
      end;
      if u^ = #13 then
        inc(u);
      if u^ = #10 then
        inc(u);
    until u^ in [#0, '['];
  end;
  result := DefaultValue;
end;

function ExistsIniName(P: PUtf8Char; UpperName: PAnsiChar): boolean;
var
  table: PNormTable;
begin
  result := false;
  if (P <> nil) and
     (P^ <> '[') then
  begin
    table := @NormToUpperAnsi7;
    repeat
      if P^ = ' ' then
      begin
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
        if P^ = #0 then
          break;
      end;
      if IdemPChar2(table, P, UpperName) then
      begin
        result := true;
        exit;
      end;
      repeat
        if P[0] > #13 then
          if P[1] > #13 then
            if P[2] > #13 then
              if P[3] > #13 then
              begin
                inc(P, 4);
                continue;
              end
              else
                inc(P, 3)
            else
              inc(P, 2)
          else
            inc(P);
        case P^ of
          #0:
            exit;
          #10:
            begin
              inc(P);
              break;
            end;
          #13:
            begin
              if P[1] = #10 then
                inc(P, 2)
              else
                inc(P);
              break;
            end;
        else
          inc(P);
        end;
      until false;
    until P^ = '[';
  end;
end;

function ExistsIniNameValue(P: PUtf8Char; const UpperName: RawUtf8;
  UpperValues: PPAnsiChar): boolean;
var
  table: PNormTable;
begin
  if (UpperValues <> nil) and
     (UpperValues^ <> nil) and
     (UpperName <> '') then
  begin
    result := true;
    table := @NormToUpperAnsi7;
    while (P <> nil) and
          (P^ <> '[') do
    begin
      if P^ = ' ' then
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
      if IdemPChar2(table, P, pointer(UpperName)) then
      begin
        inc(P, length(UpperName));
        if IdemPPChar(P, UpperValues) >= 0 then
          exit; // found one value
        break;
      end;
      P := GotoNextLine(P);
    end;
  end;
  result := false;
end;

function GetSectionContent(SectionFirstLine: PUtf8Char): RawUtf8;
var
  PBeg: PUtf8Char;
begin
  PBeg := SectionFirstLine;
  while (SectionFirstLine <> nil) and
        (SectionFirstLine^ <> '[') do
    SectionFirstLine := GotoNextLine(SectionFirstLine);
  if SectionFirstLine = nil then
    result := PBeg
  else
    FastSetString(result, PBeg, SectionFirstLine - PBeg);
end;

function GetSectionContent(const Content, SectionName: RawUtf8): RawUtf8;
var
  P: PUtf8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := GetSectionContent(P)
  else
    result := '';
end;

function DeleteSection(var Content: RawUtf8; const SectionName: RawUtf8;
  EraseSectionHeader: boolean): boolean;
var
  P: PUtf8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  result := false; // no modification
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := DeleteSection(P, Content, EraseSectionHeader);
end;

function DeleteSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  EraseSectionHeader: boolean): boolean;
var
  PEnd: PUtf8Char;
  IndexBegin: PtrInt;
begin
  result := false;
  PEnd := SectionFirstLine;
  if EraseSectionHeader then // erase [Section] header line
    while (PtrUInt(SectionFirstLine) > PtrUInt(Content)) and
          (SectionFirstLine^ <> '[') do
      dec(SectionFirstLine);
  while (PEnd <> nil) and
        (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if IndexBegin = 0 then
    exit; // no modification
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  result := true; // Content was modified
end;

procedure ReplaceSection(SectionFirstLine: PUtf8Char; var Content: RawUtf8;
  const NewSectionContent: RawUtf8);
var
  PEnd: PUtf8Char;
  IndexBegin: PtrInt;
begin
  if SectionFirstLine = nil then
    exit;
  // delete existing [Section] content
  PEnd := SectionFirstLine;
  while (PEnd <> nil) and
        (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  // insert section content
  insert(NewSectionContent, Content, IndexBegin + 1);
end;

procedure ReplaceSection(var Content: RawUtf8; const SectionName, NewSectionContent: RawUtf8);
var
  UpperSection: array[byte] of AnsiChar;
  P: PUtf8Char;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    ReplaceSection(P, Content, NewSectionContent)
  else
    Content := Content + '[' + SectionName + ']'#13#10 + NewSectionContent;
end;

function FindIniNameValueInteger(P: PUtf8Char; const UpperName: RawUtf8): PtrInt;
var
  table: PNormTable;
begin
  result := 0;
  if (P = nil) or
     (UpperName = '') then
    exit;
  table := @NormToUpperAnsi7;
  repeat
    if IdemPChar2(table, P, pointer(UpperName)) then
      break;
    P := GotoNextLine(P);
    if P = nil then
      exit;
  until false;
  result := GetInteger(P + length(UpperName));
end;

function FindIniEntry(const Content, Section, Name, DefaultValue: RawUtf8): RawUtf8;
var
  P: PUtf8Char;
  UpperSection, UpperName: array[byte] of AnsiChar;
begin
  result := DefaultValue;
  P := pointer(Content);
  if P = nil then
    exit;
  // fast UpperName := UpperCase(Name)+'='
  PWord(UpperCopy255(UpperName{%H-}, Name))^ := ord('=');
  if Section = '' then
    // find the Name= entry before any [Section]
    result := FindIniNameValue(P, UpperName, DefaultValue)
  else
  begin
    // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(UpperSection{%H-}, Section))^ := ord(']');
    if FindSectionFirstLine(P, UpperSection) then
      result := FindIniNameValue(P, UpperName, DefaultValue);
  end;
end;

function FindWinAnsiIniEntry(const Content, Section, Name: RawUtf8): RawUtf8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryInteger(const Content, Section, Name: RawUtf8): integer;
begin
  result := GetInteger(pointer(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryFile(const FileName: TFileName;
  const Section, Name, DefaultValue: RawUtf8): RawUtf8;
var
  Content: RawUtf8;
begin
  Content := StringFromFile(FileName);
  if Content = '' then
    result := DefaultValue
  else
    result := FindIniEntry(Content, Section, Name, DefaultValue);
end;

function UpdateIniNameValueInternal(var Content: RawUtf8;
  const NewValue, NewValueCRLF: RawUtf8;
  var P: PUtf8Char; UpperName: PAnsiChar; UpperNameLength: integer): boolean;
var
  PBeg: PUtf8Char;
  i: integer;
begin
  if UpperName <> nil then
    while (P <> nil) and
          (P^ <> '[') do
    begin
      while P^ = ' ' do
        inc(P);   // trim left ' '
      PBeg := P;
      P := GotoNextLine(P);
      if IdemPChar2(@NormToUpperAnsi7, PBeg, UpperName) then
      begin
       // update Name=Value entry
        result := true;
        inc(PBeg, UpperNameLength);
        i := (PBeg - pointer(Content)) + 1;
        if (i = length(NewValue)) and
           CompareMem(PBeg, pointer(NewValue), i) then
          exit; // new Value is identical to the old one -> no change
        if P = nil then // avoid last line (P-PBeg) calculation error
          SetLength(Content, i - 1)
        else
          delete(Content, i, P - PBeg); // delete old Value
        insert(NewValueCRLF, Content, i); // set new value
        exit;
      end;
    end;
  result := false;
end;

function UpdateIniNameValue(var Content: RawUtf8;
  const Name, UpperName, NewValue: RawUtf8): boolean;
var
  P: PUtf8Char;
begin
  if UpperName = '' then
    result := false
  else
  begin
    P := pointer(Content);
    result := UpdateIniNameValueInternal(Content, NewValue, NewValue + #13#10,
      P, pointer(UpperName), length(UpperName));
    if result or
       (Name = '') then
      exit;
    AppendLine(Content, [Name, NewValue]);
    result := true;
  end;
end;

procedure UpdateIniEntry(var Content: RawUtf8;
  const Section, Name, Value: RawUtf8);
const
  CRLF = #13#10;
var
  P: PUtf8Char;
  SectionFound: boolean;
  i, UpperNameLength: PtrInt;
  V: RawUtf8;
  UpperSection, UpperName: array[byte] of AnsiChar;
begin
  UpperNameLength := length(Name);
  PWord(UpperCopy255Buf(
    UpperName{%H-}, pointer(Name), UpperNameLength))^ := ord('=');
  inc(UpperNameLength);
  V := Value + CRLF;
  P := pointer(Content);
  // 1. find Section, and try update within it
  if Section = '' then
    SectionFound := true // find the Name= entry before any [Section]
  else
  begin
    PWord(UpperCopy255(UpperSection{%H-}, Section))^ := ord(']');
    SectionFound := FindSectionFirstLine(P, UpperSection);
  end;
  if SectionFound and
     UpdateIniNameValueInternal(
       Content, Value, V, P, @UpperName, UpperNameLength) then
      exit;
  // 2. section or Name= entry not found: add Name=Value
  V := Name + '=' + V;
  if not SectionFound then
    // create not existing [Section]
    V := '[' + Section + (']' + CRLF) + V;
  // insert Name=Value at P^ (end of file or end of [Section])
  if P = nil then
    // insert at end of file
    Content := Content + V
  else
  begin
    // insert at end of [Section]
    i := (P - pointer(Content)) + 1;
    insert(V, Content, i);
  end;
end;

procedure UpdateIniEntryFile(const FileName: TFileName;
  const Section, Name, Value: RawUtf8);
var
  Content: RawUtf8;
begin
  Content := StringFromFile(FileName);
  UpdateIniEntry(Content, Section, Name, Value);
  FileFromString(Content, FileName);
end;

function IsHtmlContentTypeTextual(Headers: PUtf8Char): boolean;
begin
  result := ExistsIniNameValue(Headers, HEADER_CONTENT_TYPE_UPPER, @CONTENT_TYPE_TEXTUAL);
end;

const
  WS_UPGRADE: array[0..2] of PAnsiChar = (
    'UPGRADE',
    'KEEP-ALIVE, UPGRADE',
    nil);

function IsWebSocketUpgrade(headers: PUtf8Char): boolean;
begin
  result := ExistsIniNameValue(pointer(headers), 'CONNECTION: ', @WS_UPGRADE);
end;

function IniToObject(const Ini: RawUtf8; Instance: TObject;
  const SectionName: RawUtf8; DocVariantOptions: PDocVariantOptions;
  Level: integer): boolean;
var
  r: TRttiCustom;
  i: integer;
  p: PRttiCustomProp;
  section, nested, json: PUtf8Char;
  name: PAnsiChar;
  n, v: RawUtf8;
  up: array[byte] of AnsiChar;
begin
  result := false; // true when at least one property has been read
  if (Ini = '') or
     (Instance = nil) then
    exit;
  PWord(UpperCopy255(up{%H-}, SectionName))^ := ord(']');
  section := pointer(Ini);
  if not FindSectionFirstLine(section, @up) then
    exit; // section not found
  r := Rtti.RegisterClass(Instance);
  p := pointer(r.Props.List);
  for i := 1 to r.Props.Count do
  begin
    if p^.Prop <> nil then
      if p^.Value.Kind = rkClass then
      begin
        // recursive load from another per-property section
        if Level = 0 then
          n := p^.Name
        else
          n := SectionName + '.' + p^.Name;
        if IniToObject(Ini, p^.Prop^.GetObjProp(Instance), n,
              DocVariantOptions, Level + 1) then
          result := true;
      end
      else
      begin
        PWord(UpperCopy255(up{%H-}, p^.Name))^ := ord('=');
        v := FindIniNameValue(section, @up, #0);
        if p^.Value.Parser in ptMultiLineStringTypes then
        begin
          if v = #0 then // may be stored in a multi-line section body
          begin
            name := @up;
            if Level <> 0 then
            begin
              name := UpperCopy255(name, SectionName);
              name^ := '.';
              inc(name);
            end;
            PWord(UpperCopy255(name, p^.Name))^ := ord(']');
            nested := pointer(Ini);
            if FindSectionFirstLine(nested, @up) then
            begin
              // multi-line text value has been stored in its own section
              v := GetSectionContent(nested);
              if p^.Prop^.SetValueText(Instance, v) then
                result := true;
            end;
          end
          else if p^.Prop^.SetValueText(Instance, v) then // single line text
            result := true;
        end
        else if v <> #0 then
          if (p^.OffsetSet <= 0) or // has a setter?
             (rcfBoolean in p^.Value.Cache.Flags) or // simple value?
             (p^.Value.Kind in (rkGetIntegerPropTypes + [rkEnumeration, rkFloat])) then
          begin
            if p^.Prop^.SetValueText(Instance, v) then // RTTI conversion
              result := true;
          end
          else // e.g. rkVariant, rkSet, rkDynArray
          begin
            json := pointer(v); // convert complex values from JSON
            GetDataFromJson(@PByteArray(Instance)[p^.OffsetSet], json,
              nil, p^.Value, DocVariantOptions, true, nil);
            if json <> nil then
              result := true;
          end;
      end;
    inc(p);
  end;
end;

function TrimAndIsMultiLine(var U: RawUtf8): boolean;
var
  L: PtrInt;
  P: PUtf8Char absolute U;
begin
  result := false;
  L := length(U);
  if L = 0 then
    exit;
  while P[L - 1] in [#13, #10] do
  begin
    dec(L);
    if L = 0 then
    begin
      U := ''; // no meaningful text
      exit;
    end;
  end;
  if L <> length(U) then
    SetLength(U, L); // trim right
  if BufferLineLength(P, P + L) = L then // may use x86_64 SSE2 asm
    exit; // no line feed
  result := true; // there are line feeds within this text
  U := TrimChar(U, [#13]); // normalize #13#10 into #10 as ObjectToIni()
end;

function ObjectToIni(const Instance: TObject; const SectionName: RawUtf8;
  Options: TTextWriterWriteObjectOptions; Level: integer): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
  nested: TRawUtf8DynArray;
  i, nestedcount: integer;
  r: TRttiCustom;
  p: PRttiCustomProp;
  n, s: RawUtf8;
begin
  result := '';
  if Instance = nil then
    exit;
  nestedcount := 0;
  W := DefaultJsonWriter.CreateOwnedStream(tmp);
  try
    W.CustomOptions := W.CustomOptions + [twoTrimLeftEnumSets];
    W.Add('[%]'#10, [SectionName]);
    r := Rtti.RegisterClass(Instance);
    p := pointer(r.Props.List);
    for i := 1 to r.Props.Count do
    begin
      if p^.Prop <> nil then
        if p^.Value.Kind = rkClass then
        begin
          if Level = 0 then
            n := p^.Name
          else
            n := SectionName + '.' + p^.Name;
          s := ObjectToIni(p^.Prop^.GetObjProp(Instance), n, Options, Level + 1);
          if s <> '' then
            AddRawUtf8(nested, nestedcount, s);
        end
        else if p^.Value.Kind = rkEnumeration then
        begin
          if woHumanReadableEnumSetAsComment in Options then
          begin
            p^.Value.Cache.EnumInfo^.GetEnumNameAll(
              s, '; values=', {quoted=}false, #10, {uncamelcase=}true);
            W.AddString(s);
          end;
          // AddValueJson() would have written "quotes"
          W.AddString(p^.Name);
          W.Add('=');
          W.AddTrimLeftLowerCase(p^.Value.Cache.EnumInfo^.GetEnumNameOrd(
            p^.Prop^.GetOrdProp(Instance)));
          W.Add(#10);
        end
        else if p^.Value.Parser in ptMultiLineStringTypes then
        begin
          p^.Prop^.GetAsString(Instance, s);
          if TrimAndIsMultiLine(s) then
          begin
            // store multi-line text values in their own section
            if Level = 0 then
              FormatUtf8('[%]'#10'%'#10#10, [p^.Name, s], n)
            else
              FormatUtf8('[%.%]'#10'%'#10#10, [SectionName, p^.Name, s], n);
            AddRawUtf8(nested, nestedcount, n);
          end
          else
          begin
            W.AddString(p^.Name);
            W.Add('=');
            W.AddString(s); // single line text
            W.Add(#10);
          end;
        end
        else
        begin
          W.AddString(p^.Name);
          W.Add('=');
          p^.AddValueJson(W, Instance, // simple and complex types
            Options - [woHumanReadableEnumSetAsComment], twOnSameLine);
          W.Add(#10);
        end;
      inc(p);
    end;
    W.Add(#10);
    for i := 0 to nestedcount - 1 do
      W.AddString(nested[i]);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************ RawUtf8 String Values Interning and TRawUtf8List }

{ TRawUtf8Hashed }

procedure TRawUtf8Hashed.Init;
begin
  Values.InitSpecific(TypeInfo(TRawUtf8DynArray), Value, ptRawUtf8,
    @Count, false, InterningHasher);
end;


{ TRawUtf8InterningSlot }

procedure TRawUtf8InterningSlot.Init;
begin
  fHash.Init;
end;

procedure TRawUtf8InterningSlot.Unique(var aResult: RawUtf8;
  const aText: RawUtf8; aTextHash: cardinal);
var
  i: PtrInt;
  added: boolean;
begin
  fSafe.ReadLock; // a TRWLightLock is faster here than an upgradable TRWLock
  i := fHash.Values.Hasher.FindOrNewComp(aTextHash, @aText);
  if i >= 0 then
  begin
    aResult := fHash.Value[i]; // return unified string instance
    fSafe.ReadUnLock;
    exit;
  end;
  fSafe.ReadUnLock;
  fSafe.WriteLock; // need to be added within the write lock
  i := fHash.Values.FindHashedForAdding(aText, added, aTextHash);
  if added then
  begin
    fHash.Value[i] := aText; // copy new value to the pool
    aResult := aText;
  end
  else
    aResult := fHash.Value[i]; // was added in a background thread
  fSafe.WriteUnLock;
end;

procedure TRawUtf8InterningSlot.UniqueFromBuffer(var aResult: RawUtf8;
  aText: PUtf8Char; aTextLen: PtrInt; aTextHash: cardinal);
var
  c: AnsiChar;
  added: boolean;
  i: PtrInt;
  bak: TDynArraySortCompare;
begin
  if not fSafe.TryReadLock then
  begin
    FastSetString(aResult, aText, aTextLen); // avoid waiting on contention
    exit;
  end;
  c := aText[aTextLen];
  aText[aTextLen] := #0; // input buffer may not be #0 terminated
  i := fHash.Values.Hasher.FindOrNewComp(aTextHash, @aText, @SortDynArrayPUtf8Char);
  if i >= 0 then
  begin
    aResult := fHash.Value[i]; // return unified string instance
    fSafe.ReadUnLock;
    aText[aTextLen] := c;
    exit;
  end;
  fSafe.ReadUnLock;
  fSafe.WriteLock; // need to be added
  bak := fHash.Values.Hasher.Compare; // (RawUtf8,RawUtf8) -> (RawUtf8,PUtf8Char)
  PDynArrayHasher(@fHash.Values.Hasher)^.fCompare := @SortDynArrayPUtf8Char;
  i := fHash.Values.FindHashedForAdding(aText, added, aTextHash);
  PDynArrayHasher(@fHash.Values.Hasher)^.fCompare := bak;
  if added then
    FastSetString(fHash.Value[i], aText, aTextLen); // new value to the pool
  aResult := fHash.Value[i];
  fSafe.WriteUnLock;
  aText[aTextLen] := c;
end;

procedure TRawUtf8InterningSlot.UniqueText(var aText: RawUtf8; aTextHash: cardinal);
var
  i: PtrInt;
  added: boolean;
begin
  fSafe.ReadLock;
  i := fHash.Values.Hasher.FindOrNewComp(aTextHash, @aText);
  if i >= 0 then
  begin
    aText := fHash.Value[i]; // return unified string instance
    fSafe.ReadUnLock;
    exit;
  end;
  fSafe.ReadUnLock;
  fSafe.WriteLock; // need to be added
  i := fHash.Values.FindHashedForAdding(aText, added, aTextHash);
  if added then
    fHash.Value[i] := aText  // copy new value to the pool
  else
    aText := fHash.Value[i]; // was added in a background thread
  fSafe.WriteUnLock;
end;

function TRawUtf8InterningSlot.Existing(const aText: RawUtf8; aTextHash: cardinal): pointer;
var
  i: PtrInt;
begin
  result := nil;
  fSafe.ReadLock;
  i := fHash.Values.Hasher.FindOrNewComp(aTextHash, @aText);
  if i >= 0 then
    result := pointer(fHash.Value[i]); // return a pointer to unified string instance
  fSafe.ReadUnLock;
end;

procedure TRawUtf8InterningSlot.Clear;
begin
  fSafe.WriteLock;
  try
    fHash.Values.SetCount(0); // Values.Clear
    fHash.Values.Hasher.ForceReHash;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TRawUtf8InterningSlot.Clean(aMaxRefCount: TStrCnt): integer;
var
  i: integer;
  s, d: PPtrUInt; // points to RawUtf8 values
begin
  result := 0;
  if fHash.Count = 0 then
    exit;
  fSafe.WriteLock;
  try
    if fHash.Count = 0 then
      exit;
    s := pointer(fHash.Value);
    d := s;
    for i := 1 to fHash.Count do
    begin
      if PStrCnt(PAnsiChar(s^) - _STRCNT)^ <= aMaxRefCount then
      begin
        {$ifdef FPC}
        FastAssignNew(PRawUtf8(s)^);
        {$else}
        PRawUtf8(s)^ := '';
        {$endif FPC}
        inc(result);
      end
      else
      begin
        if s <> d then
        begin
          d^ := s^; // bypass COW assignments
          s^ := 0;  // avoid GPF
        end;
        inc(d);
      end;
      inc(s);
    end;
    if result > 0 then
    begin
      fHash.Values.SetCount((PtrUInt(d) - PtrUInt(fHash.Value)) div SizeOf(d^));
      fHash.Values.ForceReHash;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;


{ TRawUtf8Interning }

constructor TRawUtf8Interning.Create(aHashTables: integer);
var
  p: integer;
  i: PtrInt;
begin
  inherited Create; // may have been overriden
  for p := 0 to 9 do
    if aHashTables = 1 shl p then
    begin
      SetLength(fPool, aHashTables);
      fPoolLast := aHashTables - 1;
      for i := 0 to fPoolLast do
        fPool[i].Init;
      exit;
    end;
  raise ESynException.CreateUtf8('%.Create(%) not allowed: ' +
    'should be a power of 2 <= 512', [self, aHashTables]);
end;

procedure TRawUtf8Interning.Clear;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to fPoolLast do
      fPool[i].Clear;
end;

function TRawUtf8Interning.Clean(aMaxRefCount: TStrCnt): integer;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to fPoolLast do
      inc(result, fPool[i].Clean(aMaxRefCount));
end;

function TRawUtf8Interning.Count: integer;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to fPoolLast do
      inc(result, fPool[i].Count);
end;

procedure TRawUtf8Interning.Unique(var aResult: RawUtf8; const aText: RawUtf8);
var
  hash: cardinal;
begin
  if aText = '' then
    aResult := ''
  else if self = nil then
    aResult := aText
  else
  begin
    // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].Unique(aResult, aText, hash);
  end;
end;

procedure TRawUtf8Interning.UniqueText(var aText: RawUtf8);
var
  hash: cardinal;
begin
  if (self <> nil) and
     (aText <> '') then
  begin
    // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].UniqueText(aText, hash);
  end;
end;

function TRawUtf8Interning.Unique(const aText: RawUtf8): RawUtf8;
var
  hash: cardinal;
begin
  if aText = '' then
    FastAssignNew(result)
  else if self = nil then
    result := aText
  else
  begin
    // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].Unique(result, aText, hash);
  end;
end;

function TRawUtf8Interning.Existing(const aText: RawUtf8): pointer;
var
  hash: cardinal;
begin
  result := nil;
  if self = nil then
    exit;
  hash := InterningHasher(0, pointer(aText), length(aText));
  result := fPool[hash and fPoolLast].Existing(aText, hash);
end;

function TRawUtf8Interning.Unique(aText: PUtf8Char; aTextLen: PtrInt): RawUtf8;
begin
  Unique(result, aText, aTextLen);
end;

procedure TRawUtf8Interning.Unique(var aResult: RawUtf8;
  aText: PUtf8Char; aTextLen: PtrInt);
var
  hash: cardinal;
begin
  if (aText = nil) or
     (aTextLen <= 0) then
    FastAssignNew(aResult)
  else if self = nil then
    FastSetString(aResult, aText, aTextLen)
  else
  begin
    // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), aTextLen);
    fPool[hash and fPoolLast].UniqueFromBuffer(aResult, aText, aTextLen, hash);
  end;
end;

procedure TRawUtf8Interning.UniqueVariant(
  var aResult: variant; const aText: RawUtf8);
begin
  ClearVariantForString(aResult);
  Unique(RawUtf8(TVarData(aResult).VAny), aText);
end;

procedure TRawUtf8Interning.UniqueVariantString(var aResult: variant;
  const aText: string);
var
  tmp: RawUtf8;
begin
  StringToUtf8(aText, tmp);
  UniqueVariant(aResult, tmp);
end;

procedure TRawUtf8Interning.UniqueVariant(var aResult: variant);
var
  vd: TVarData absolute aResult;
  vt: cardinal;
begin
  vt := vd.VType;
  if vt = varString then
    UniqueText(RawUtf8(vd.VString))
  else if vt = varVariantByRef then
    UniqueVariant(PVariant(vd.VPointer)^)
  else if vt = varStringByRef then
    UniqueText(PRawUtf8(vd.VPointer)^);
end;


{ TRawUtf8List }

{$ifdef PUREMORMOT2}
constructor TRawUtf8List.Create;
begin
  CreateEx([fCaseSensitive]);
end;
{$else}
constructor TRawUtf8List.Create;
begin
  SetDefaultFlags;
  CreateEx(fFlags + [fCaseSensitive]);
end;

constructor TRawUtf8List.Create(aOwnObjects, aNoDuplicate, aCaseSensitive: boolean);
begin
  SetDefaultFlags;
  if aOwnObjects then
    include(fFlags, fObjectsOwned);
  if aNoDuplicate then
    include(fFlags, fNoDuplicate);
  if aCaseSensitive then
    include(fFlags, fCaseSensitive);
  CreateEx(fFlags);
end;

procedure TRawUtf8List.SetDefaultFlags;
begin
end;
procedure TRawUtf8ListLocked.SetDefaultFlags;
begin
  fFlags := [fThreadSafe];
end;
procedure TRawUtf8ListHashed.SetDefaultFlags;
begin
  fFlags := [fNoDuplicate];
end;
procedure TRawUtf8ListHashedLocked.SetDefaultFlags;
begin
  fFlags := [fNoDuplicate, fThreadSafe];
end;
{$endif PUREMORMOT2}

constructor TRawUtf8List.CreateEx(aFlags: TRawUtf8ListFlags);
begin
  inherited Create; // may have been overriden
  fNameValueSep := '=';
  fFlags := aFlags;
  fValues.InitSpecific(TypeInfo(TRawUtf8DynArray), fValue, ptRawUtf8, @fCount,
    not (fCaseSensitive in aFlags));
end;

destructor TRawUtf8List.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

procedure TRawUtf8List.SetCaseSensitive(Value: boolean);
begin
  if (self = nil) or
     (fCaseSensitive in fFlags = Value) then
    exit;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    if Value then
      include(fFlags, fCaseSensitive)
    else
      exclude(fFlags, fCaseSensitive);
    fValues.Hasher.InitSpecific(@fValues, ptRawUtf8, not Value, nil);
    Changed;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

procedure TRawUtf8List.SetCapacity(const capa: PtrInt);
begin
  if self <> nil then
  begin
    if fThreadSafe in fFlags then
      fSafe.WriteLock;
    try
      if capa <= 0 then
      begin
        // clear
        if fObjects <> nil then
        begin
          if fObjectsOwned in fFlags then
            RawObjectsClear(pointer(fObjects), fCount);
          fObjects := nil;
        end;
        fValues.Clear;
        if fNoDuplicate in fFlags then
          fValues.ForceReHash;
        Changed;
      end
      else
      begin
        // resize
        if capa < fCount then
        begin
          // resize down
          if fObjects <> nil then
          begin
            if fObjectsOwned in fFlags then
              RawObjectsClear(@fObjects[capa], fCount - capa - 1);
            SetLength(fObjects, capa);
          end;
          fValues.Count := capa;
          if fNoDuplicate in fFlags then
            fValues.ForceReHash;
          Changed;
        end;
        if capa > length(fValue) then
        begin
          // resize up
          SetLength(fValue, capa);
          if fObjects <> nil then
            SetLength(fObjects, capa);
        end;
      end;
    finally
      if fThreadSafe in fFlags then
        fSafe.WriteUnLock;
    end;
  end;
end;

function TRawUtf8List.Add(const aText: RawUtf8; aRaiseExceptionIfExisting: boolean): PtrInt;
begin
  result := AddObject(aText, nil, aRaiseExceptionIfExisting);
end;

function TRawUtf8List.AddObject(const aText: RawUtf8; aObject: TObject;
  aRaiseExceptionIfExisting: boolean; aFreeAndReturnExistingObject: PPointer;
  aReplaceExistingObject: boolean): PtrInt;
var
  added: boolean;
  obj: TObject;
begin
  result := -1;
  if self = nil then
    exit;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    if fNoDuplicate in fFlags then
    begin
      result := fValues.FindHashedForAdding(aText, added, {noadd=}true);
      if not added then
      begin
        obj := GetObject(result);
        if (obj = aObject) and
           (obj <> nil) then
          exit; // found identical aText/aObject -> behave as if added
        if aFreeAndReturnExistingObject <> nil then
        begin
          aObject.Free;
          aFreeAndReturnExistingObject^ := obj;
        end;
        if aRaiseExceptionIfExisting then
          raise ESynException.CreateUtf8('%.Add duplicate [%]', [self, aText]);
        if aReplaceExistingObject then
        begin
          if obj = nil then
            raise ESynException.CreateUtf8(
              '%.AddOrReplaceObject with no object at [%]', [self, aText]);
          if fObjectsOwned in fFlags then
            FreeAndNil(fObjects[result]);
          fObjects[result] := aObject;
        end
        else
          result := -1;
        exit;
      end;
    end;
    result := fValues.Add(aText);
    if (fObjects <> nil) or
       (aObject <> nil) then
    begin
      if result >= length(fObjects) then
        SetLength(fObjects, length(fValue)); // same capacity
      if aObject <> nil then
        fObjects[result] := aObject;
    end;
    if Assigned(fOnChange) then
      Changed;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

function TRawUtf8List.AddOrReplaceObject(const aText: RawUtf8; aObject: TObject): PtrInt;
begin
  result := AddObject(aText, aObject, {raiseexisting=}false, nil, {replace=}true);
end;

procedure TRawUtf8List.AddObjectUnique(const aText: RawUtf8;
  aObjectToAddOrFree: PPointer);
begin
  if fNoDuplicate in fFlags then
    AddObject(aText, aObjectToAddOrFree^, {raiseexc=}false,
      {freeandreturnexisting=}aObjectToAddOrFree);
end;

procedure TRawUtf8List.AddRawUtf8List(List: TRawUtf8List);
var
  i: PtrInt;
begin
  if List <> nil then
  begin
    BeginUpdate; // includes Safe.Lock
    try
      for i := 0 to List.fCount - 1 do
        AddObject(List.fValue[i], List.GetObject(i));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TRawUtf8List.BeginUpdate;
begin
  if InterLockedIncrement(fOnChangeLevel) > 1 then
    exit;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  fOnChangeBackupForBeginUpdate := fOnChange;
  fOnChange := OnChangeHidden;
  exclude(fFlags, fOnChangeTrigerred);
end;

procedure TRawUtf8List.EndUpdate;
begin
  if (fOnChangeLevel <= 0) or
     (InterLockedDecrement(fOnChangeLevel) > 0) then
    exit; // allows nested BeginUpdate..EndUpdate calls
  fOnChange := fOnChangeBackupForBeginUpdate;
  if (fOnChangeTrigerred in fFlags) and
     Assigned(fOnChange) then
    Changed;
  exclude(fFlags, fOnChangeTrigerred);
  if fThreadSafe in fFlags then
    fSafe.WriteUnLock;
end;

procedure TRawUtf8List.Changed;
begin
  if Assigned(fOnChange) then
  try
    fOnChange(self);
  except // ignore any exception in user code (may not trigger fSafe.UnLock)
  end;
end;

procedure TRawUtf8List.Clear;
begin
  SetCapacity(0); // will also call Changed
end;

procedure TRawUtf8List.InternalDelete(Index: PtrInt);
begin
  // caller ensured Index is correct
  fValues.Delete(Index); // includes dec(fCount)
  if PtrUInt(Index) < PtrUInt(length(fObjects)) then
  begin
    if fObjectsOwned in fFlags then
      fObjects[Index].Free;
    if fCount > Index then
      MoveFast(fObjects[Index + 1], fObjects[Index],
        (fCount - Index) * SizeOf(pointer));
    fObjects[fCount] := nil;
  end;
  if Assigned(fOnChange) then
    Changed;
end;

procedure TRawUtf8List.Delete(Index: PtrInt);
begin
  if (self <> nil) and
     (PtrUInt(Index) < PtrUInt(fCount)) then
    if fNoDuplicate in fFlags then // force update the hash table
      Delete(fValue[Index])
    else
      InternalDelete(Index);
end;

function TRawUtf8List.Delete(const aText: RawUtf8): PtrInt;
begin
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    if fNoDuplicate in fFlags then
      result := fValues.FindHashedAndDelete(aText, nil, {nodelete=}true)
    else
      result := FindRawUtf8(pointer(fValue), aText, fCount, fCaseSensitive in fFlags);
    if result >= 0 then
      InternalDelete(result);
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

function TRawUtf8List.DeleteFromName(const Name: RawUtf8): PtrInt;
begin
  result := -1;
  if fThreadSafe in fFlags then
    fSafe.ReadWriteLock;
  try
    result := IndexOfName(Name);
    if result >= 0 then
    begin
      if fThreadSafe in fFlags then
        fSafe.WriteLock;
      Delete(result);
    end;
  finally
    if fThreadSafe in fFlags then
    begin
      if result >= 0 then
        fSafe.WriteUnlock;
      fSafe.ReadWriteUnLock;
    end;
  end;
end;

function TRawUtf8List.Exists(const aText: RawUtf8): boolean;
begin
  if self <> nil then
    if fThreadSafe in fFlags then
    begin
      fSafe.ReadOnlyLock;
      try
        result := IndexOf(aText) >= 0;
      finally
        fSafe.ReadOnlyUnLock;
      end;
    end
    else
      result := IndexOf(aText) >= 0
  else
    result := false;
end;

function TRawUtf8List.IndexOf(const aText: RawUtf8): PtrInt;
begin
  if self <> nil then
  begin
    if fNoDuplicate in fFlags then
      result := fValues.FindHashed(aText)
    else
      result := FindRawUtf8(
        pointer(fValue), aText, fCount, fCaseSensitive in fFlags);
  end
  else
    result := -1;
end;

function TRawUtf8List.Get(Index: PtrInt): RawUtf8;
begin
  if (self = nil) or
     (PtrUInt(Index) >= PtrUInt(fCount)) then
    result := ''
  else
    result := fValue[Index];
end;

function TRawUtf8List.GetS(Index: PtrInt): string;
begin
  if (self = nil) or
     (PtrUInt(Index) >= PtrUInt(fCount)) then
    result := ''
  else
    Utf8ToStringVar(fValue[Index], result);
end;

function TRawUtf8List.GetCapacity: PtrInt;
begin
  if self = nil then
    result := 0
  else
    result := length(fValue);
end;

function TRawUtf8List.GetCount: PtrInt;
begin
  if self = nil then
    result := 0
  else
    result := fCount;
end;

function TRawUtf8List.GetTextPtr: PPUtf8CharArray;
begin
  result := pointer(self);
  if self <> nil then
    result := pointer(fValue);
end;

function TRawUtf8List.GetObjectPtr: PPointerArray;
begin
  result := pointer(self);
  if self <> nil then
    result := pointer(fObjects);
end;

function TRawUtf8List.GetName(Index: PtrInt): RawUtf8;
begin
  result := Get(Index);
  if result = '' then
    exit;
  Index := PosExChar(NameValueSep, result);
  if Index = 0 then
    result := ''
  else
    SetLength(result, Index - 1);
end;

function TRawUtf8List.GetObject(Index: PtrInt): pointer;
begin
  if (self <> nil) and
     (fObjects <> nil) and
     (PtrUInt(Index) < PtrUInt(fCount)) then
    result := fObjects[Index]
  else
    result := nil;
end;

function TRawUtf8List.GetObjectFrom(const aText: RawUtf8): pointer;
var
  ndx: PtrUInt;
begin
  result := nil;
  if (self <> nil) and
     (fObjects <> nil) then
  begin
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyLock;
    try
      ndx := IndexOf(aText);
      if ndx < PtrUInt(fCount) then
        result := fObjects[ndx];
    finally
      if fThreadSafe in fFlags then
        fSafe.ReadOnlyUnLock;
    end;
  end;
end;

function TRawUtf8List.GetText(const Delimiter: RawUtf8): RawUtf8;
var
  DelimLen, i, Len: PtrInt;
  P: PUtf8Char;
begin
  result := '';
  if (self = nil) or
     (fCount = 0) then
    exit;
  if fThreadSafe in fFlags then
    fSafe.ReadOnlyLock;
  try
    DelimLen := length(Delimiter);
    Len := DelimLen * (fCount - 1);
    for i := 0 to fCount - 1 do
      inc(Len, length(fValue[i]));
    FastSetString(result, Len);
    P := pointer(result);
    i := 0;
    repeat
      Len := length(fValue[i]);
      if Len > 0 then
      begin
        MoveFast(pointer(fValue[i])^, P^, Len);
        inc(P, Len);
      end;
      inc(i);
      if i >= fCount then
        Break;
      if DelimLen > 0 then
      begin
        MoveByOne(pointer(Delimiter), P, DelimLen);
        inc(P, DelimLen);
      end;
    until false;
  finally
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyUnLock;
  end;
end;

procedure TRawUtf8List.SaveToStream(Dest: TStream; const Delimiter: RawUtf8);
var
  W: TTextWriter;
  i: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  if (self = nil) or
     (fCount = 0) then
    exit;
  if fThreadSafe in fFlags then
    fSafe.ReadOnlyLock;
  try
    W := TTextWriter.Create(Dest, @temp, SizeOf(temp));
    try
      i := 0;
      repeat
        W.AddString(fValue[i]);
        inc(i);
        if i >= fCount then
          Break;
        W.AddString(Delimiter);
      until false;
      W.FlushFinal;
    finally
      W.Free;
    end;
  finally
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyUnLock;
  end;
end;

procedure TRawUtf8List.SaveToFile(
  const FileName: TFileName; const Delimiter: RawUtf8);
var
  FS: TStream;
begin
  FS := TFileStreamEx.Create(FileName, fmCreate);
  try
    SaveToStream(FS, Delimiter);
  finally
    FS.Free;
  end;
end;

function TRawUtf8List.GetTextCRLF: RawUtf8;
begin
  result := GetText;
end;

function TRawUtf8List.GetValue(const Name: RawUtf8): RawUtf8;
begin
  if fThreadSafe in fFlags then
    fSafe.ReadOnlyLock;
  try
    result := GetValueAt(IndexOfName(Name));
  finally
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyUnLock;
  end;
end;

function TRawUtf8List.GetValueAt(Index: PtrInt): RawUtf8;
begin
  result := Get(Index);
  if result = '' then
    exit;
  Index := PosExChar(NameValueSep, result);
  if Index = 0 then
    result := ''
  else
    TrimChars(result, Index, 0);
end;

function TRawUtf8List.EqualValueAt(Index: PtrInt; const aText: RawUtf8): boolean;
begin
  result := (self <>nil) and
            (PtrUInt(Index) < PtrUInt(fCount)) and
            (fValue[Index] = aText);
end;

function TRawUtf8List.IndexOfName(const Name: RawUtf8): PtrInt;
var
  UpperName: array[byte] of AnsiChar;
  table: PNormTable;
begin
  if self <> nil then
  begin
    PWord(UpperCopy255(UpperName{%H-}, Name))^ := ord(NameValueSep);
    table := @NormToUpperAnsi7;
    for result := 0 to fCount - 1 do
      if IdemPChar(Pointer(fValue[result]), UpperName, table) then
        exit;
  end;
  result := -1;
end;

function TRawUtf8List.IndexOfObject(aObject: TObject): PtrInt;
begin
  if (self <> nil) and
     (fObjects <> nil) then
  begin
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyLock;
    try
      result := PtrUIntScanIndex(pointer(fObjects), fCount, PtrUInt(aObject));
    finally
      if fThreadSafe in fFlags then
        fSafe.ReadOnlyUnLock;
    end
  end
  else
    result := -1;
end;

function TRawUtf8List.Contains(const aText: RawUtf8; aFirstIndex: integer): PtrInt;
var
  i: PtrInt; // use a temp variable to make oldest Delphi happy :(
begin
  result := -1;
  if self = nil then
    exit;
  if fThreadSafe in fFlags then
    fSafe.ReadOnlyLock;
  try
    for i := aFirstIndex to fCount - 1 do
      if PosEx(aText, fValue[i]) > 0 then
      begin
        result := i;
        exit;
      end;
  finally
    if fThreadSafe in fFlags then
      fSafe.ReadOnlyUnLock;
  end;
end;

procedure TRawUtf8List.OnChangeHidden(Sender: TObject);
begin
  if self <> nil then
    include(fFlags, fOnChangeTrigerred);
end;

procedure TRawUtf8List.Put(Index: PtrInt; const Value: RawUtf8);
begin
  if (self <> nil) and
     (PtrUInt(Index) < PtrUInt(fCount)) then
  begin
    fValue[Index] := Value;
    if Assigned(fOnChange) then
      Changed;
  end;
end;

procedure TRawUtf8List.PutS(Index: PtrInt; const Value: string);
begin
  Put(Index, StringToUtf8(Value));
end;

procedure TRawUtf8List.PutObject(Index: PtrInt; Value: pointer);
begin
  if (self <> nil) and
     (PtrUInt(Index) < PtrUInt(fCount)) then
  begin
    if fObjects = nil then
      SetLength(fObjects, Length(fValue));
    fObjects[Index] := Value;
    if Assigned(fOnChange) then
      Changed;
  end;
end;

procedure TRawUtf8List.SetText(const aText: RawUtf8; const Delimiter: RawUtf8);
begin
  SetTextPtr(pointer(aText), PUtf8Char(pointer(aText)) + length(aText), Delimiter);
end;

procedure TRawUtf8List.LoadFromFile(const FileName: TFileName);
begin
  SetText(RawUtf8FromFile(FileName), #13#10); // RawUtf8FromFile() detects BOM
end;

procedure TRawUtf8List.SetTextPtr(P, PEnd: PUtf8Char; const Delimiter: RawUtf8);
var
  DelimLen: PtrInt;
  DelimFirst: AnsiChar;
  PBeg, DelimNext: PUtf8Char;
  Line: RawUtf8;
begin
  DelimLen := length(Delimiter);
  BeginUpdate; // also makes fSafe.Lock
  try
    Clear;
    if (P <> nil) and
       (DelimLen > 0) and
       (P < PEnd) then
    begin
      DelimFirst := Delimiter[1];
      DelimNext := PUtf8Char(pointer(Delimiter)) + 1;
      repeat
        PBeg := P;
        while P < PEnd do
        begin
          if (P^ = DelimFirst) and
             CompareMemSmall(P + 1, DelimNext, DelimLen - 1) then
            break;
          inc(P);
        end;
        FastSetString(Line, PBeg, P - PBeg);
        AddObject(Line, nil);
        if P >= PEnd then
          break;
        inc(P, DelimLen);
      until P >= PEnd;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRawUtf8List.SetTextCRLF(const Value: RawUtf8);
begin
  SetText(Value, #13#10);
end;

procedure TRawUtf8List.SetFrom(const aText: TRawUtf8DynArray;
  const aObject: TObjectDynArray);
var
  n: integer;
begin
  BeginUpdate; // also makes fSafe.Lock
  try
    Clear;
    n := length(aText);
    if n = 0 then
      exit;
    SetCapacity(n);
    fCount := n;
    fValue := aText;
    fObjects := aObject;
    if fNoDuplicate in fFlags then
      fValues.ForceReHash;
  finally
    EndUpdate;
  end;
end;

procedure TRawUtf8List.SetValue(const Name, Value: RawUtf8);
var
  i: PtrInt;
  txt: RawUtf8;
begin
  txt := Name + RawUtf8(NameValueSep) + Value;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    i := IndexOfName(Name);
    if i < 0 then
      AddObject(txt, nil)
    else if fValue[i] <> txt then
    begin
      fValue[i] := txt;
      if fNoDuplicate in fFlags then
        fValues.Hasher.ForceReHash; // invalidate internal hash table
      Changed;
    end;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

function TRawUtf8List.GetCaseSensitive: boolean;
begin
  result := (self <> nil) and
            (fCaseSensitive in fFlags);
end;

function TRawUtf8List.GetNoDuplicate: boolean;
begin
  result := (self <> nil) and
            (fNoDuplicate in fFlags);
end;

function TRawUtf8List.UpdateValue(const Name: RawUtf8; var Value: RawUtf8;
  ThenDelete: boolean): boolean;
var
  i: PtrInt;
begin
  result := false;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    i := IndexOfName(Name);
    if i >= 0 then
    begin
      Value := GetValueAt(i); // copy value
      if ThenDelete then
        Delete(i); // optionally delete
      result := true;
    end;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

function TRawUtf8List.PopFirst(out aText: RawUtf8; aObject: PObject): boolean;
begin
  result := false;
  if fCount = 0 then
    exit;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    if fCount > 0 then
    begin
      aText := fValue[0];
      if aObject <> nil then
        if fObjects <> nil then
          aObject^ := fObjects[0]
        else
          aObject^ := nil;
      Delete(0);
      result := true;
    end;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;

function TRawUtf8List.PopLast(out aText: RawUtf8; aObject: PObject): boolean;
var
  last: PtrInt;
begin
  result := false;
  if fCount = 0 then
    exit;
  if fThreadSafe in fFlags then
    fSafe.WriteLock;
  try
    last := fCount - 1;
    if last >= 0 then
    begin
      aText := fValue[last];
      if aObject <> nil then
        if fObjects <> nil then
          aObject^ := fObjects[last]
        else
          aObject^ := nil;
      Delete(last);
      result := true;
    end;
  finally
    if fThreadSafe in fFlags then
      fSafe.WriteUnLock;
  end;
end;


{ ********** Efficient RTTI Values Binary Serialization and Comparison }

// per-type efficient binary serialization

function _BS_Ord(Data: pointer; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  result := ORDTYPE_SIZE[Info^.RttiOrd];
  Dest.Write(Data, result);
end;

function _BL_Ord(Data: pointer; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  result := ORDTYPE_SIZE[Info^.RttiOrd];
  Source.Copy(Data, result);
end;

function _BS_Float(Data: pointer; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  result := FLOATTYPE_SIZE[Info^.RttiFloat];
  Dest.Write(Data, result);
end;

function _BL_Float(Data: pointer; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  result := FLOATTYPE_SIZE[Info^.RttiFloat];
  Source.Copy(Data, result);
end;

function _BS_64(Data: PInt64; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  {$ifdef CPU32}
  Dest.Write8(Data);
  {$else}
  Dest.WriteI64(Data^);
  {$endif CPU32}
  result := 8;
end;

function _BL_64(Data: PQWord; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  Data^ := Source.Next8;
  result := 8;
end;

function _BS_String(Data: PRawByteString; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  Dest.WriteVar(pointer(Data^), length(Data^));
  result := SizeOf(pointer);
end;

function _BL_LString(Data: PRawByteString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    {$ifdef HASCODEPAGE}
    FastSetStringCP(Data^, Ptr, Len, Info^.AnsiStringCodePageStored);
    {$else}
    SetString(Data^, Ptr, Len);
    {$endif HASCODEPAGE}
  result := SizeOf(pointer);
end;

{$ifdef HASVARUSTRING}

function _BS_UString(Data: PUnicodeString; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  Dest.WriteVar(pointer(Data^), length(Data^) * 2);
  result := SizeOf(pointer);
end;

function _BL_UString(Data: PUnicodeString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    SetString(Data^, PWideChar(Ptr), Len shr 1); // length in bytes was stored
  result := SizeOf(pointer);
end;

{$endif HASVARUSTRING}

function _BS_WString(Data: PWideString; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  Dest.WriteVar(pointer(Data^), length(Data^) * 2);
  result := SizeOf(pointer);
end;

function _BL_WString(Data: PWideString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    SetString(Data^, PWideChar(Ptr), Len shr 1); // length in bytes was stored
  result := SizeOf(pointer);
end;

// efficient branchless comparison of every TRttiOrd/TRttiFloat raw value

function _BC_SByte(A, B: PShortInt; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 1;
end;

function _BC_UByte(A, B: PByte; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 1;
end;

function _BC_SWord(A, B: PSmallInt; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 2;
end;

function _BC_UWord(A, B: PWord; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 2;
end;

function _BC_SLong(A, B: PInteger; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 4;
end;

function _BC_ULong(A, B: PCardinal; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 4;
end;

function _BC_SQWord(A, B: PInt64; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 8;
end;

function _BC_UQWord(A, B: PQWord; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := 8;
end;

function _BC_Ord(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  result := RTTI_ORD_COMPARE[Info^.RttiOrd](A, B, Info, Compared);
end;

function _BC_Single(A, B: PSingle; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := SizeOf(single);
end;

function _BC_Double(A, B: PDouble; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := SizeOf(double);
end;

function _BC_Extended(A, B: PExtended; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ord(A^ > B^) - ord(A^ < B^);
  result := SizeOf(extended);
end;

function _BC_Float(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  result := RTTI_FLOAT_COMPARE[Info^.RttiFloat](A, B, Info, Compared);
end;

function _BC_64(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if Info^.IsQWord then
    Compared := ord(PQWord(A)^ > PQWord(B)^) - ord(PQWord(A)^ < PQWord(B)^)
  else
    Compared := ord(PInt64(A)^ > PInt64(B)^) - ord(PInt64(A)^ < PInt64(B)^);
  result := 8;
end;

function _BC_LString(A, B: PRawByteString; Info: PRttiInfo;
  out Compared: integer): PtrInt;
begin
  // StrComp() would fail for RawByteString
  {$ifdef CPUINTEL}
  compared := SortDynArrayAnsiString(A^, B^); // optimized asm using length()
  {$else}
  compared := SortDynArrayRawByteString(A^, B^);
  {$endif CPUINTEL}
  result := SizeOf(pointer);
end;

function _BC_WString(A, B: PPWideChar; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := StrCompW(A^, B^);
  result := SizeOf(pointer);
end;

function _BCI_LString(A, B: PPUtf8Char; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := StrIComp(A^, B^);
  result := SizeOf(pointer);
end;

function _BCI_WString(A, B: PPWideChar; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := AnsiICompW(A^, B^);
  result := SizeOf(pointer);
end;

function DelphiType(Info: PRttiInfo): integer;
  {$ifdef HASINLINE} inline; {$endif}
begin
  // compatible with legacy TDynArray.SaveTo() format
  if Info = nil then
    result := 0
  else
    {$ifdef FPC}
    result := ord(FPCTODELPHI[Info^.Kind]);
    {$else}
    result := ord(Info^.Kind);
    {$endif FPC}
end;

procedure DynArraySave(Data: PAnsiChar; ExternalCount: PInteger;
  Dest: TBufferWriter; Info: PRttiInfo);
var
  n, itemsize: PtrInt;
  sav: TRttiBinarySave;
label
  raw;
begin
  Info := Info^.DynArrayItemType(itemsize);
  Dest.Write1(0); // warning: store itemsize=0 (mORMot 1 ignores it anyway)
  Dest.Write1(DelphiType(Info));
  Data := PPointer(Data)^; // de-reference pointer to array data
  if Data = nil then
    Dest.Write1(0) // store dynamic array count of 0
  else
  begin
    if ExternalCount <> nil then
      n := ExternalCount^ // e.g. from TDynArray with external count
    else
      n := PDALen(Data - _DALEN)^ + _DAOFF;
    Dest.WriteVarUInt32(n);
    Dest.Write4(0); // warning: we don't store any Hash32 checksum any more
    if Info = nil then
raw:  Dest.Write(Data, itemsize * n)
    else
    begin
      sav := RTTI_BINARYSAVE[Info^.Kind];
      if Assigned(sav) then // paranoid check
        repeat
          inc(Data, sav(Data, Dest, Info));
          dec(n);
        until n = 0
      else
        goto raw;
    end;
  end;
end;

function _BS_DynArray(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  DynArraySave(Data, nil, Dest, Info);
  result := SizeOf(pointer);
end;

function DynArrayLoad(var Value; Source: PAnsiChar; TypeInfo: PRttiInfo;
  TryCustomVariants: PDocVariantOptions; SourceMax: PAnsiChar): PAnsiChar;
begin
  {$ifndef PUREMORMOT2}
  if SourceMax = nil then
    // mORMot 1 unsafe backward compatible: assume fake 100MB Source input
    SourceMax := Source + 100 shl 20;
  {$endif PUREMORMOT2}
  result := BinaryLoad(
    @Value, source, TypeInfo, nil, SourceMax, [rkDynArray], TryCustomVariants);
end;

function DynArraySave(var Value; TypeInfo: PRttiInfo): RawByteString;
begin
  result := BinarySave(@Value, TypeInfo, [rkDynArray]);
end;

function DynArrayLoadHeader(var Source: TFastReader;
  ArrayInfo, ItemInfo: PRttiInfo): integer;
begin
  Source.VarNextInt; // ignore stored itemsize (0 stored now)
  if Source.NextByte <> DelphiType(ItemInfo) then
    Source.ErrorData('RTTI_BINARYLOAD[rkDynArray] failed for %', [ArrayInfo.RawName]);
  result := Source.VarUInt32;
  if result <> 0 then
    Source.Next4; // ignore deprecated Hash32 checksum (0 stored now)
end;

function _BL_DynArray(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  n, itemsize: PtrInt;
  iteminfo: PRttiInfo;
  load: TRttiBinaryLoad;
label
  raw;
begin
  iteminfo := Info^.DynArrayItemType(itemsize); // nil for unmanaged items
  n := DynArrayLoadHeader(Source, Info, iteminfo);
  if PPointer(Data)^ <> nil then
    FastDynArrayClear(pointer(Data), iteminfo);
  if n > 0 then
  begin
    DynArrayNew(pointer(Data), n, itemsize); // allocate zeroed  memory
    Data := PPointer(Data)^; // point to first item
    if iteminfo = nil then
raw:  Source.Copy(Data, itemsize * n)
    else
    begin
      load := RTTI_BINARYLOAD[iteminfo^.Kind];
      if Assigned(load) then
        repeat
          inc(Data, load(Data, Source, iteminfo));
          dec(n);
        until n = 0
      else
        goto raw;
    end;
  end;
  result := SizeOf(pointer);
end;

function DynArrayCompare(A, B: PAnsiChar; ExternalCountA, ExternalCountB: PInteger;
  Info: PRttiInfo; CaseInSensitive: boolean): integer;
var
  n1, n2, n: PtrInt;
begin
  A := PPointer(A)^;
  B := PPointer(B)^;
  if A = B then
  begin
    result := 0;
    exit;
  end
  else if A = nil then
  begin
    result := -1;
    exit;
  end
  else if B = nil then
  begin
    result := 1;
    exit;
  end;
  if ExternalCountA <> nil then
    n1 := ExternalCountA^ // e.g. from TDynArray with external count
  else
    n1 := PDALen(A - _DALEN)^ + _DAOFF;
  if ExternalCountB <> nil then
    n2 := ExternalCountB^
  else
    n2 := PDALen(B - _DALEN)^ + _DAOFF;
  n := n1;
  if n > n2 then
    n := n2;
  if Info = TypeInfo(TObjectDynArray) then
    result := ObjectCompare(PObject(A), PObject(B), n, CaseInSensitive)
  else
    result := BinaryCompare(A, B, Info^.DynArrayItemType, n, CaseInSensitive);
  if result = 0 then
    result := n1 - n2;
end;

function DynArrayAdd(TypeInfo: PRttiInfo; var DynArray; const Item): integer;
var
  da: TDynArray;
begin
  da.Init(TypeInfo, DynArray);
  result := da.Add(Item);
end;

function DynArrayDelete(TypeInfo: PRttiInfo; var DynArray; Index: PtrInt): boolean;
var
  da: TDynArray;
begin
  da.Init(TypeInfo, DynArray);
  result := da.Delete(Index);
end;

function DynArrayEquals(TypeInfo: PRttiInfo; var Array1, Array2;
  Array1Count, Array2Count: PInteger; CaseInsensitive: boolean): boolean;
begin
  result := DynArrayCompare(@Array1, @Array2, Array1Count, Array2Count,
    TypeInfo, CaseInsensitive) = 0;
end;

{$ifdef FPCGENERICS}

function DynArrayAdd<TArray>(var DynArray: TArray; const Item): integer;
begin
  result := DynArrayAdd(TypeInfo(TArray), DynArray, Item);
end;

function DynArrayDelete<TArray>(var DynArray: TArray; Index: PtrInt): boolean;
begin
  result := DynArrayDelete(TypeInfo(TArray), DynArray, Index);
end;

function DynArrayCompare<TArray>(var Array1, Array2: TArray;
  CaseInSensitive: boolean): integer;
begin
  result := DynArrayCompare(
    @Array1, @Array2, nil, nil, TypeInfo(TArray), CaseInSensitive);
end;

{$endif FPCGENERICS}

function _BC_DynArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(A, B, nil, nil, Info, {caseinsens=}false);
  result := SizeOf(pointer);
end;

function _BCI_DynArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(A, B, nil, nil, Info, {caseinsens=}true);
  result := SizeOf(pointer);
end;

function _BC_ObjArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(
    A, B, nil, nil, TypeInfo(TObjectDynArray), {caseinsens=}false);
  result := SizeOf(pointer);
end;

function _BCI_ObjArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(
    A, B, nil, nil, TypeInfo(TObjectDynArray), {caseinsens=}true);
  result := SizeOf(pointer);
end;

function _BS_Record(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @RTTI_BINARYSAVE; // reuse pointer slot on stack
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        Dest.Write(Data, offset);
        inc(Data, offset);
      end;
      offset := PRttiBinarySaves(fields.Fields)[Info^.Kind](Data, Dest, Info);
      inc(Data, offset);
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset <> 0 then
    Dest.Write(Data, offset);
  result := fields.Size;
end;

function _BL_Record(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @RTTI_BINARYLOAD; // reuse pointer slot on stack
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        Source.Copy(Data, offset);
        inc(Data, offset);
      end;
      offset := PRttiBinaryLoads(fields.Fields)[Info^.Kind](Data, Source, Info);
      inc(Data, offset);
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset <> 0 then
    Source.Copy(Data, offset);
  result := fields.Size;
end;

function _RecordCompare(A, B: PUtf8Char; Info: PRttiInfo;
 CaseInSensitive: boolean): integer;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @RTTI_COMPARE[CaseInSensitive]; // reuse pointer slot on stack
  offset := 0;
  if fields.Count <> 0 then
    repeat
      dec(fields.Count);
      Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
      {$ifdef FPC_OLDRTTI}
      if Info^.Kind in rkManagedTypes then
      {$endif FPC_OLDRTTI}
      begin
        offset := f^.Offset - offset;
        if offset <> 0 then
        begin
          result := MemCmp(pointer(A), pointer(B), offset); // binary comparison
          if result <> 0 then
            exit;
          inc(A, offset);
          inc(B, offset);
        end;
        offset := PRttiCompares(fields.Fields)[Info^.Kind](A, B, Info, result);
        inc(A, offset);
        inc(B, offset);
        if result <> 0 then
          exit;
        inc(offset, f^.Offset);
      end;
      inc(f);
    until fields.Count = 0
  else
    result := 0;
  offset := PtrUInt(fields.Size) - offset;
  if offset <> 0 then
    result := MemCmp(pointer(A), pointer(B), offset); // trailing binary
end;

function _BC_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if A = B then
    Compared := 0
  else
    Compared := _RecordCompare(A, B, Info, {caseinsens=}false);
  result := Info^.RecordSize;
end;

function _BCI_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if A = B then
    Compared := 0
  else
    Compared := _RecordCompare(A, B, Info, {caseinsens=}true);
  result := Info^.RecordSize;
end;

function _BS_Array(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  n: PtrInt;
  sav: TRttiBinarySave;
label
  raw;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
raw:Dest.Write(Data, result)
  else
  begin
    sav := RTTI_BINARYSAVE[Info^.Kind];
    if Assigned(sav) then // paranoid check
      repeat
        inc(Data, sav(Data, Dest, Info));
        dec(n);
      until n = 0
    else
      goto raw;
  end;
end;

function _BL_Array(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  n: PtrInt;
  load: TRttiBinaryLoad;
label
  raw;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
raw:Source.Copy(Data, result)
  else
  begin
    load := RTTI_BINARYLOAD[Info^.Kind];
    if Assigned(load) then // paranoid check
      repeat
        inc(Data, load(Data, Source, Info));
        dec(n);
      until n = 0
    else
      goto raw;
  end;
end;

function _BC_Array(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
var
  n: PtrInt;
begin
  Info := Info^.ArrayItemType(n, result);
  Compared := BinaryCompare(A, B, Info, n, {CaseInSensitive=}false);
end;

function _BCI_Array(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
var
  n: PtrInt;
begin
  Info := Info^.ArrayItemType(n, result);
  Compared := BinaryCompare(A, B, Info, n, {CaseInSensitive=}true);
end;

procedure _BS_VariantComplex(Data: PVariant; Dest: TBufferWriter);
var
  temp: RawUtf8;
begin
  // not very fast, but creates valid JSON
  _VariantSaveJson(Data^, twJsonEscape, temp);
  Dest.Write(temp);
end;

procedure _BL_VariantComplex(Data: PVariant; var Source: TFastReader);
var
  temp: TSynTempBuffer;
begin
  Source.VarBlob(temp); // load into a private copy for in-place JSON parsing
  try
    BinaryVariantLoadAsJson(Data^, temp.buf, Source.CustomVariants);
  finally
    temp.Done;
  end;
end;

const
  // 0 for unserialized VType, 255 for valOleStr
  VARIANT_SIZE: array[varEmpty .. varWord64] of byte = (
    0, 0, 2, 4, 4, 8, 8, 8, 255, 0, 0, 2, 0, 0, 0, 0, 1, 1, 2, 4, 8, 8);

function _BS_Variant(Data: PVarData; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  vt: cardinal;
begin
  Data := VarDataFromVariant(PVariant(Data)^); // handle varByRef
  vt := Data^.VType;
  Dest.Write2(vt);
  if vt <= high(VARIANT_SIZE) then
  begin
    vt := VARIANT_SIZE[vt];
    if vt <> 0 then
      if vt = 255 then // valOleStr
        Dest.WriteVar(Data^.vAny, length(WideString(Data^.vAny)) * 2)
      else
        Dest.Write(@Data^.VInt64, vt); // simple types are stored as binary
  end
  else if (vt = varString) and  // expect only RawUtf8
          (Data^.vAny <> nil) then
    Dest.WriteVar(Data^.vAny, PStrLen(PAnsiChar(Data^.VAny) - _STRLEN)^)
  {$ifdef HASVARUSTRING}
  else if vt = varUString then
    Dest.WriteVar(Data^.vAny, length(UnicodeString(Data^.vAny)) * 2)
  {$endif HASVARUSTRING}
  else
    _BS_VariantComplex(pointer(Data), Dest);
  result := SizeOf(Data^);
end;

function _BL_Variant(Data: PVarData; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  vt: cardinal;
begin
  VarClear(PVariant(Data)^);
  Source.Copy(@Data^.VType, 2);
  Data^.VAny := nil; // to avoid GPF below
  vt := Data^.VType;
  if vt <= high(VARIANT_SIZE) then
  begin
    vt := VARIANT_SIZE[vt];
    if vt <> 0 then
      if vt = 255 then
        with Source.VarBlob do // valOleStr
          SetString(WideString(Data^.vAny), PWideChar(Ptr), Len shr 1)
      else
        Source.Copy(@Data^.VInt64, vt); // simple types
  end
  else if vt = varString then
    with Source.VarBlob do
      FastSetString(RawUtf8(Data^.vAny), Ptr, Len) // expect only RawUtf8
  {$ifdef HASVARUSTRING}
  else if vt = varUString then
    with Source.VarBlob do
      SetString(UnicodeString(Data^.vAny), PWideChar(Ptr), Len shr 1)
  {$endif HASVARUSTRING}
  else if Assigned(BinaryVariantLoadAsJson) then
    _BL_VariantComplex(pointer(Data), Source)
  else
    Source.ErrorData('RTTI_BINARYLOAD[tkVariant] missing mormot.core.json.pas', []);
  result := SizeOf(Data^);
end;

function _BC_Variant(A, B: PVarData; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if A = B then
    Compared := 0
  else
    Compared := SortDynArrayVariantComp(A^, B^, {caseinsens=}false);
  result := SizeOf(variant);
end;

function _BCI_Variant(A, B: PVarData; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if A = B then
    Compared := 0
  else
    Compared := SortDynArrayVariantComp(A^, B^, {caseinsens=}true);
  result := SizeOf(variant);
end;

function ObjectCompare(A, B: TObject; CaseInSensitive: boolean): integer;
var
  rA, rB: TRttiCustom;
  pA, pB: PRttiCustomProp;
  i: integer;
begin
  if (A = nil) or
     (B = nil) or
     (A = B) then
  begin
    result := ComparePointer(A, B);
    exit;
  end;
  result := 0;
  rA := Rtti.RegisterClass(A); // faster than RegisterType(Info)
  pA := pointer(rA.Props.List);
  if PClass(B)^.InheritsFrom(PClass(A)^) then
    // same (or similar/inherited) class -> compare per exact properties
    for i := 1 to rA.Props.Count do
    begin
      result := pA^.CompareValue(A, B, pA^, CaseInSensitive);
      if result <> 0 then
        exit;
      inc(pA);
    end
  else
  begin
    // compare properties by name
    rB := Rtti.RegisterClass(B);
    for i := 1 to rA.Props.Count do
    begin
      if pA^.Name <> '' then
      begin
        pB := rB.Props.Find(pA^.Name);
        if pB <> nil then // just ignore missing properties
        begin
          result := pA^.CompareValue(A, B, pB^, CaseInSensitive);
          if result <> 0 then
            exit;
        end;
      end;
      inc(pA);
    end;
  end;
end;

function _BC_Object(A, B: PObject; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ObjectCompare(A^, B^, {caseinsens=}false);
  result := SizeOf(pointer);
end;

function _BCI_Object(A, B: PObject; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ObjectCompare(A^, B^, {caseinsens=}true);
  result := SizeOf(pointer);
end;

function ObjectEquals(A, B: TObject): boolean;
begin
  result := ObjectCompare(A, B, {caseinsensitive=}false) = 0;
end;

function ObjectEqualsI(A, B: TObject): boolean;
begin
  result := ObjectCompare(A, B, {caseinsensitive=}true) = 0;
end;

function ObjectCompare(A, B: PObject; Count: PtrInt;
  CaseInsensitive: boolean): integer;
begin
  if Count > 0 then
    repeat
      result := ObjectCompare(A^, B^, CaseInsensitive);
      if result <> 0 then
        exit;
      inc(A);
      inc(B);
      dec(Count);
    until Count = 0;
  result := 0;
end;

function BinaryEquals(A, B: pointer; Info: PRttiInfo; PSize: PInteger;
  Kinds: TRttiKinds; CaseInSensitive: boolean): boolean;
var
  size, comp: integer;
  cmp: TRttiCompare;
begin
  cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
  if Assigned(cmp) and
     (Info^.Kind in Kinds) then
  begin
    size := cmp(A, B, Info, comp);
    if PSize <> nil then
      PSize^ := size;
    result := comp = 0;
  end
  else
    result := false; // no fair comparison possible
end;

function BinaryCompare(A, B: pointer; Info: PRttiInfo;
  CaseInSensitive: boolean): integer;
var
  cmp: TRttiCompare;
begin
  if A <> B then
    if Info <> nil then
    begin
      cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
      if Assigned(cmp) then
        cmp(A, B, Info, result)
      else
        result := MemCmp(A, B, Info^.RttiSize);
    end
    else
      result := ComparePointer(A, B)
  else
    result := 0;
end;

function BinaryCompare(A, B: pointer; Info: PRttiInfo; Count: PtrInt;
  CaseInSensitive: boolean): integer;
var
  cmp: TRttiCompare;
  siz: PtrInt;
begin
  if (A <> B) and
     (Count > 0) then
    if Info <> nil then
    begin
      cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
      if Assigned(cmp) then
        repeat
          siz := cmp(A, B, Info, result);
          inc(PAnsiChar(A), siz);
          inc(PAnsiChar(B), siz);
          if result <> 0 then
            exit;
          dec(Count);
        until Count = 0
      else
        result := MemCmp(A, B, Count * Info^.RttiSize);
    end
    else
      result := ComparePointer(A, B)
  else
    result := 0;
end;

{$ifndef PUREMORMOT2}

function BinarySaveLength(Data: pointer; Info: PRttiInfo; Len: PInteger;
  Kinds: TRttiKinds): integer;
var
  size: integer;
  W: TBufferWriter; // not very fast, but good enough (RecordSave don't use it)
  temp: array[byte] of byte; // will use mostly TFakeWriterStream.Write()
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TFakeWriterStream, @temp, SizeOf(temp));
    try
      size := save(Data, W, Info);
      if Len <> nil then
        Len^ := size;
      result := W.TotalWritten;
    finally
      W.Free;
    end;
  end
  else
    result := 0;
end;

function BinarySave(Data: pointer; Dest: PAnsiChar; Info: PRttiInfo;
  out Len: integer; Kinds: TRttiKinds): PAnsiChar;
var
  W: TBufferWriter;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TFakeWriterStream, Dest, 1 shl 30);
    try
      Len := save(Data, W, Info);
      result := Dest + W.BufferPosition; // Dest as a 1GB temporary buffer :)
    finally
      W.Free;
    end;
  end
  else
    result := nil;
end;

{$endif PUREMORMOT2}

procedure BinarySave(Data: pointer; Info: PRttiInfo; Dest: TBufferWriter);
var
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) then
    save(Data, Dest, Info);
end;

function BinarySave(Data: pointer; Info: PRttiInfo;
  Kinds: TRttiKinds; WithCrc: boolean): RawByteString;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      if WithCrc then
        W.Write4(0);
      save(Data, W, Info);
      result := W.FlushTo;
      if WithCrc then
        PCardinal(result)^ :=
          crc32c(0, @PCardinalArray(result)[1], length(result) - 4);
    finally
      W.Free;
    end;
  end
  else
    result := '';
end;

function BinarySaveBytes(Data: pointer; Info: PRttiInfo;
  Kinds: TRttiKinds): TBytes;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      save(Data, W, Info);
      result := W.FlushToBytes;
    finally
      W.Free;
    end;
  end
  else
    result := nil;
end;

procedure BinarySave(Data: pointer; var Dest: TSynTempBuffer; Info: PRttiInfo;
  Kinds: TRttiKinds; WithCrc: boolean);
var
  W: TBufferWriter;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TRawByteStringStream, @Dest.tmp,
      SizeOf(Dest.tmp) - 16); // Dest.Init() reserves 16 additional bytes
    try
      if WithCrc then
        W.Write4(0);
      save(Data, W, Info);
      if W.Stream.Position = 0 then
        // only Dest.tmp buffer was used -> just set the proper size
        Dest.Init(W.TotalWritten)
      else
        // more than 4KB -> temporary allocation through the temp RawByteString
        Dest.Init(W.FlushTo);
      if WithCrc then
        PCardinal(Dest.buf)^ :=
          crc32c(0, @PCardinalArray(Dest.buf)[1], Dest.len  - 4);
    finally
      W.Free;
    end;
  end
  else
    Dest.Init(0);
end;

function BinarySaveBase64(Data: pointer; Info: PRttiInfo; UriCompatible: boolean;
  Kinds: TRttiKinds; WithCrc: boolean): RawUtf8;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  tmp: RawByteString;
  P: PAnsiChar;
  len: integer;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and
     (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      if WithCrc then
        // placeholder for the trailing crc32c
        W.Write4(0);
      save(Data, W, Info);
      len := W.TotalWritten;
      if W.Stream.Position = 0 then
        // only temp buffer was used
        P := pointer(@temp)
      else
      begin
        // more than 8KB -> temporary allocation
        tmp := W.FlushTo;
        P := pointer(tmp);
      end;
      if WithCrc then
        // as mORMot 1.18 RecordSaveBase64()
        PCardinal(P)^ := crc32c(0, P + 4, len - 4);
      if UriCompatible then
        result := BinToBase64uri(P, len)
      else
        result := BinToBase64(P, len);
    finally
      W.Free;
    end;
  end
  else
    result := '';
end;

function BinaryLoad(Data: pointer; Source: PAnsiChar; Info: PRttiInfo;
  Len: PInteger; SourceMax: PAnsiChar; Kinds: TRttiKinds;
  TryCustomVariants: PDocVariantOptions): PAnsiChar;
var
  size: integer;
  read: TFastReader;
  load: TRttiBinaryLoad;
begin
  load := RTTI_BINARYLOAD[Info^.Kind];
  if Assigned(load) and
     (Info^.Kind in Kinds) and
     (SourceMax <> nil) then
  begin
    {%H-}read.Init(Source, SourceMax - Source);
    read.CustomVariants := TryCustomVariants;
    size := load(Data, read, Info);
    if Len <> nil then
      Len^ := size;
    result := read.P;
  end
  else
    result := nil;
end;

function BinaryLoad(Data: pointer; const Source: RawByteString; Info: PRttiInfo;
  Kinds: TRttiKinds; TryCustomVariants: PDocVariantOptions): boolean;
var
  P: PAnsiChar;
begin
  if Info^.Kind in Kinds then
  begin
    P := pointer(Source);
    P := BinaryLoad(Data, P, Info, nil, P + length(Source), Kinds, TryCustomVariants);
    result := (P <> nil) and
              (P - pointer(Source) = length(Source));
  end
  else
    result := false;
end;

function BinaryLoadBase64(Source: PAnsiChar; Len: PtrInt; Data: pointer;
  Info: PRttiInfo; UriCompatible: boolean; Kinds: TRttiKinds;
  WithCrc: boolean; TryCustomVariants: PDocVariantOptions): boolean;
var
  temp: TSynTempBuffer;
  tempend: pointer;
begin
  if (Len > 6) and
     (Info^.Kind in Kinds) then
  begin
    if UriCompatible then
      result := Base64uriToBin(Source, Len, temp)
    else
      result := Base64ToBin(Source, Len, temp);
    tempend := PAnsiChar(temp.buf) + temp.len;
    if result then
      if WithCrc then
        result := (temp.len >= 4) and
          (crc32c(0, PAnsiChar(temp.buf) + 4, temp.len - 4) = PCardinal(temp.buf)^) and
          (BinaryLoad(Data, PAnsiChar(temp.buf) + 4, Info, nil, tempend,
            Kinds, TryCustomVariants) = tempend)
      else
        result := (BinaryLoad(Data, temp.buf, Info, nil, tempend,
            Kinds, TryCustomVariants) = tempend);
    temp.Done;
  end
  else
    result := false;
end;


function RecordEquals(const RecA, RecB; TypeInfo: PRttiInfo; PRecSize: PInteger;
  CaseInSensitive: boolean): boolean;
begin
  result := BinaryEquals(@RecA, @RecB, TypeInfo, PRecSize,
    rkRecordTypes, CaseInSensitive);
end;

{$ifndef PUREMORMOT2}

function RecordSaveLength(const Rec; TypeInfo: PRttiInfo; Len: PInteger): integer;
begin
 result := {%H-}BinarySaveLength(@Rec, TypeInfo, Len, rkRecordTypes);
end;

function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: PRttiInfo;
  out Len: integer): PAnsiChar;
begin
  result := {%H-}BinarySave(@Rec, Dest, TypeInfo, Len, rkRecordTypes);
end;

function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: PRttiInfo): PAnsiChar;
var
  dummylen: integer;
begin
  result := {%H-}BinarySave(@Rec, Dest, TypeInfo, dummylen, rkRecordTypes);
end;

{$endif PUREMORMOT2}

function RecordSave(const Rec; TypeInfo: PRttiInfo): RawByteString;
begin
  result := BinarySave(@Rec, TypeInfo, rkRecordTypes);
end;

function RecordSaveBytes(const Rec; TypeInfo: PRttiInfo): TBytes;
begin
 result := BinarySaveBytes(@Rec, TypeInfo, rkRecordTypes);
end;

procedure RecordSave(const Rec; var Dest: TSynTempBuffer; TypeInfo: PRttiInfo);
begin
  BinarySave(@Rec, Dest, TypeInfo, rkRecordTypes);
end;

function RecordSaveBase64(const Rec; TypeInfo: PRttiInfo; UriCompatible: boolean): RawUtf8;
begin
  result := BinarySaveBase64(@Rec, TypeInfo, UriCompatible, rkRecordTypes);
end;

function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: PRttiInfo;
  Len: PInteger; SourceMax: PAnsiChar; TryCustomVariants: PDocVariantOptions): PAnsiChar;
begin
  {$ifndef PUREMORMOT2}
  if SourceMax = nil then
    // mORMot 1 unsafe backward compatible: assume fake 100MB Source input
    SourceMax := Source + 100 shl 20;
  {$endif PUREMORMOT2}
  result := BinaryLoad(@Rec, Source, TypeInfo, Len, SourceMax,
    rkRecordTypes, TryCustomVariants);
end;

function RecordLoad(var Rec; const Source: RawByteString; TypeInfo: PRttiInfo;
  TryCustomVariants: PDocVariantOptions): boolean;
begin
  result := BinaryLoad(@Rec, Source, TypeInfo, rkRecordTypes, TryCustomVariants);
end;

function RecordLoadBase64(Source: PAnsiChar; Len: PtrInt; var Rec;
  TypeInfo: PRttiInfo; UriCompatible: boolean; TryCustomVariants: PDocVariantOptions): boolean;
begin
  result := BinaryLoadBase64(Source, Len, @Rec, TypeInfo, UriCompatible,
    rkRecordTypes, {withcrc=}true, TryCustomVariants);
end;





{ ************ TDynArray and TDynArrayHashed Wrappers }

const
  // helper arrays to get the standard comparison/hash functions
  PT_SORT: array[{caseins=}boolean, TRttiParserType] of TDynArraySortCompare = (
    // case sensitive comparison/sort functions:
    (nil,                       //  ptNone
     nil,                       //  ptArray
     SortDynArrayBoolean,       //  ptBoolean
     SortDynArrayByte,          //  ptByte
     SortDynArrayCardinal,      //  ptCardinal
     SortDynArrayInt64,         //  ptCurrency
     SortDynArrayDouble,        //  ptDouble
     SortDynArrayExtended,      //  ptExtended
     SortDynArrayInt64,         //  ptInt64
     SortDynArrayInteger,       //  ptInteger
     SortDynArrayQWord,         //  ptQWord
     {$ifdef CPUINTEL}SortDynArrayAnsiString
     {$else}SortDynArrayRawByteString{$endif}, //  ptRawByteString
     SortDynArrayAnsiString,    //  ptRawJson
     SortDynArrayAnsiString,    //  ptRawUtf8
     nil,                       //  ptRecord
     SortDynArraySingle,        //  ptSingle
     {$ifdef UNICODE}SortDynArrayString
     {$else}SortDynArrayAnsiString{$endif}, //  ptString
     SortDynArrayUnicodeString, //  ptSynUnicode
     SortDynArrayDouble,        //  ptDateTime
     SortDynArrayDouble,        //  ptDateTimeMS
     SortDynArray128,           //  ptGuid
     SortDynArray128,           //  ptHash128
     SortDynArray256,           //  ptHash256
     SortDynArray512,           //  ptHash512
     SortDynArrayInt64,         //  ptOrm
     SortDynArrayInt64,         //  ptTimeLog
     SortDynArrayUnicodeString, //  ptUnicodeString
     SortDynArrayInt64,         //  ptUnixTime
     SortDynArrayInt64,         //  ptUnixMSTime
     SortDynArrayVariant,       //  ptVariant
     SortDynArrayUnicodeString, //  ptWideString
     SortDynArrayAnsiString,    //  ptWinAnsi
     SortDynArrayWord,          //  ptWord
     nil,                       //  ptEnumeration
     nil,                       //  ptSet
     SortDynArrayPointer,       //  ptClass
     nil,                       //  ptDynArray
     SortDynArrayPointer,       //  ptInterface
     SortDynArrayPUtf8Char,     //  ptPUtf8Char
     nil),                      //  ptCustom
    // case insensitive comparison/sort functions:
    (nil,                        //  ptNone
     nil,                        //  ptArray
     SortDynArrayBoolean,        //  ptBoolean
     SortDynArrayByte,           //  ptByte
     SortDynArrayCardinal,       //  ptCardinal
     SortDynArrayInt64,          //  ptCurrency
     SortDynArrayDouble,         //  ptDouble
     SortDynArrayExtended,       //  ptExtended
     SortDynArrayInt64,          //  ptInt64
     SortDynArrayInteger,        //  ptInteger
     SortDynArrayQWord,          //  ptQWord
     {$ifdef CPUINTEL}SortDynArrayAnsiString
     {$else}SortDynArrayRawByteString{$endif}, //  ptRawByteString
     SortDynArrayAnsiStringI,    //  ptRawJson
     SortDynArrayAnsiStringI,    //  ptRawUtf8
     nil,                        //  ptRecord
     SortDynArraySingle,         //  ptSingle
     SortDynArrayStringI,        //  ptString
     SortDynArrayUnicodeStringI, //  ptSynUnicode
     SortDynArrayDouble,         //  ptDateTime
     SortDynArrayDouble,         //  ptDateTimeMS
     SortDynArray128,            //  ptGuid
     SortDynArray128,            //  ptHash128
     SortDynArray256,            //  ptHash256
     SortDynArray512,            //  ptHash512
     SortDynArrayInt64,          //  ptOrm
     SortDynArrayInt64,          //  ptTimeLog
     SortDynArrayUnicodeStringI, //  ptUnicodeString
     SortDynArrayInt64,          //  ptUnixTime
     SortDynArrayInt64,          //  ptUnixMSTime
     SortDynArrayVariantI,       //  ptVariant
     SortDynArrayUnicodeStringI, //  ptWideString
     SortDynArrayAnsiStringI,    //  ptWinAnsi
     SortDynArrayWord,           //  ptWord
     nil,                        //  ptEnumeration
     nil,                        //  ptSet
     SortDynArrayPointer,        //  ptClass
     nil,                        //  ptDynArray
     SortDynArrayPointer,        //  ptInterface
     SortDynArrayPUtf8CharI,     //  ptPUtf8Char
     nil));                      //  ptCustom

function DynArraySortOne(Kind: TRttiParserType;
  CaseInsensitive: boolean): TDynArraySortCompare;
begin
  result := PT_SORT[CaseInsensitive, Kind];
end;

procedure ObjArraySort(var aValue; Compare: TDynArraySortCompare;
  CountPointer: PInteger);
begin
  DynArray(TypeInfo(TObjectDynArray), aValue, CountPointer).Sort(Compare);
end;


{ TDynArray }

procedure TDynArray.InitRtti(aInfo: TRttiCustom; var aValue;
  aCountPointer: PInteger);
begin
  fInfo := aInfo;
  fValue := @aValue;
  fCountP := aCountPointer;
  if fCountP <> nil then
    fCountP^ := 0;
  fCompare := nil;
  fSorted := false;
  fNoFinalize := false;
end;

procedure TDynArray.InitRtti(aInfo: TRttiCustom; var aValue);
begin
  fInfo := aInfo;
  fValue := @aValue;
  fCountP := nil;
  fCompare := nil;
  fSorted := false;
  fNoFinalize := false;
end;

procedure TDynArray.Init(aTypeInfo: PRttiInfo; var aValue;
  aCountPointer: PInteger);
begin
  if aTypeInfo^.Kind <> rkDynArray then
    raise EDynArray.CreateUtf8('TDynArray.Init: % is %, expected rkDynArray',
      [aTypeInfo.RawName, ToText(aTypeInfo.Kind)^]);
  InitRtti(Rtti.RegisterType(aTypeInfo), aValue, aCountPointer);
end;

function TDynArray.InitSpecific(aTypeInfo: PRttiInfo; var aValue;
  aKind: TRttiParserType; aCountPointer: PInteger; aCaseInsensitive: boolean): TRttiParserType;
begin
  if aTypeInfo^.Kind <> rkDynArray then
    raise EDynArray.CreateUtf8('TDynArray.InitSpecific: % is %, expected rkDynArray',
      [aTypeInfo.RawName, ToText(aTypeInfo.Kind)^]);
  InitRtti(Rtti.RegisterType(aTypeInfo), aValue, aCountPointer);
  result := SetParserType(aKind, aCaseInsensitive);
end;

function TDynArray.SetParserType(aKind: TRttiParserType;
  aCaseInsensitive: boolean): TRttiParserType;
begin
  case aKind of
    ptNone:
      if Assigned(fInfo.ArrayRtti) then
        result := fInfo.ArrayRtti.Parser
      else
        result := fInfo.ArrayFirstField;
  else
    result := aKind;
  end;
  fCompare := PT_SORT[aCaseInsensitive, result];
  if not Assigned(fCompare) then
    if result = ptVariant then
      raise EDynArray.CreateUtf8('TDynArray.SetParserType(%): missing mormot.core.json',
        [Info.Name, ToText(result)^])
    else if aKind <> ptNone then
      raise EDynArray.CreateUtf8('TDynArray.SetParserType(%) unsupported %',
        [Info.Name, ToText(result)^]);
end;

function TDynArray.ItemSize: PtrUInt;
begin
  result := fInfo.Cache.ItemSize;
end;

function TDynArray.GetCount: PtrInt;
begin // use result as a single temporary variable for better FPC asm generation
  result := PtrUInt(fCountP);
  if result <> 0 then
    result := PInteger(result)^ // count is external
  else
  begin
    result := PtrUInt(fValue);
    if result <> 0 then
    begin
      result := PPtrInt(result)^;
      if result <> 0 then
      begin
        result := PDALen(result - _DALEN)^; // count = length()
        {$ifdef FPC} inc(result, _DAOFF); {$endif}
      end;
    end;
  end;
end;

function TDynArray.GetCapacity: PtrInt;
begin
  result := PtrInt(fValue);
  if result <> 0 then
  begin
    result := PPtrInt(result)^;
    if result <> 0 then
    begin
      result := PDALen(result - _DALEN)^; // capacity = length()
      {$ifdef FPC} inc(result, _DAOFF); {$endif}
    end;
  end;
end;

procedure TDynArray.ItemCopy(Source, Dest: pointer);
var
  nfo: TRttiCustom;
begin
  nfo := fInfo.ArrayRtti;
  if (nfo <> nil) and // inlined nfo.ValueCopy() to avoid MoveFast() twice
     Assigned(nfo.Copy) then
    nfo.Copy(Dest, Source, nfo.Info) // also for T*ObjArray
  else
    MoveFast(Source^, Dest^, fInfo.Cache.ItemSize);
end;

procedure TDynArray.ItemClear(Item: pointer);
begin
  if Item = nil then
    exit;
  if (fInfo.ArrayRtti <> nil) and
     not fNoFinalize then
    fInfo.ArrayRtti.ValueFinalize(Item); // also for T*ObjArray
  FillCharFast(Item^, fInfo.Cache.ItemSize, 0); // always
end;

procedure TDynArray.ItemRandom(Item: pointer);
begin
  if Item <> nil then
    if fInfo.ArrayRtti <> nil then
      fInfo.ArrayRtti.ValueRandom(Item)
    else
      SharedRandom.Fill(Item, fInfo.Cache.ItemSize);
end;

function TDynArray.ItemEquals(A, B: pointer; CaseInSensitive: boolean): boolean;
begin
  result := ItemCompare(A, B, CaseInSensitive) = 0;
end;

function TDynArray.ItemCompare(A, B: pointer; CaseInSensitive: boolean): integer;
var
  comp: TRttiCompare;
  rtti: PRttiInfo;
label
  bin;
begin
  if Assigned(fCompare) then
    result := fCompare(A^, B^)
  else if not(rcfArrayItemManaged in fInfo.Flags) then
bin: // fast binary comparison with length
     result := MemCmp(A, B, fInfo.Cache.ItemSize)
  else
  begin
    rtti := fInfo.Cache.ItemInfo; // <> nil for managed items
    comp := RTTI_COMPARE[CaseInsensitive, rtti.Kind];
    if Assigned(comp) then
      comp(A, B, rtti, result)
    else
      goto bin;
  end;
end;

function TDynArray.Add(const Item): PtrInt;
begin
  result := GetCount;
  if fValue = nil then
    exit; // avoid GPF if void
  SetCount(result + 1);
  ItemCopy(@Item, PAnsiChar(fValue^) + result * fInfo.Cache.ItemSize);
end;

function TDynArray.New: PtrInt;
begin
  result := GetCount;
  SetCount(result + 1);
end;

function TDynArray.NewPtr: pointer;
var
  index: PtrInt;
begin
  index := GetCount; // in two explicit steps to ensure no problem at inlining
  SetCount(index + 1);
  result := PAnsiChar(fValue^) + index * fInfo.Cache.ItemSize;
end;

function TDynArray.Peek(var Dest): boolean;
var
  index: PtrInt;
begin
  index := GetCount - 1;
  result := index >= 0;
  if result then
    ItemCopy(PAnsiChar(fValue^) + index * fInfo.Cache.ItemSize, @Dest);
end;

function TDynArray.Pop(var Dest): boolean;
var
  index: PtrInt;
begin
  index := GetCount - 1;
  result := index >= 0;
  if result then
  begin
    ItemMoveTo(index, @Dest);
    SetCount(index);
  end;
end;

function TDynArray.PeekHead(var Dest): boolean;
begin
  result := GetCount <> 0;
  if result then
    ItemCopy(fValue^, @Dest);
end;

function TDynArray.PopHead(var Dest): boolean;
begin
  result := GetCount <> 0;
  if result then
  begin
    ItemMoveTo(0, @Dest);
    Delete(0);
  end;
end;

procedure TDynArray.Insert(Index: PtrInt; const Item);
var
  n: PtrInt;
  s: PtrUInt;
  P: PAnsiChar;
begin
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  SetCount(n + 1);
  s := fInfo.Cache.ItemSize;
  if PtrUInt(Index) < PtrUInt(n) then
  begin
    // reserve space for the new item
    P := PAnsiChar(fValue^) + PtrUInt(Index) * s;
    MoveFast(P[0], P[s], PtrUInt(n - Index) * s);
    if rcfArrayItemManaged in fInfo.Flags then // avoid GPF in ItemCopy() below
      FillCharFast(P^, s, 0);
  end
  else
    // Index>=Count -> add at the end
    P := PAnsiChar(fValue^) + PtrUInt(n) * s;
  ItemCopy(@Item, P);
end;

procedure TDynArray.Clear;
begin
  SetCount(0);
end;

function TDynArray.ClearSafe: boolean;
begin
  try
    SetCount(0);
    result := true;
  except // weak code, but may be a good idea in a destructor
    result := false;
  end;
end;

function TDynArray.Delete(aIndex: PtrInt): boolean;
var
  n: PtrInt;
  s, len: PtrUInt;
  P: PAnsiChar;
  wassorted: boolean;
begin
  result := false;
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  if PtrUInt(aIndex) >= PtrUInt(n) then
    exit; // out of range
  if PDACnt(PAnsiChar(fValue^) - _DACNT)^ > 1 then
    InternalSetLength(n, n); // unique
  dec(n);
  s := fInfo.Cache.ItemSize;
  P := PAnsiChar(fValue^) + PtrUInt(aIndex) * s;
  if (fInfo.ArrayRtti <> nil) and
     not fNoFinalize then
    fInfo.ArrayRtti.ValueFinalize(P); // also for T*ObjArray
  len := n - aIndex;
  if len <> 0 then
  begin
    len := len * s;
    MoveFast(P[s], P[0], len);
    inc(P, len);
  end;
  FillCharFast(P^, s, 0);
  wassorted := fSorted;
  SetCount(n); // won't reallocate
  fSorted := wassorted; // deletion won't change the order
  result := true;
end;

{$ifdef FPC} // very efficient inlined code on FPC
function TDynArray.ItemPtr(index: PtrInt): pointer;
label
  ok, ko; // labels make the code shorter and more efficient
var
  c: PtrUInt;
begin
  result := pointer(fValue);
  if result = nil then
    exit;
  result := PPointer(result)^;
  if result = nil then
    exit;
  c := PtrUInt(fCountP);
  if c <> 0 then
  begin
    if PtrUInt(index) < PCardinal(c)^ then
ok:   inc(PByte(result), index * fInfo.Cache.ItemSize) // branchless ext count
    else
      goto ko;
  end
  else // FPC stores high() in TDALen=PtrInt
    if PtrUInt(index) <= PPtrUInt(PAnsiChar(result) - _DALEN)^ then
      goto ok
    else
ko:   result := nil;
end;
{$else} // latest Delphi compilers have troubles with inlining + labels
function TDynArray.ItemPtr(index: PtrInt): pointer;
var
  c: PtrUInt;
begin
  result := pointer(fValue);
  if result = nil then
    exit;
  result := PPointer(result)^;
  if result = nil then
    exit;
  c := PtrUInt(fCountP);
  if c <> 0 then
    if PtrUInt(index) < PCardinal(c)^ then
      inc(PByte(result), index * fInfo.Cache.ItemSize) // branchless ext count
    else
      result := nil
  else // Delphi stores length() in TDALen=NativeInt
    if PtrUInt(index) < PPtrUInt(PtrUInt(result) - _DALEN)^ then
      inc(PByte(result), index * fInfo.Cache.ItemSize)
    else
      result := nil;
end;
{$endif FPC}

function TDynArray.ItemCopyAt(index: PtrInt; Dest: pointer): boolean;
var
  p: pointer;
begin
  p := ItemPtr(index);
  if p <> nil then
  begin
    ItemCopy(p, Dest);
    result := true;
  end
  else
    result := false;
end;

function TDynArray.ItemMoveTo(index: PtrInt; Dest: pointer): boolean;
var
  p: pointer;
begin
  p := ItemPtr(index);
  if (p = nil) or
     (Dest = nil) then
  begin
    result := false;
    exit;
  end;
  if (fInfo.ArrayRtti <> nil) and
     not fNoFinalize then
    fInfo.ArrayRtti.ValueFinalize(Dest); // also handle T*ObjArray
  MoveFast(p^, Dest^, fInfo.Cache.ItemSize);
  FillCharFast(p^, fInfo.Cache.ItemSize, 0);
  result := true;
end;

procedure TDynArray.ItemCopyFrom(Source: pointer; index: PtrInt;
  ClearBeforeCopy: boolean);
var
  p: pointer;
begin
  p := ItemPtr(index);
  if p <> nil then
  begin
    if ClearBeforeCopy then // safer if Source is a copy of p^
      ItemClear(p);
    ItemCopy(Source, p);
  end;
end;

{$ifdef CPU64}
procedure Exchg16(P1, P2: PPtrIntArray); inline;
var
  c: PtrInt;
begin
  c := P1[0];
  P1[0] := P2[0];
  P2[0] := c;
  c := P1[1];
  P1[1] := P2[1];
  P2[1] := c;
end;
{$endif CPU64}

procedure TDynArray.Reverse;
var
  n, siz: PtrInt;
  P1, P2: PAnsiChar;
  c: AnsiChar;
  i32: integer;
  i64: Int64;
begin
  n := GetCount - 1;
  if n > 0 then
  begin
    siz := fInfo.Cache.ItemSize;
    P1 := fValue^;
    case siz of
      1:
        begin
          // optimized version for TByteDynArray and such
          P2 := P1 + n;
          while P1 < P2 do
          begin
            c := P1^;
            P1^ := P2^;
            P2^ := c;
            inc(P1);
            dec(P2);
          end;
        end;
      4:
        begin
          // optimized version for TIntegerDynArray and such
          P2 := P1 + n * SizeOf(integer);
          while P1 < P2 do
          begin
            i32 := PInteger(P1)^;
            PInteger(P1)^ := PInteger(P2)^;
            PInteger(P2)^ := i32;
            inc(P1, 4);
            dec(P2, 4);
          end;
        end;
      8:
        begin
          // optimized version for TInt64DynArray + TDoubleDynArray and such
          P2 := P1 + n * SizeOf(Int64);
          while P1 < P2 do
          begin
            i64 := PInt64(P1)^;
            PInt64(P1)^ := PInt64(P2)^;
            PInt64(P2)^ := i64;
            inc(P1, 8);
            dec(P2, 8);
          end;
        end;
      16:
        begin
          // optimized version for 32-bit TVariantDynArray and such
          P2 := P1 + n * 16;
          while P1 < P2 do
          begin
            {$ifdef CPU64}Exchg16{$else}ExchgVariant{$endif}(pointer(P1), pointer(P2));
            inc(P1, 16);
            dec(P2, 16);
          end;
        end;
    {$ifdef CPU64}
      24:
        begin
          // optimized version for 64-bit TVariantDynArray and such
          P2 := P1 + n * 24;
          while P1 < P2 do
          begin
            ExchgVariant(Pointer(P1), Pointer(P2));
            inc(P1, 24);
            dec(P2, 24);
          end;
        end;
    {$endif CPU64}
    else
      begin
        // generic version
        P2 := P1 + n * siz;
        while P1 < P2 do
        begin
          Exchg(P1, P2, siz);
          inc(P1, siz);
          dec(P2, siz);
        end;
      end;
    end;
  end;
end;

procedure TDynArray.FillZero;
var
  n: integer;
begin
  n := GetCount;
  if n <> 0 then
    if not (rcfArrayItemManaged in fInfo.Flags) then
      FillCharFast(fValue^^, n * fInfo.Cache.ItemSize, 0) // e.g. THash256
    else
      FillZeroRtti(fInfo.Cache.ItemInfo, fValue^^);
end;

procedure TDynArray.SaveTo(W: TBufferWriter);
begin
  DynArraySave(pointer(fValue), fCountP, W, Info.Info);
end;

procedure TDynArray.SaveToStream(Stream: TStream);
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer; // 8KB buffer
begin
  if (fValue = nil) or
     (Stream = nil) then
    exit; // avoid GPF if void
  W := TBufferWriter.Create(Stream, @tmp, SizeOf(tmp));
  try
    SaveTo(W);
    W.Flush;
  finally
    W.Free;
  end;
end;

function TDynArray.SaveTo: RawByteString;
var
  W: TRawByteStringStream;
begin
  W := TRawByteStringStream.Create;
  try
    SaveToStream(W);
    result := W.DataString;
  finally
    W.Free;
  end;
end;

function TDynArray.LoadFrom(Source, SourceMax: PAnsiChar): PAnsiChar;
var
  read: TFastReader;
begin
  {$ifndef PUREMORMOT2}
  if SourceMax = nil then
    // mORMot 1 unsafe backward compatible: assume fake 100MB Source input
    SourceMax := Source + 100 shl 20;
  {$endif PUREMORMOT2}
  {%H-}read.Init(Source, SourceMax - Source);
  LoadFromReader(read);
  if read.P <> Source then
    result := read.P
  else
    result := nil;
end;

function TDynArray.LoadFromBinary(const Buffer: RawByteString): boolean;
var
  read: TFastReader;
begin
  read.Init(Buffer);
  LoadFromReader(read);
  result := read.P = read.Last;
end;

procedure TDynArray.LoadFromReader(var Read: TFastReader);
begin
  if fValue <> nil then
  begin
    _BL_DynArray(pointer(fValue), Read, Info.Info);
    if fCountP <> nil then // _BL_DynArray() set length -> reflect on Count
      if fValue^ = nil then
        fCountP^ := 0
      else
        fCountP^ := PDALen(PAnsiChar(fValue^) - _DALEN)^ + _DAOFF;
  end;
end;

procedure TDynArray.LoadFromStream(Stream: TCustomMemoryStream);
var
  S, P: PAnsiChar;
begin
  S := PAnsiChar(Stream.Memory);
  P := LoadFrom(S + Stream.Position, S + Stream.Size);
  Stream.Seek(Int64(PtrUInt(P) - PtrUInt(S)), soBeginning);
end;

function TDynArray.SaveToJson(EnumSetsAsText: boolean; reformat: TTextWriterJsonFormat): RawUtf8;
begin
  SaveToJson(result, EnumSetsAsText, reformat);
end;

procedure TDynArray.SaveToJson(out result: RawUtf8; EnumSetsAsText: boolean;
  reformat: TTextWriterJsonFormat);
begin
  SaveToJson(result, TEXTWRITEROPTIONS_ENUMASTEXT[EnumSetsAsText],
    TEXTWRITEROBJECTOPTIONS_ENUMASTEXT[EnumSetsAsText], reformat);
end;

procedure TDynArray.SaveToJson(out result: RawUtf8; Options: TTextWriterOptions;
  ObjectOptions: TTextWriterWriteObjectOptions; reformat: TTextWriterJsonFormat);
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if GetCount = 0 then
    result := '[]'
  else
  begin
    W := DefaultJsonWriter.CreateOwnedStream(temp);
    try
      W.CustomOptions := W.CustomOptions + Options;
      SaveToJson(W, ObjectOptions);
      W.SetText(result, reformat);
    finally
      W.Free;
    end;
  end;
end;

procedure TDynArray.SaveToJson(W: TTextWriter;
  ObjectOptions: TTextWriterWriteObjectOptions);
var
  len, backup: PtrInt;
  hacklen: PDALen;
begin
  len := GetCount;
  if len = 0 then
    W.Add('[', ']')
  else
  begin
    hacklen := PDALen(PAnsiChar(fValue^) - _DALEN);
    backup := hacklen^;
    try
      hacklen^ := len - _DAOFF; // may use ExternalCount
      W.AddTypedJson(fValue, Info.Info, ObjectOptions); // from mormot.core.json
    finally
      hacklen^ := backup;
    end;
  end;
end;

procedure _GetDataFromJson(Data: pointer; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; Rtti: TRttiCustom;
  CustomVariantOptions: PDocVariantOptions; Tolerant: boolean;
  Interning: TRawUtf8InterningAbstract);
begin
  raise ERttiException.Create('GetDataFromJson() not implemented - ' +
    'please include mormot.core.json in your uses clause');
end;

function TDynArray.LoadFromJson(P: PUtf8Char; EndOfObject: PUtf8Char;
  CustomVariantOptions: PDocVariantOptions; Tolerant: boolean;
  Interning: TRawUtf8InterningAbstract): PUtf8Char;
begin
  SetCount(0); // faster to use our own routine now
  GetDataFromJson(fValue, P,
    EndOfObject, Info, CustomVariantOptions, Tolerant, Interning);
  if fCountP <> nil then
    // GetDataFromJson() set the array length (capacity), not the external count
    if fValue^ = nil then
      fCountP^ := 0
    else
      fCountP^ := PDALen(PAnsiChar(fValue^) - _DALEN)^ + _DAOFF;
  result := P;
end;

function TDynArray.LoadFromJson(const Json: RawUtf8;
  CustomVariantOptions: PDocVariantOptions; Tolerant: boolean;
  Interning: TRawUtf8InterningAbstract): boolean;
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json);
  try
    result := LoadFromJson(tmp.buf, nil,
      CustomVariantOptions, Tolerant, Interning) <> nil;
  finally
    tmp.Done;
  end;
end;

function TDynArray.ItemCopyFirstField(Source, Dest: Pointer): boolean;
var
  rtti: PRttiInfo;
begin
  result := false;
  if fInfo.ArrayFirstField in ptUnmanagedTypes then
    MoveFast(Source^, Dest^, PT_SIZE[fInfo.ArrayFirstField])
  else
    begin
      rtti := PT_INFO[fInfo.ArrayFirstField];
      if rtti = nil then
        exit; // ptNone, ptInterface, ptCustom
      rtti^.Copy(Dest, Source);
    end;
  result := true;
end;

function BruteFind(P, V: PAnsiChar; cmp: TDynArraySortCompare; n, s: PtrInt): PtrInt;
begin // array is very small, or not sorted -> O(n) iterative search
  result := 0;
  repeat
    if cmp(P^, V^) = 0 then
      exit;
    inc(result);
    inc(P, s);
  until result = n;
  result := -1;
end;

function TDynArray.Find(const Item; const aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare): PtrInt;
var
  n, L: PtrInt;
  cmp: integer;
  P: PAnsiChar;
begin
  n := GetCount;
  if Assigned(aCompare) and
     (n > 0) then
  begin
    P := fValue^;
    if length(aIndex) >= n then
    begin // fast O(log(n)) binary search over aIndex[]
      dec(n);
      L := 0;
      repeat
        result := (L + n) shr 1;
        cmp := aCompare(P[aIndex[result] * fInfo.Cache.ItemSize], Item);
        if cmp = 0 then
        begin
          result := aIndex[result]; // returns index in TDynArray
          exit;
        end;
        if cmp < 0 then
          L := result + 1
        else
          n := result - 1;
      until L > n;
    end
    else
    begin // fallback to O(n) linear search on void aIndex[]
      result := BruteFind(P, @Item, aCompare, n, fInfo.Cache.ItemSize);
      exit;
    end;
  end;
  result := -1;
end;

function SortFind(P, V: PAnsiChar; cmp: TDynArraySortCompare; R, s: PtrInt): PtrInt;
var
  m, L: PtrInt;
  res: integer;
begin // array is sorted -> use fast O(log(n)) binary search
  L := 0;
  dec(R);
  repeat
    result := (L + R) shr 1;
    res := cmp(P[result * s], V^);
    if res = 0 then
      exit;
    m := result - 1;
    inc(result);
    if res > 0 then // compile as cmovnle/cmovle opcodes on FPC x86_64
      R := m
    else
      L := result;
  until L > R;
  result := -1;
end;

function TDynArray.Find(const Item; aCompare: TDynArraySortCompare): PtrInt;
var
  n: PtrInt;
  fnd: function(P, V: PAnsiChar; cmp: TDynArraySortCompare; n, s: PtrInt): PtrInt;
begin
  n := GetCount;
  if not Assigned(aCompare) then
    aCompare := fCompare;
  if n > 0 then
    if Assigned(aCompare) then
    begin
      fnd := @BruteFind;
      if n > 10 then
        if fSorted and
           (@aCompare = @fCompare) then
          fnd := @SortFind
        else if not(rcfArrayItemManaged in fInfo.Flags) and
                (fInfo.ArrayRtti <> nil) and
                (@aCompare = @PT_SORT[false, fInfo.ArrayRtti.Parser]) then
        begin // optimized brute force search with potential SSE2 asm
          result := AnyScanIndex(fValue^, @Item, n, fInfo.Cache.ItemSize);
          exit;
        end;
      result := fnd(fValue^, @Item, aCompare, n, fInfo.Cache.ItemSize);
    end
    else
      result := IndexOf(Item, {caseinsens=}false) // no fCompare -> default
  else
    result := -1;
end;

function TDynArray.FindIndex(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): PtrInt;
begin
  if aIndex <> nil then
    result := Find(Item, aIndex^, aCompare)
  else
    result := Find(Item, aCompare);
end;

function TDynArray.FindAndFill(var Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then
    // if found, fill Item with the matching item
    ItemCopy(PAnsiChar(fValue^) + (result * fInfo.Cache.ItemSize), @Item);
end;

function TDynArray.FindAndDelete(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then
    // if found, delete the item from the array
    Delete(result);
end;

function TDynArray.FindAndUpdate(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then
    // if found, fill Item with the matching item
    ItemCopy(@Item, PAnsiChar(fValue^) + (result * fInfo.Cache.ItemSize));
end;

function TDynArray.FindAndAddIfNotExisting(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result < 0 then
    // -1 will mark success
    Add(Item);
end;

function TDynArray.FindAllSorted(const Item;
  out FirstIndex, LastIndex: integer): boolean;
var
  found, last: integer; // FastLocateSorted() requires an integer
  siz: PtrInt;
  P, val: PAnsiChar;
begin
  result := FastLocateSorted(Item, found);
  if not result then
    exit;
  FirstIndex := found;
  P := fValue^;
  siz := fInfo.Cache.ItemSize;
  inc(P, found * siz);
  val := P; // faster than Item after RawUtf8 interning
  while FirstIndex > 0 do
  begin
    dec(P, siz);
    if fCompare(P^, val^) <> 0 then
      break;
    dec(FirstIndex);
  end;
  last := GetCount - 1;
  LastIndex := found;
  P := val;
  while LastIndex < last do
  begin
    inc(P, siz);
    if fCompare(P^, val^) <> 0 then
      break;
    inc(LastIndex);
  end;
end;

function TDynArray.FindAllSorted(const Item; out FindCount: integer): pointer;
var
  found: integer; // FastLocateSorted() requires an integer
  siz: PtrInt;
  P, fnd, limit: PAnsiChar;
begin
  FindCount := 0;
  result := nil;
  if not FastLocateSorted(Item, found) then
    exit;
  P := fValue^;
  limit := P;
  siz := fInfo.Cache.ItemSize;
  inc(P, found * siz);
  fnd := P; // faster than Item after RawUtf8 interning
  repeat
    result := P;
    inc(FindCount);
    dec(P, siz);
  until (P < limit) or
        (fCompare(P^, fnd^) <> 0);
  inc(limit, GetCount * siz);
  P := fnd;
  repeat
    inc(P, siz);
    if (P >= limit) or
       (fCompare(P^, fnd^) <> 0) then
      break;
    inc(FindCount);
  until false;
end;

function TDynArray.FastLocateSorted(const Item; out Index: integer): boolean;
var
  n, i: PtrInt;
  cmp: integer;
  P: PAnsiChar;
begin
  result := False;
  n := GetCount;
  if Assigned(fCompare) then
    if n = 0 then // a void array is always sorted
      Index := 0
    else if fSorted then
    begin
      P := fValue^;
      // first compare with the last sorted item (common case, e.g. with IDs)
      dec(n);
      cmp := fCompare(Item, P[n * fInfo.Cache.ItemSize]);
      if cmp >= 0 then
      begin
        Index := n;
        if cmp = 0 then
          // was just added: returns true + index of last item
          result := true
        else
          // bigger than last item: returns false + insert after last position
          inc(Index);
        exit;
      end;
      // O(log(n)) binary search of the sorted position
      Index := 0; // more efficient code if we use Index and not a local var
      repeat
        i := (Index + n) shr 1;
        cmp := fCompare(Item, P[i * fInfo.Cache.ItemSize]);
        if cmp = 0 then
        begin
          // returns true + index of existing Item
          Index := i;
          result := True;
          exit;
        end
        else if cmp > 0 then
          Index := i + 1
        else
          n := i - 1;
      until Index > n;
      // Item not found: returns false + the index where to insert
    end
    else
      Index := -1 // not Sorted
  else
    Index := -1; // no fCompare()
end;

procedure TDynArray.FastAddSorted(Index: PtrInt; const Item);
begin
  Insert(Index, Item);
  fSorted := true; // Insert -> SetCount -> fSorted := false
end;

procedure TDynArray.FastDeleteSorted(Index: PtrInt);
begin
  Delete(Index);
  fSorted := true; // Delete -> SetCount -> fSorted := false
end;

function TDynArray.FastLocateOrAddSorted(const Item; wasAdded: PBoolean): integer;
var
  added: boolean;
begin
  added := not FastLocateSorted(Item, result) and
           (result >= 0);
  if added then
  begin
    Insert(result, Item);
    fSorted := true; // Insert -> SetCount -> fSorted := false
  end;
  if wasAdded <> nil then
    wasAdded^ := added;
end;

type
  // internal structure used to make QuickSort faster & with less stack usage
  {$ifdef USERECORDWITHMETHODS}
  TDynArrayQuickSort = record
  {$else}
  TDynArrayQuickSort = object
  {$endif USERECORDWITHMETHODS}
  public
    Compare: TDynArraySortCompare;
    CompareEvent: TOnDynArraySortCompare;
    Pivot: pointer;
    index: PCardinalArray;
    ElemSize: cardinal;
    p: PtrInt;
    Value: PAnsiChar;
    IP, JP: PAnsiChar;
    procedure QuickSort(L, R: PtrInt);
    procedure QuickSortIndexed(L, R: PtrInt);
    procedure QuickSortEvent(L, R: PtrInt);
    procedure QuickSortEventReverse(L, R: PtrInt);
  end;

procedure QuickSortIndexedPUtf8Char(Values: PPUtf8CharArray; Count: integer;
  var SortedIndexes: TCardinalDynArray; CaseSensitive: boolean);
var
  QS: TDynArrayQuickSort;
begin
  if CaseSensitive then
    QS.Compare := SortDynArrayPUtf8Char
  else
    QS.Compare := SortDynArrayPUtf8CharI;
  QS.Value := pointer(Values);
  QS.ElemSize := SizeOf(PUtf8Char);
  SetLength(SortedIndexes, Count);
  FillIncreasing(pointer(SortedIndexes), 0, Count);
  QS.Index := pointer(SortedIndexes);
  QS.QuickSortIndexed(0, Count - 1);
end;

procedure DynArraySortIndexed(Values: pointer; ItemSize, Count: integer;
  out Indexes: TSynTempBuffer; Compare: TDynArraySortCompare);
begin
  DynArraySortIndexed(Values, ItemSize, Count,
    pointer(Indexes.InitIncreasing(Count)), Compare);
end;

procedure DynArraySortIndexed(Values: pointer; ItemSize, Count: integer;
  Indexes: PCardinalArray; Compare: TDynArraySortCompare);
var
  QS: TDynArrayQuickSort;
begin
  QS.Compare := Compare;
  QS.Value := Values;
  QS.ElemSize := ItemSize;
  QS.Index := Indexes;
  QS.QuickSortIndexed(0, Count - 1);
end;

procedure TDynArrayQuickSort.QuickSort(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while Compare(IP^, Pivot^) < 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while Compare(JP^, Pivot^) > 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortEvent(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while CompareEvent(IP^, Pivot^) < 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while CompareEvent(JP^, Pivot^) > 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortEvent(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortEvent(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortEventReverse(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while CompareEvent(IP^, Pivot^) > 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while CompareEvent(JP^, Pivot^) < 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortEventReverse(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortEventReverse(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortIndexed(L, R: PtrInt);
var
  I, J: PtrInt;
  tmp: integer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + index[p] * ElemSize;
        while Compare(Value[index[I] * ElemSize], Pivot^) < 0 do
          inc(I);
        while Compare(Value[index[J] * ElemSize], Pivot^) > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            tmp := index[I];
            index[I] := index[J];
            index[J] := tmp;
          end;
          if p = I then
            p := J
          else if p = J then
            p := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortIndexed(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortIndexed(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArray.Sort(aCompare: TDynArraySortCompare);
begin
  SortRange(0, Count - 1, aCompare);
  fSorted := true;
end;

procedure QuickSortPtr(L, R: PtrInt; Compare: TDynArraySortCompare; V: PPointerArray);
var
  I, J, P: PtrInt;
  tmp: pointer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(V[I], V[P]) < 0 do
          inc(I);
        while Compare(V[J], V[P]) > 0 do
          dec(J);
        if I <= J then
        begin
          tmp := V[I];
          V[I] := V[J];
          V[J] := tmp;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortPtr(L, J, Compare, V);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortPtr(I, R, Compare, V);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArray.SortRange(aStart, aStop: integer;
  aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
begin
  if aStop <= aStart then
    exit; // nothing to sort
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and
     (fValue <> nil) and
     (fValue^ <> nil) then
  begin
    if fInfo.ArrayRtti <> nil then
      case fInfo.ArrayRtti.Parser of
        // call optimized sorting functions for most simple types
        ptWord:
          if @QuickSort.Compare = @SortDynArrayWord then
          begin
            QuickSortWord(fValue^, aStart, aStop);
            exit;
          end;
        ptInteger:
          if @QuickSort.Compare = @SortDynArrayInteger then
          begin
            QuickSortInteger(fValue^, aStart, aStop);
            exit;
          end;
        ptInt64:
          if @QuickSort.Compare = @SortDynArrayInt64 then
          begin
            QuickSortInt64(fValue^, aStart, aStop);
            exit;
          end;
        ptQWord:
          if @QuickSort.Compare = @SortDynArrayQWord then
          begin
            QuickSortQWord(fValue^, aStart, aStop);
            exit;
          end;
        ptDouble:
          if @QuickSort.Compare = @SortDynArrayDouble then
          begin
            QuickSortDouble(fValue^, aStart, aStop);
            exit;
          end;
      end;
    if fInfo.Cache.ItemSize = SizeOf(pointer) then
      // dedicated function for pointers - e.g. strings or T*ObjArray
      QuickSortPtr(aStart, aStop, QuickSort.Compare, fValue^)
    else
    begin
      // generic process for any kind of array items
      QuickSort.Value := fValue^;
      QuickSort.ElemSize := fInfo.Cache.ItemSize;
      QuickSort.QuickSort(aStart, aStop);
    end;
  end;
end;

function TDynArray.IsSorted(aCompare: TDynArraySortCompare): boolean;
var
  n: integer;
  siz: PtrInt;
  p, prev: PAnsiChar;
begin
  result := false;
  n := GetCount;
  if not Assigned(aCompare) then
    aCompare := fCompare;
  if (not Assigned(aCompare)) or
     (n = 0) then
    exit; // nothing to sort
  siz := fInfo.Cache.ItemSize;
  p := fValue^;
  prev := p;
  inc(p, siz);
  dec(n);
  if n <> 0 then
    repeat
      if aCompare(p^, prev^) < 0 then
        exit;
      prev := p;
      inc(p, siz);
      dec(n);
    until n = 0;
  result := true; // all items are sorted
end;

procedure TDynArray.EnsureSorted(aCompare: TDynArraySortCompare);
begin
  if IsSorted(aCompare) then
    fSorted := true
  else
    Sort(aCompare);
end;

procedure TDynArray.Sort(const aCompare: TOnDynArraySortCompare; aReverse: boolean);
var
  QuickSort: TDynArrayQuickSort;
  R: PtrInt;
begin
  if (not Assigned(aCompare)) or
     (fValue = nil) or
     (fValue^ = nil) then
    exit; // nothing to sort
  QuickSort.CompareEvent := aCompare;
  QuickSort.Value := fValue^;
  QuickSort.ElemSize := fInfo.Cache.ItemSize;
  R := Count - 1;
  if aReverse then
    QuickSort.QuickSortEventReverse(0, R)
  else
    QuickSort.QuickSortEvent(0, R);
end;

procedure TDynArray.CreateOrderedIndex(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
  n: integer;
begin
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and
     (fValue <> nil) and
     (fValue^ <> nil) then
  begin
    n := GetCount;
    if length(aIndex) < n then
    begin
      SetLength(aIndex, n);
      FillIncreasing(pointer(aIndex), 0, n);
    end;
    QuickSort.Value := fValue^;
    QuickSort.ElemSize := fInfo.Cache.ItemSize;
    QuickSort.Index := pointer(aIndex);
    QuickSort.QuickSortIndexed(0, n - 1);
  end;
end;

procedure TDynArray.CreateOrderedIndex(out aIndex: TSynTempBuffer;
  aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
  n: integer;
begin
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and
     (fValue <> nil) and
     (fValue^ <> nil) then
  begin
    n := GetCount;
    QuickSort.Value := fValue^;
    QuickSort.ElemSize := fInfo.Cache.ItemSize;
    QuickSort.Index := PCardinalArray(aIndex.InitIncreasing(n));
    QuickSort.QuickSortIndexed(0, n - 1);
  end
  else
    aIndex.buf := nil; // avoid GPF in aIndex.Done
end;

procedure TDynArray.CreateOrderedIndexAfterAdd(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
var
  ndx: integer;
begin
  ndx := GetCount - 1;
  if ndx < 0 then
    exit;
  if aIndex <> nil then
  begin
    // whole FillIncreasing(aIndex[]) for first time
    if ndx >= length(aIndex) then
      SetLength(aIndex, NextGrow(ndx)); // grow aIndex[] if needed
    aIndex[ndx] := ndx;
  end;
  CreateOrderedIndex(aIndex, aCompare);
end;

procedure TDynArray.InitFrom(aAnother: PDynArray; var aValue);
begin
  self := aAnother^; // raw RTTI fields copy
  fValue := @aValue; // points to the new value
  fCountP := nil;
end;

procedure TDynArray.AddDynArray(aSource: PDynArray;
  aStartIndex: integer; aCount: integer);
var
  SourceCount: integer;
begin
  if (aSource <> nil) and
     (aSource^.fValue <> nil) and
     (fInfo.Cache.ItemInfo = aSource^.Info.Cache.ItemInfo) then
  begin
    // check supplied aCount paramter with (external) Source.Count
    SourceCount := aSource^.Count;
    if (aCount < 0) or
       (aCount > SourceCount) then
      aCount := SourceCount;
    // actually add the items
    AddArray(aSource.fValue^, aStartIndex, aCount);
  end;
end;

function TDynArray.Equals(B: PDynArray; IgnoreCompare, CaseSensitive: boolean): boolean;
begin
  result := Compares(B, IgnoreCompare, CaseSensitive) = 0;
end;

function TDynArray.Compares(B: PDynArray; IgnoreCompare, CaseSensitive: boolean): integer;
var
  i, n: integer;
  s: PtrUInt;
  P1, P2: PAnsiChar;
begin
  n := GetCount;
  result := n - B.Count;
  if (result = 0) and
     (n <> 0) then
    if fInfo.Cache.ItemInfo <> B.Info.Cache.ItemInfo then
      result := ComparePointer(fValue^, B.fValue^)
    else if Assigned(fCompare) and
       not ignorecompare then
    begin
      // use specified fCompare() function
      P1 := fValue^;
      P2 := B.fValue^;
      s := fInfo.Cache.ItemSize;
      for i := 1 to n do
      begin
        result := fCompare(P1^, P2^);
        if result <> 0 then
          exit;
        inc(P1, s);
        inc(P2, s);
      end;
    end
    else if rcfObjArray in fInfo.Flags then
      // T*ObjArray comparison of published properties
      result := ObjectCompare(fValue^, B.fValue^, n, not CaseSensitive)
    else if not(rcfArrayItemManaged in fInfo.Flags) then
      // binary comparison with length (always CaseSensitive)
      result := MemCmp(fValue^, B.fValue^, n * fInfo.Cache.ItemSize)
    else
      result := BinaryCompare(fValue^, B.fValue^, fInfo.Cache.ItemInfo, n,
        not CaseSensitive);
end;

procedure TDynArray.Copy(Source: PDynArray; ObjArrayByRef: boolean);
begin
  if (fValue = nil) or
     (fInfo.Cache.ItemInfo <> Source.Info.Cache.ItemInfo) then
    exit;
  if not ObjArrayByRef and
     (rcfObjArray in fInfo.Flags) then
    LoadFromJson(pointer(Source.SaveToJson))
  else
  begin
    DynArrayCopy(fValue, Source.fValue, fInfo.Info, Source.fCountP);
    if fCountP <> nil then
      fCountP^ := GetCapacity;
  end;
end;

procedure TDynArray.CopyFrom(const Source; MaxItem: integer; ObjArrayByRef: boolean);
var
  SourceDynArray: TDynArray;
begin
  SourceDynArray.InitRtti(fInfo, pointer(@Source)^);
  SourceDynArray.fCountP := @MaxItem; // would set Count=0 at Init()
  Copy(@SourceDynArray, ObjArrayByRef);
end;

procedure TDynArray.CopyTo(out Dest; ObjArrayByRef: boolean);
var
  DestDynArray: TDynArray;
begin
  DestDynArray.InitRtti(fInfo, Dest);
  DestDynArray.Copy(@self, ObjArrayByRef);
end;

function IndexFind(P, V: PAnsiChar; cmp: TRttiCompare; rtti: PRttiInfo; n: integer): PtrInt;
var
  comp: integer;
begin
  result := 0;
  repeat
    inc(P, cmp(P, V, rtti, comp));
    if comp = 0 then
      exit;
    inc(result);
    dec(n);
  until n = 0;
  result := -1;
end;

function TDynArray.IndexOf(const Item; CaseInSensitive: boolean): PtrInt;
var
  rtti: PRttiInfo;
  cmp: TRttiCompare;
  n: PtrInt;
label
  bin;
begin
  n := GetCount;
  if (n <> 0) and
     (@Item <> nil) then
    if not(rcfArrayItemManaged in fInfo.Flags) then
bin:  result := AnyScanIndex(fValue^, @Item, n, fInfo.Cache.ItemSize)
    else
    begin
      rtti := fInfo.Cache.ItemInfo;
      if rtti = nil then
        goto bin; // unmanaged items
      cmp := RTTI_COMPARE[CaseInSensitive, rtti.Kind];
      if Assigned(cmp) then
        result := IndexFind(fValue^, @Item, cmp, rtti, n)
      else
        goto bin;
    end
  else
    result := -1;
end;

procedure TDynArray.UseExternalCount(aCountPointer: PInteger);
begin
  fCountP := aCountPointer;
end;

procedure TDynArray.Void;
begin
  fValue := nil;
end;

function TDynArray.IsVoid: boolean;
begin
  result := fValue = nil;
end;

procedure TDynArray.InternalSetLength(OldLength, NewLength: PtrUInt);
var
  p: PDynArrayRec;
  NeededSize, minLength: PtrUInt;
begin
  // this method is faster than default System.DynArraySetLength() function
  p := fValue^;
  // check that new array length is not just a finalize in disguise
  if NewLength = 0 then
  begin
    if p <> nil then
    begin
      // FastDynArrayClear() with ObjArray support
      dec(p);
      if (p^.refCnt > 0) and
         DACntDecFree(p^.refCnt) then
      begin
        if (OldLength <> 0) and
           not fNoFinalize then
          if rcfArrayItemManaged in fInfo.Flags then
            FastFinalizeArray(fValue^, fInfo.Cache.ItemInfo, OldLength)
          else if rcfObjArray in fInfo.Flags then
            RawObjectsClear(fValue^, OldLength);
        FreeMem(p);
      end;
      fValue^ := nil;
    end;
    exit;
  end;
  // calculate the needed size of the resulting memory structure on heap
  NeededSize := NewLength * PtrUInt(fInfo.Cache.ItemSize) + SizeOf(TDynArrayRec);
  {$ifdef CPU32}
  if NeededSize > 1 shl 30 then
    // in practice, consider that max workable memory block is 1 GB on 32-bit
    raise EDynArray.CreateUtf8('TDynArray.InternalSetLength(%,%) size concern',
      [fInfo.Name, NewLength]);
  {$endif CPU32}
  // if not shared (refCnt=1), resize; if shared, create copy (not thread safe)
  if p = nil then
  begin
    p := AllocMem(NeededSize); // RTL/OS will return zeroed memory
    OldLength := NewLength;    // no FillcharFast() below
  end
  else
  begin
    dec(p); // p^ = start of heap object
    if p^.refCnt = 1 then
    begin
      // we own the dynamic array instance -> direct reallocation
      if (NewLength < OldLength) and
         not fNoFinalize then
        // reduce array in-place
        if rcfArrayItemManaged in fInfo.Flags then // in trailing items
          FastFinalizeArray(pointer(PAnsiChar(p) + NeededSize),
            fInfo.Cache.ItemInfo, OldLength - NewLength)
        else if rcfObjArray in fInfo.Flags then // FreeAndNil() of resized objects
          RawObjectsClear(pointer(PAnsiChar(p) + NeededSize), OldLength - NewLength);
      ReallocMem(p, NeededSize);
    end
    else
    begin
      // dynamic array already referenced elsewhere -> create our own copy
      minLength := OldLength;
      if minLength > NewLength then
        minLength := NewLength;
      if fInfo.Cache.ItemInfo = nil then // unmanaged items
      begin
        GetMem(p, NeededSize);
        MoveFast(fValue^^, PByteArray(p)[SizeOf(TDynArrayRec)],
          minLength * PtrUInt(fInfo.Cache.ItemSize));
      end
      else
      begin
        p := AllocMem(NeededSize);
        OldLength := NewLength;    // no FillcharFast() below
        CopySeveral(@PByteArray(p)[SizeOf(TDynArrayRec)], fValue^,
          minLength, fInfo.Cache.ItemInfo, fInfo.Cache.ItemSize);
      end;
      // for thread safety, adjust the refcount after data copy
      if fNoFinalize then
        FastDynArrayClear(fValue, nil)
      else // note: rcfObjArray should never appear with refcnt>1
        FastDynArrayClear(fValue, fInfo.Cache.ItemInfo);
    end;
  end;
  // set refCnt=1 and new length to the heap header
  with p^ do
  begin
    refCnt := 1;
    length := NewLength;
  end;
  inc(p); // start of dynamic aray items
  fValue^ := p;
  // reset new allocated items content to zero
  if NewLength > OldLength then
  begin
    minLength := fInfo.Cache.ItemSize;
    OldLength := OldLength * minLength;
    FillCharFast(PAnsiChar(p)[OldLength], NewLength * minLength - OldLength, 0);
  end;
end;

procedure TDynArray.SetCount(aCount: PtrInt);
const
  MINIMUM_SIZE = 64;
var
  oldlen, extcount, arrayptr, capa, delta: PtrInt;
begin
  arrayptr := PtrInt(fValue);
  extcount := PtrInt(fCountP);
  fSorted := false;
  if arrayptr = 0 then
    exit; // avoid GPF if void
  arrayptr := PPtrInt(arrayptr)^;
  if extcount <> 0 then
  begin
    // fCountP^ as external capacity
    oldlen := PInteger(extcount)^;
    delta := aCount - oldlen;
    if delta = 0 then
      exit;
    PInteger(extcount)^ := aCount; // store new length
    if arrayptr <> 0 then
    begin
      // non void array: check new count against existing capacity
      capa := PDALen(arrayptr - _DALEN)^ + _DAOFF;
      if delta > 0 then
      begin
        // size-up - Add() - is handled branchless
        if capa >= aCount then
          exit; // no need to grow
        capa := NextGrow(capa);
        if capa > aCount then
          aCount := capa; // grow by chunks
      end
      else
      // size-down - Delete()
      if (aCount > 0) and
         ((capa <= MINIMUM_SIZE) or
          (capa - aCount < capa shr 3)) then
        // reallocate memory only if worth it (for faster Delete)
        exit;
    end
    else
    begin
      // void array
      if (delta > 0) and
         (aCount < MINIMUM_SIZE) then
        // reserve some minimal (64) items for Add()
        aCount := MINIMUM_SIZE;
    end;
  end
  else
    // no external capacity: use length()
    if arrayptr = 0 then
      oldlen := arrayptr
    else
    begin
      oldlen := PDALen(arrayptr - _DALEN)^ + _DAOFF;
      if oldlen = aCount then
        exit; // InternalSetLength(samecount) would have made a private copy
    end;
  // no external Count, array size-down or array up-grow -> realloc
  InternalSetLength(oldlen, aCount);
end;

procedure TDynArray.SetCapacity(aCapacity: PtrInt);
var
  oldlen, capa: PtrInt;
begin
  if fValue = nil then
    exit;
  capa := GetCapacity;
  if fCountP <> nil then
  begin
    oldlen := fCountP^;
    if oldlen > aCapacity then
      fCountP^ := aCapacity;
  end
  else
    oldlen := capa;
  if capa <> aCapacity then
    InternalSetLength(oldlen, aCapacity);
end;

procedure TDynArray.SetCompare(const aCompare: TDynArraySortCompare);
begin
  if @aCompare <> @fCompare then
  begin
    @fCompare := @aCompare;
    fSorted := false;
  end;
end;

procedure TDynArray.Slice(var Dest; Limit, Offset: cardinal);
var
  n: cardinal;
  dst: TDynArray;
begin
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  if Offset >= n then
    Limit := 0
  else
  begin
    dec(n, Offset);
    if Limit > n  then
      Limit := n;
  end;
  dst.InitRtti(fInfo, Dest);
  dst.SetCapacity(Limit);
  CopySeveral(pointer(Dest),
    @(PByteArray(fValue^)[Offset * cardinal(fInfo.Cache.ItemSize)]),
    Limit, fInfo.Cache.ItemInfo, fInfo.Cache.ItemSize);
end;

procedure TDynArray.SliceAsDynArray(Dest: PPointer; Offset, Limit: integer);
var
  p: PDynArrayRec;
  n: integer;
begin
  if dest^ <> nil then
    FastDynArrayClear(dest, fInfo.Cache.ItemInfo); // reset Dest variable slot
  n := GetCount;
  if Offset < 0 then
  begin
    // ! SliceAsDynArray(DA, -10);  // last Count-10..Count-1 items
    inc(Offset, n);
    if Offset < 0 then
      Offset := 0;
  end;
  if Offset >= n then // also handles n = 0
    exit;
  if (Offset = 0) and
     ((Limit = 0) or
      (Limit >= n)) then
  begin
    // we can return the current dynamic array with proper Copy-On-Write
    p := fValue^;
    if p = nil then
      exit;
    dec(p);
    inc(p^.refCnt); // COW reuse of the existing dynamic array instance
    p^.Length := n; // no memory realloc/copy, just force Capacity=Length=Count
    inc(p);
    dest^ := p;     // assign to Dest variable
  end
  else
  begin
    // ! SliceAsDynArray(DA, 0, 10);  // first 0..9 items
    // ! SliceAsDynArray(DA, 10, 20); // items 10..29 - truncated if Count < 30
    if Limit = 0 then
      // ! SliceAsDynArray(DA);       // all items
      // ! SliceAsDynArray(DA, 10);   // all items excluding the first 0..9
      Limit := n;
    Slice(Dest^, Limit, Offset);
  end;
end;

function TDynArray.AddArray(const DynArrayVar; aStartIndex, aCount: integer): integer;
var
  c, s: PtrInt;
  n: integer;
  PS, PD: pointer;
begin
  result := 0;
  if fValue = nil then
    exit; // avoid GPF if void
  c := PtrInt(DynArrayVar);
  if c <> 0 then
    c := PDALen(c - _DALEN)^ + _DAOFF;
  if aStartIndex >= c then
    exit; // nothing to copy
  if (aCount < 0) or
     (cardinal(aStartIndex + aCount) > cardinal(c)) then
    aCount := c - aStartIndex;
  if aCount <= 0 then
    exit;
  result := aCount;
  n := GetCount;
  SetCount(n + aCount);
  s := fInfo.Cache.ItemSize;
  PS := PAnsiChar(DynArrayVar) + aStartIndex * s;
  PD := PAnsiChar(fValue^) + n * s;
  CopySeveral(PD, PS, aCount, fInfo.Cache.ItemInfo, s);
end;

function TDynArray.ItemLoadMem(Source, SourceMax: PAnsiChar): RawByteString;
begin
  if (Source <> nil) and
     (fInfo.Cache.ItemInfo = nil) then // unmanaged items
    FastSetRawByteString(result, Source, fInfo.Cache.ItemSize)
  else
  begin
    FastSetRawByteString(result, nil, fInfo.Cache.ItemSize);
    FillCharFast(pointer(result)^, fInfo.Cache.ItemSize, 0);
    ItemLoad(Source, pointer(result), SourceMax);
  end;
end;

procedure TDynArray.ItemLoad(Source, SourceMax: PAnsiChar; Item: pointer);
begin
  if Source <> nil then // avoid GPF
    if fInfo.Cache.ItemInfo = nil then
    begin
      if {$ifndef PUREMORMOT2} (SourceMax = nil) or {$endif}
         (Source + fInfo.Cache.ItemSize <= SourceMax) then
        MoveFast(Source^, Item^, fInfo.Cache.ItemSize);
    end
    else
      BinaryLoad(Item, Source, fInfo.Cache.ItemInfo, nil, SourceMax, rkAllTypes);
end;

procedure TDynArray.ItemLoadMemClear(var ItemTemp: RawByteString);
begin
  ItemClear(pointer(ItemTemp));
  ItemTemp := '';
end;

function TDynArray.ItemSave(Item: pointer): RawByteString;
begin
  if fInfo.Cache.ItemInfo = nil then
    FastSetRawByteString(result, Item, fInfo.Cache.ItemSize)
  else
    result := BinarySave(Item, fInfo.Cache.ItemInfo, rkAllTypes);
end;

function TDynArray.ItemLoadFind(Source, SourceMax: PAnsiChar): integer;
var
  tmp: array[0..2047] of byte;
  data: pointer;
begin
  result := -1;
  if (Source = nil) or
     (fInfo.Cache.ItemSize > SizeOf(tmp)) then
    exit;
  if fInfo.Cache.ItemInfo = nil then // nil for unmanaged items
    data := Source
  else
  begin
    FillCharFast(tmp, fInfo.Cache.ItemSize, 0);
    BinaryLoad(@tmp, Source, fInfo.Cache.ItemInfo, nil, SourceMax, rkAllTypes);
    if Source = nil then
      exit;
    data := @tmp;
  end;
  try
    if Assigned(fCompare) then
      result := Find(data^) // use specific comparer
    else
      result := IndexOf(data^); // use RTTI
  finally
    if data = @tmp then
      fInfo.ArrayRtti.ValueFinalize(data);
  end;
end;


{ ************ TDynArrayHasher }

function HashAnsiString(Item: PAnsiChar; Hasher: THasher): cardinal;
begin
  Item := PPointer(Item)^; // passed by reference
  if Item <> nil then
    result := Hasher(0, Item, PStrLen(Item - _STRLEN)^)
  else
    result := 0;
end;

function HashAnsiStringI(Item: PUtf8Char; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  Item := PPointer(Item)^;
  if Item <> nil then
    result := Hasher(0, tmp{%H-},
      UpperCopy255Buf(tmp{%H-}, Item, PStrLen(Item - _STRLEN)^) - {%H-}tmp)
  else
    result := 0;
end;

function HashSynUnicode(Item: PSynUnicode; Hasher: THasher): cardinal;
begin
  if PtrUInt(Item^) <> 0 then
    result := Hasher(0, Pointer(Item^), Length(Item^) * 2)
  else
    result := 0;
end;

function HashSynUnicodeI(Item: PSynUnicode; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Item^) <> 0 then
    result := Hasher(0, tmp{%H-}, UpperCopy255W(tmp{%H-}, Item^) - {%H-}tmp)
  else
    result := 0;
end;

function HashWideString(Item: PWideString; Hasher: THasher): cardinal;
begin
  // WideString internal size is in bytes, not WideChar
  if PtrUInt(Item^) <> 0 then
    result := Hasher(0, Pointer(Item^), Length(Item^) * 2)
  else
    result := 0;
end;

function HashWideStringI(Item: PWideString; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Item^) <> 0 then
    result := Hasher(0, tmp{%H-},
      UpperCopy255W(tmp{%H-}, pointer(Item^), Length(Item^)) - {%H-}tmp)
  else
    result := 0;
end;

function HashPUtf8Char(Item: PAnsiChar; Hasher: THasher): cardinal;
begin
  Item := PPointer(Item)^; // passed by reference
  if Item <> nil then
    result := Hasher(0, Item, StrLen(Item))
  else
    result := 0;
end;

function HashPUtf8CharI(Item: PUtf8Char; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  Item := PPointer(Item)^;
  if Item <> nil then
    result := Hasher(0, tmp{%H-},
      UpperCopy255Buf(tmp{%H-}, Item, StrLen(Item)) - {%H-}tmp)
  else
    result := 0;
end;

function HashByte(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(byte));
end;

function HashWord(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(word));
end;

function HashInteger(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(integer));
end;

function HashInt64(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(Int64));
end;

function HashExtended(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(TSynExtended));
end;

function Hash128(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash128));
end;

function Hash256(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash256));
end;

function Hash512(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash512));
end;

function VariantHash(const value: variant; CaseInsensitive: boolean;
  Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid heap allocation
  vt: cardinal;
  S: TStream;
  W: TTextWriter;
  P: pointer;
  len: integer;
begin
  if not Assigned(Hasher) then
    Hasher := DefaultHasher;
  with TVarData(value) do
  begin
    vt := VType;
    P := @VByte; // same address than VWord/VInteger/VInt64...
    case vt of
      varNull, varEmpty:
        len := 0; // good enough for void values
      varShortInt, varByte:
        len := 1;
      varSmallint, varWord, varboolean:
        len := 2;
      varLongWord, varInteger, varSingle:
        len := 4;
      varInt64, varDouble, varDate, varCurrency, varWord64:
        len := 8;
      varString:
        begin
          len := length(RawUtf8(VAny));
          P := VAny;
        end;
      varOleStr:
        begin
          len := length(WideString(VAny));
          P := VAny;
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        begin
          len := length(UnicodeString(VAny));
          P := VAny;
        end;
      {$endif HASVARUSTRING}
      else
      begin
        S := TFakeWriterStream.Create;
        W := DefaultJsonWriter.Create(S, @tmp, SizeOf(tmp));
        try
          W.AddVariant(value, twJsonEscape);
          len := W.WrittenBytes;
          if len > 255 then
            len := 255;
          P := @tmp; // big JSON won't be hasheable anyway -> use only buffer
        finally
          W.Free;
          S.Free;
        end;
      end;
    end;
    if CaseInsensitive and
       (P <> @VByte) then
    begin
      len := UpperCopy255Buf(tmp, P, len) - tmp;
      P := @tmp;
    end;
    result := Hasher(vt, P, len)
  end;
end;

function HashVariant(Item: PVariant; Hasher: THasher): cardinal;
begin
  result := VariantHash(Item^, false, Hasher);
end;

function HashVariantI(Item: PVariant; Hasher: THasher): cardinal;
begin
  result := VariantHash(Item^, true, Hasher);
end;

const
  // helper arrays to get the standard hash functions
  PT_HASH: array[{caseinsensitive=}boolean, TRttiParserType] of pointer = (
   // case sensitive hash functions:
   (nil,                     //  ptNone
    nil,                     //  ptArray
    @HashByte,               //  ptBoolean
    @HashByte,               //  ptByte
    @HashInteger,            //  ptCardinal
    @HashInt64,              //  ptCurrency
    @HashInt64,              //  ptDouble
    @HashExtended,           //  ptExtended
    @HashInt64,              //  ptInt64
    @HashInteger,            //  ptInteger
    @HashInt64,              //  ptQWord
    @HashAnsiString,         //  ptRawByteString
    @HashAnsiString,         //  ptRawJson
    @HashAnsiString,         //  ptRawUtf8
    nil,                     //  ptRecord
    @HashInteger,            //  ptSingle
    {$ifdef UNICODE} @HashSynUnicode {$else} @HashAnsiString {$endif}, //  ptString
    @HashSynUnicode,         //  ptSynUnicode
    @HashInt64,              //  ptDateTime
    @HashInt64,              //  ptDateTimeMS
    @Hash128,                //  ptGuid
    @Hash128,                //  ptHash128
    @Hash256,                //  ptHash256
    @Hash512,                //  ptHash512
    @HashInt64,              //  ptOrm
    @HashInt64,              //  ptTimeLog
    @HashSynUnicode,         //  ptUnicodeString
    @HashInt64,              //  ptUnixTime
    @HashInt64,              //  ptUnixMSTime
    @HashVariant,            //  ptVariant
    @HashWideString,         //  ptWideString
    @HashAnsiString,         //  ptWinAnsi
    @HashWord,               //  ptWord
    nil,                     //  ptEnumeration
    nil,                     //  ptSet
    {$ifdef CPU32} @HashInteger {$else} @HashInt64 {$endif}, // ptClass
    nil,                     //  ptDynArray
    {$ifdef CPU32} @HashInteger {$else} @HashInt64 {$endif}, // ptInterface
    @HashPUtf8Char,          //  ptPUtf8Char
    nil),                    //  ptCustom
   // case insensitive hash functions:
   (nil,                     //  ptNone
    nil,                     //  ptArray
    @HashByte,               //  ptBoolean
    @HashByte,               //  ptByte
    @HashInteger,            //  ptCardinal
    @HashInt64,              //  ptCurrency
    @HashInt64,              //  ptDouble
    @HashExtended,           //  ptExtended
    @HashInt64,              //  ptInt64
    @HashInteger,            //  ptInteger
    @HashInt64,              //  ptQWord
    @HashAnsiString,         //  ptRawByteString
    @HashAnsiStringI,        //  ptRawJson
    @HashAnsiStringI,        //  ptRawUtf8
    nil,                     //  ptRecord
    @HashInteger,            //  ptSingle
    {$ifdef UNICODE} @HashSynUnicodeI {$else} @HashAnsiStringI {$endif}, //  ptString
    @HashSynUnicodeI,        //  ptSynUnicode
    @HashInt64,              //  ptDateTime
    @HashInt64,              //  ptDateTimeMS
    @Hash128,                //  ptGuid
    @Hash128,                //  ptHash128
    @Hash256,                //  ptHash256
    @Hash512,                //  ptHash512
    @HashInt64,              //  ptOrm
    @HashInt64,              //  ptTimeLog
    @HashSynUnicodeI,        //  ptUnicodeString
    @HashInt64,              //  ptUnixTime
    @HashInt64,              //  ptUnixMSTime
    @HashVariantI,           //  ptVariant
    @HashWideStringI,        //  ptWideString
    @HashAnsiStringI,        //  ptWinAnsi
    @HashWord,               //  ptWord
    nil,                     //  ptEnumeration
    nil,                     //  ptSet
    {$ifdef CPU32} @HashInteger {$else} @HashInt64 {$endif}, // ptClass
    nil,                     //  ptDynArray
    {$ifdef CPU32} @HashInteger {$else} @HashInt64 {$endif}, // ptInterface
    @HashPUtf8CharI,         //  ptPUtf8Char
    nil));                   //  ptCustom

function DynArrayHashOne(Kind: TRttiParserType;
  CaseInsensitive: boolean): TDynArrayHashOne;
begin
  result := PT_HASH[CaseInsensitive, Kind];
end;

procedure TDynArrayHasher.Init(aDynArray: PDynArray; aHashItem: TDynArrayHashOne;
  const aEventHash: TOnDynArrayHashOne; aHasher: THasher;
  aCompare: TDynArraySortCompare; const aEventCompare: TOnDynArraySortCompare;
  aCaseInsensitive: boolean);
begin
  fDynArray := aDynArray;
  fHashItem := aHashItem;
  fEventHash := aEventHash;
  if not (Assigned(fHashItem) or
          Assigned(fEventHash)) then
  begin
    fHashItem := PT_HASH[aCaseInsensitive, fDynArray^.Info.ArrayFirstField];
    if not Assigned(fHashItem) then
      fEventHash := fDynArray^.Info.ValueFullHash;
  end;
  fCompare := aCompare;
  fEventCompare := aEventCompare;
  if not (Assigned(fCompare) or
          Assigned(fEventCompare)) then
  begin
    fCompare := PT_SORT[aCaseInsensitive, fDynArray^.Info.ArrayFirstField];
    if not Assigned(fCompare) then
      fEventCompare := fDynArray^.Info.ValueFullCompare;
  end;
  HashTableInit(aHasher);
end;

procedure TDynArrayHasher.InitSpecific(aDynArray: PDynArray;
  aKind: TRttiParserType; aCaseInsensitive: boolean; aHasher: THasher);
begin
  fDynArray := aDynArray;
  fHashItem := PT_HASH[aCaseInsensitive, aKind];
  if Assigned(fHashItem) then
    fEventHash := nil
  else
    fEventHash := aDynArray^.Info.ValueFullHash;
  fCompare := PT_SORT[aCaseInsensitive, aKind];
  if Assigned(fCompare) then
    fEventCompare := nil
  else
    fEventCompare := aDynArray^.Info.ValueFullCompare;
  HashTableInit(aHasher);
end;

procedure TDynArrayHasher.HashTableInit(aHasher: THasher);
begin
  if not Assigned(aHasher) then
    aHasher := DefaultHasher;
  fHasher := aHasher;
  fHashTableStore := nil;
  if (Assigned(fHashItem) or
      Assigned(fEventHash)) and
     (Assigned(fCompare) or
      Assigned(fEventCompare)) then
  begin
    // same logic than ReHash(true) with no data
    fHashTableSize := 256;
    {$ifdef DYNARRAYHASH_16BIT}
    SetLength(fHashTableStore, 128 {$ifndef DYNARRAYHASH_PO2} + 1 {$endif});
    fState := [hasHasher, hash16bit];
    {$else}
    SetLength(fHashTableStore, 256);
    byte(State) := 1 shl ord(hasHasher)
    {$endif DYNARRAYHASH_16BIT}
  end
  else
    byte(fState) := 0;
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  CountCollisions := 0;
  CountCollisionsCurrent := 0;
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
end;

procedure TDynArrayHasher.SetEventCompare(const Value: TOnDynArraySortCompare);
begin
  if fDynArray^.GetCount <> 0 then
    raise EDynArray.Create('TDynArrayHasher: unexpected SetEventCompare');
  fEventCompare := Value;
  HashTableInit(fHasher);
end;

procedure TDynArrayHasher.SetEventHash(const Value: TOnDynArrayHashOne);
begin
  if fDynArray^.GetCount <> 0 then
    raise EDynArray.Create('TDynArrayHasher: unexpected SetEventHash');
  fEventHash := Value;
  HashTableInit(fHasher);
end;

function TDynArrayHasher.HashOne(Item: pointer): cardinal;
begin
  if Assigned(fEventHash) then
    result := fEventHash(Item^)
  else if not Assigned(fHashItem) then
    result := 0 // will be ignored afterwards for sure
  else
    result := fHashItem(Item^, fHasher);
end;

function TDynArrayHasher.Equals(Item: pointer; ndx: PtrInt): boolean;
begin
  ndx := ndx * fDynArray^.fInfo.Cache.ItemSize;
  inc(ndx, PPtrInt(fDynArray^.Value)^);
  if Assigned(fEventCompare) then
    result := fEventCompare(pointer(ndx)^, Item^) = 0
  else
    result := fCompare(pointer(ndx)^, Item^) = 0;
end;

const
  // reduces memory consumption and enhances distribution at hash table growing
  _PRIMES: array[0..38 {$ifndef DYNARRAYHASH_PO2} + 13 {$endif}] of integer = (
    {$ifndef DYNARRAYHASH_PO2}
    251, 499, 797, 1259, 2011, 3203, 5087, 8089, 12853, 20399, 81649, 129607, 205759,
    {$endif DYNARRAYHASH_PO2}
    // start after HASH_PO2=2^18=262144 for DYNARRAYHASH_PO2 (poor 64-bit mul)
    326617, 411527, 518509, 653267, 823117, 1037059, 1306601, 1646237,
    2074129, 2613229, 3292489, 4148279, 5226491, 6584983, 8296553, 10453007,
    13169977, 16593127, 20906033, 26339969, 33186281, 41812097, 52679969,
    66372617, 83624237, 105359939, 132745199, 167248483, 210719881, 265490441,
    334496971, 421439783, 530980861, 668993977, 842879579, 1061961721,
    1337987929, 1685759167, 2123923447);

// as used internally by TDynArrayHasher.ForceReHash()
function NextPrime(v: integer): integer; {$ifdef HASINLINE}inline;{$endif}
var
  i: PtrInt;
  P: PIntegerArray;
begin
  P := @_PRIMES;
  for i := 0 to high(_PRIMES) do
  begin
    result := P^[i];
    if result > v then
      exit;
  end;
end;

// see TTestCoreBase._TSynDictionary for some numbers, and why
//  DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2 are defined by default
function TDynArrayHasher.HashTableIndex(aHashCode: PtrUInt): PtrUInt;
begin
  result := fHashTableSize;
  {$ifdef DYNARRAYHASH_PO2}
  // Delphi Win32 e.g. is not efficient with Lemire 64-bit multiplication
  if result <= HASH_PO2 then
    // efficient AND for power of two division
    result := aHashCode and (result - 1)
  else
  {$endif DYNARRAYHASH_PO2}
  {$ifdef DYNARRAYHASH_LEMIRE}
    // FPC or dcc64 compile next line as very optimized asm
    result := (QWord(aHashCode) * result) shr 32;
    // https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction
  {$else}
    // regular 32-bit modulo over a Prime: slower but best from our tests
    result := aHashCode mod result;
  {$endif DYNARRAYHASH_LEMIRE}
end;

function TDynArrayHasher.HashTableIndexToIndex(aHashTableIndex: PtrInt): PtrInt;
begin
  result := PtrUInt(fHashTableStore);
  {$ifdef DYNARRAYHASH_16BIT}
  if hash16bit in fState then
    result := PWordArray(result)[aHashTableIndex]
  else
  {$endif DYNARRAYHASH_16BIT}
    result := PIntegerArray(result)[aHashTableIndex];
end;

function TDynArrayHasher.Find(aHashCode: cardinal; aForAdd: boolean): PtrInt;
var
  first, last, ndx, siz: PtrInt;
  P: PAnsiChar;
begin
  if not (hasHasher in fState) then
  begin
    result := -1;
    exit;
  end;
  result := HashTableIndex(aHashCode);
  first := result;
  last := fHashTableSize;
  P := fDynArray^.Value^;
  siz := fDynArray^.Info.Cache.ItemSize;
  repeat
    ndx := HashTableIndexToIndex(result) - 1; // index+1 was stored
    if ndx < 0 then
    begin
      // found void entry
      result := -(result + 1);
      exit;
    end
    else if not aForAdd and
            (HashOne(P + ndx * siz) = aHashCode) then
    begin
      result := ndx;
      exit;
    end;
    inc(result); // try next entry on hash collision
    if result = last then
      // reached the end -> search once from HashTable[0] to HashTable[first-1]
      if result = first then
        break
      else
      begin
        result := 0;
        last := first;
      end;
  until false;
  RaiseFatalCollision('Find', aHashCode);
end;

function TDynArrayHasher.FindOrNew(aHashCode: cardinal; Item: pointer;
  aHashTableIndex: PPtrInt): PtrInt;
var
  first, last, ndx: PtrInt;
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  collisions: integer;
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
  P: PAnsiChar;
begin
  if not (hasHasher in fState) then
  begin
    result := -1;
    exit; // we need comparison and hash functions
  end;
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  collisions := 0;
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
  result := HashTableIndex(aHashCode);
  first := result;
  last := fHashTableSize;
  repeat
    ndx := HashTableIndexToIndex(result) - 1; // index+1 was stored
    if ndx < 0 then
    begin
      // not found: returns void index in HashTable[] as negative value
      result := - (result + 1);
      {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
      inc(CountCollisions, collisions);
      inc(CountCollisionsCurrent, collisions);
      {$endif DYNARRAYHASHCOLLISIONCOUNT}
      exit;
    end;
    // comparison with item is faster than hash e.g. for huge strings
    with fDynArray^ do
      P := PAnsiChar(Value^) + ndx * fInfo.Cache.ItemSize;
    if ((not Assigned(fEventCompare)) and
        (fCompare(P^, Item^) = 0)) or
       (Assigned(fEventCompare) and
        (fEventCompare(P^, Item^) = 0)) then
    begin
      // found: returns the matching index
      if aHashTableIndex <> nil then
        aHashTableIndex^ := result;
      result := ndx;
      exit;
    end;
    // hash or slot collision -> search next item
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    inc(collisions);
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    inc(result);
    if result = last then
      // reached the end -> search once from HashTable[0] to HashTable[first-1]
      if result = first then
        break
      else
      begin
        result := 0;
        last := first;
      end;
  until false;
  RaiseFatalCollision('FindOrNew', aHashCode);
end;

function TDynArrayHasher.FindOrNewComp(aHashCode: cardinal; Item: pointer;
  Comp: TDynArraySortCompare): PtrInt;
var
  first, last, ndx: PtrInt;
begin // cut-down version of FindOrNew()
  if not Assigned(Comp) then
    Comp := fCompare;
  ndx := HashTableIndex(aHashCode);
  first := ndx;
  last := fHashTableSize;
  if hasHasher in fState then
    repeat
      result := HashTableIndexToIndex(ndx) - 1; // index+1 was stored
      if (result < 0) or // void slot = not found, or return matching index
         (Comp((PAnsiChar(fDynArray^.Value^) +
           result * fDynArray^.fInfo.Cache.ItemSize)^, Item^) = 0) then
        exit;
      inc(ndx); // hash or slot collision -> search next item
      if ndx = last then
        if ndx= first then
          break
        else
        begin
          ndx := 0;
          last := first;
        end;
    until false;
  result := 0; // make compiler happy
  RaiseFatalCollision('FindOrNewComp', aHashCode);
end;

procedure TDynArrayHasher.HashAdd(aHashCode: cardinal; var result: PtrInt);
var
  n, ndx: PtrInt;
begin
  // on input: HashTable[result] slot is already computed
  n := fDynArray^.Count;
  ndx := result;
  result := n;
  if fHashTableSize < n then
    RaiseFatalCollision('HashAdd HashTableSize', aHashCode);
  if fHashTableSize - n < n shr 2 then
  begin
    // grow hash table when 25% void (192/256,384/512,768/1024,1536/2048...)
    ForceReHash;
    ndx := Find(aHashCode, {foradd=}true); // recompute position
    if ndx >= 0 then
      RaiseFatalCollision('HashAdd', aHashCode);
  end;
  ndx := -ndx - 1; // store Index+1 (0 means void slot)
  inc(n);
  {$ifdef DYNARRAYHASH_16BIT}
  if hash16bit in fState then
    PWordArray(fHashTableStore)[ndx] := n
  else
  {$endif DYNARRAYHASH_16BIT}
    fHashTableStore[ndx] := n;
end; // on output: result holds the position in fValue[]

procedure TDynArrayHasher.HashDelete(aArrayIndex, aHashTableIndex: PtrInt;
  aHashCode: cardinal);
var
  first, next, last, n, s, ndx, i: PtrInt;
  P: PAnsiChar;
  indexes: array[0..511] of integer; // to be rehashed  (seen always < 32)
begin
  // retrieve hash table entries to be recomputed
  first := aHashTableIndex;
  last := fHashTableSize;
  next := first;
  n := 0;
  repeat
    {$ifdef DYNARRAYHASH_16BIT}
    if hash16bit in fState then
      PWordArray(fHashTableStore)[next] := 0
    else
    {$endif DYNARRAYHASH_16BIT}
      fHashTableStore[next] := 0; // Clear slots
    inc(next);
    if next = last then
      if next = first then
        RaiseFatalCollision('HashDelete down', aHashCode)
      else
      begin
        next := 0;
        last := first;
      end;
    ndx := HashTableIndexToIndex(next) - 1; // index+1 was stored
    if ndx < 0 then
      break; // stop at void entry
    if n = high(indexes) then // paranoid (typical 0..23 range)
      RaiseFatalCollision('HashDelete indexes[] overflow', aHashCode);
    indexes[n] := ndx;
    inc(n);
  until false;
  // ReHash collided entries - note: item is not yet deleted in Value^[]
  s := fDynArray^.Info.Cache.ItemSize;
  for i := 0 to n - 1 do
  begin
    P := PAnsiChar(fDynArray^.Value^) + {%H-}indexes[i] * s;
    ndx := FindOrNew(HashOne(P), P, nil);
    if ndx < 0 then // ignore ndx>=0 dups (like ReHash)
    begin
      ndx := -ndx - 1;     // compute the new slot position
      n := indexes[i] + 1; // store index+1
      {$ifdef DYNARRAYHASH_16BIT}
      if hash16bit in fState then
        PWordArray(fHashTableStore)[ndx] := n
      else
      {$endif DYNARRAYHASH_16BIT}
        fHashTableStore[ndx] := n;
    end;
  end;
  // adjust all stored indexes (using SSE2/AVX2 on x86_64)
  {$ifdef DYNARRAYHASH_16BIT}
  if hash16bit in fState then
    DynArrayHashTableAdjust16(pointer(fHashTableStore), aArrayIndex, fHashTableSize)
  else
  {$endif DYNARRAYHASH_16BIT}
    DynArrayHashTableAdjust(pointer(fHashTableStore), aArrayIndex, fHashTableSize);
end;

function TDynArrayHasher.FindBeforeAdd(Item: pointer; out wasAdded: boolean;
  aHashCode: cardinal): PtrInt;
begin
  wasAdded := false;
  if hasHasher in fState then
  begin
    result := FindOrNew(aHashCode, Item, nil);
    if result >= 0 then
      exit;
    // found no matching item
    wasAdded := true;
    HashAdd(aHashCode, result);
  end
  else
    result := -1
end;

function TDynArrayHasher.FindBeforeDelete(Item: pointer): PtrInt;
var
  h: cardinal;
  ndx: PtrInt;
begin
  if hasHasher in fState then
  begin
    h := HashOne(Item);
    result := FindOrNew(h, Item, @ndx);
    if result < 0 then
      result := -1
    else
      HashDelete(result, ndx, h);
  end
  else
    result := -1;
end;

procedure TDynArrayHasher.RaiseFatalCollision(const caller: shortstring;
  aHashCode: cardinal);
begin
  // a dedicated sub-procedure reduces code size
  raise EDynArray.CreateUtf8('TDynArrayHasher.% fatal collision: ' +
    'aHashCode=% HashTableSize=% Count=% Capacity=% Array=% Parser=%',
    [caller, CardinalToHexShort(aHashCode), fHashTableSize, fDynArray^.Count,
     fDynArray^.Capacity, fDynArray^.Info.Name, ToText(fDynArray^.Info.Parser)^]);
end;

function TDynArrayHasher.GetHashFromIndex(aIndex: PtrInt): cardinal;
var
  P: pointer;
begin
  P := fDynArray^.ItemPtr(aIndex);
  if P <> nil then
    result := HashOne(P)
  else
    result := 0;
end;

function TDynArrayHasher.Scan(Item: pointer): PtrInt;
var
  P: PAnsiChar;
  i, max, siz: PtrInt;
begin
  result := -1;
  max := fDynArray^.Count - 1;
  P := fDynArray^.Value^;
  siz := fDynArray^.Info.Cache.ItemSize;
  if Assigned(fEventCompare) then // custom comparison
    for i := 0 to max do
      if fEventCompare(P^, Item^) = 0 then
      begin
        result := i;
        break;
      end
      else
        inc(P, siz)
  else if Assigned(fCompare) then
    for i := 0 to max do
      if fCompare(P^, Item^) = 0 then
      begin
        result := i;
        break;
      end
      else
        inc(P, siz)
  else
    exit;
end;

function TDynArrayHasher.Find(Item: pointer): PtrInt;
begin
  result := Find(Item, HashOne(Item));
end;

function TDynArrayHasher.Find(Item: pointer; aHashCode: cardinal): PtrInt;
begin
  result := FindOrNew(aHashCode, Item, nil); // fallback to Scan() if needed
  if result < 0 then
    result := -1; // for coherency with most search methods
end;

type
  {$ifdef USERECORDWITHMETHODS}
  TFastReHash = record
  {$else}
  TFastReHash = object // dedicated object for better register allocation
  {$endif USERECORDWITHMETHODS}
  public
    hc: cardinal;
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    collisions: integer;
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    ht: integer;
    values, first, last, siz: PtrInt;
    duplicates: PInteger;
    P: PAnsiChar;
    // fill fHashTableStore[] from all stored items
    procedure Process(Hasher: PDynArrayHasher; count: PtrInt);
  end;

procedure TFastReHash.Process(Hasher: PDynArrayHasher; count: PtrInt);
var
  fnd, ndx: PtrInt;
label
  s;
begin
  // should match FindOrNew() logic
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  collisions := 0;
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
  P := Hasher^.fDynArray^.Value^;
  values := PtrUInt(P);
  siz := Hasher^.fDynArray^.Info.Cache.ItemSize;
  ht := 1; // store index + 1
  repeat
s:  if Assigned(Hasher^.fEventHash) then // inlined HashOne()
      hc := Hasher^.fEventHash(P^)
    else
      hc := Hasher^.fHashItem(P^, Hasher^.fHasher);
    ndx := Hasher^.HashTableIndex(hc);
    first := ndx;
    last := Hasher^.fHashTableSize;
    repeat
      {$ifdef DYNARRAYHASH_16BIT} // inlined HashTableIndexToIndex()
      if hash16bit in Hasher^.fState then
      begin
        if PWordArray(Hasher^.fHashTableStore)[ndx] = 0 then // store index + 1
        begin
          // we can use this void entry (most common case)
          PWordArray(Hasher^.fHashTableStore)[ndx] := ht;
          inc(P, siz); // next item
          inc(ht);
          dec(count);
          if count <> 0 then
            goto s;
          exit;
        end;
      end
      else
      {$endif DYNARRAYHASH_16BIT}
      if Hasher^.fHashTableStore[ndx] = 0 then // void entry
      begin
        Hasher^.fHashTableStore[ndx] := ht;
        inc(P, siz); // next item
        inc(ht);
        dec(count);
        if count <> 0 then
          goto s;
        exit;
      end;
      {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
      inc(collisions);
      {$endif DYNARRAYHASHCOLLISIONCOUNT}
      if duplicates <> nil then
      begin
        // check for duplicated values only if necessary (slow down process)
        if hash16bit in Hasher^.fState then
          fnd := PWordArray(Hasher^.fHashTableStore)[ndx]
        else
          fnd := Hasher^.fHashTableStore[ndx];
        fnd := values + (fnd - 1) * siz; // stored index + 1
        if ((not Assigned(Hasher^.fEventCompare)) and
            (Hasher^.fCompare(pointer(fnd)^, P^) = 0)) or
           (Assigned(Hasher^.fEventCompare) and
            (Hasher^.fEventCompare(pointer(fnd)^, P^) = 0)) then
        begin
          inc(duplicates^); // report but ignore duplicates
          break;
        end;
      end;
      inc(ndx);
      if ndx = last then
        // reached the end -> search from HashTable[0] to HashTable[first-1]
        if ndx = first then
          Hasher.RaiseFatalCollision('ReHash', hc)
        else
        begin
          ndx := 0;
          last := first;
        end;
    until false;
    inc(P, siz); // next item
    inc(ht);
    dec(count);
  until count = 0;
end;

procedure TDynArrayHasher.ForceReHash(duplicates: PInteger);
var
  n, cap, siz: PtrInt;
  fastrehash: TFastReHash;
begin
  if duplicates <> nil then
    duplicates^ := 0;
  if not (hasHasher in fState) then
    exit;
  // Capacity better than Count or HashTableSize, * 2 to reserve some void slots
  cap := fDynArray^.Capacity * 2;
  {$ifdef DYNARRAYHASH_PO2}
  if cap <= HASH_PO2 then
  begin
    siz := 256; // find nearest power of two for fast bitwise division
    while siz < cap do
      siz := siz shl 1;
  end
  else
  {$endif DYNARRAYHASH_PO2}
    siz := NextPrime(cap);
//QueryPerformanceMicroSeconds(t1); write('rehash count=',n,' old=',HashTableSize,
//' new=', siz, ' oldcol=',CountCollisionsCurrent);
  fHashTableStore := nil;
  fHashTableSize := siz;
  {$ifdef DYNARRAYHASH_16BIT}
  if siz <= 1 shl 16 then
  begin
    include(fState, hash16bit); // we can store indexes as 16-bit word values
    siz := (siz shr 1) {$ifndef DYNARRAYHASH_PO2} + 1 {$endif}; // 32-bit count
  end
  else
    exclude(fState, hash16bit);
  {$endif DYNARRAYHASH_16BIT}
  SetLength(fHashTableStore, siz); // fill with 0 (void slot)
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  CountCollisionsCurrent := 0; // count collision for this HashTable[] only
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
  // fill fHashTableStore[]=index+1 from all existing items
  n := fDynArray^.Count;
  if n <> 0 then
  begin
    fastrehash.duplicates := duplicates;
    fastrehash.Process(@self, n);
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    inc(CountCollisions, fastrehash.collisions);
    inc(CountCollisionsCurrent, fastrehash.collisions);
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
  end;
//QueryPerformanceMicroSeconds(t2); writeln(' newcol=',CountCollisionsCurrent,' ',
//(CountCollisionsCurrent * 100) div cardinal(n), '%  ',MicroSecToString(t2-t1));
end;

{$ifndef PUREMORMOT2}
function TDynArrayHasher.ReHash(forced: boolean): integer;
begin
  ForceRehash(@result); // always forced for true thread-safety
end;
{$endif PUREMORMOT2}


{ ************ TDynArrayHashed }

{ TDynArrayHashed }

{$ifdef UNDIRECTDYNARRAY} // some Delphi 2009+ wrapper definitions

function TDynArrayHashed.GetCount: PtrInt;
begin
  result := InternalDynArray.GetCount;
end;

procedure TDynArrayHashed.SetCount(aCount: PtrInt);
begin
  InternalDynArray.SetCount(aCount);
end;

function TDynArrayHashed.GetCapacity: PtrInt;
begin
  result := InternalDynArray.GetCapacity;
end;

procedure TDynArrayHashed.SetCapacity(aCapacity: PtrInt);
begin
  InternalDynArray.SetCapacity(aCapacity);
end;

function TDynArrayHashed.Value: PPointer;
begin
  result := InternalDynArray.fValue;
end;

function TDynArrayHashed.Info: TRttiCustom;
begin
  result := InternalDynArray.fInfo;
end;

function TDynArrayHashed.ItemSize: PtrUInt;
begin
  result := InternalDynArray.fInfo.Cache.ItemSize;
end;

procedure TDynArrayHashed.ItemCopy(Source, Dest: pointer);
begin
  InternalDynArray.ItemCopy(Source, Dest);
end;

function TDynArrayHashed.ItemPtr(index: PtrInt): pointer;
begin
  result := InternalDynArray.ItemPtr(index);
end;

function TDynArrayHashed.ItemCopyAt(index: PtrInt; Dest: pointer): boolean;
begin
  result := InternalDynArray.ItemCopyAt(index, Dest);
end;

procedure TDynArrayHashed.Clear;
begin
  InternalDynArray.SetCount(0);
end;

function TDynArrayHashed.Add(const Item): PtrInt;
begin
  result := InternalDynArray.Add(Item);
end;

procedure TDynArrayHashed.Delete(aIndex: PtrInt);
begin
  InternalDynArray.Delete(aIndex);
end;

function TDynArrayHashed.SaveTo: RawByteString;
begin
  result := InternalDynArray.SaveTo;
end;

function TDynArrayHashed.LoadFrom(Source, SourceMax: PAnsiChar): PAnsiChar;
begin
  result := InternalDynArray.LoadFrom(Source, SourceMax);
end;

function TDynArrayHashed.LoadFromBinary(const Buffer: RawByteString): boolean;
begin
  result := InternalDynArray.LoadFromBinary(Buffer);
end;

procedure TDynArrayHashed.SaveTo(W: TBufferWriter);
begin
  InternalDynArray.SaveTo(W);
end;

procedure TDynArrayHashed.Sort(aCompare: TDynArraySortCompare);
begin
  InternalDynArray.Sort(aCompare);
end;

procedure TDynArrayHashed.CreateOrderedIndex(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
begin
  InternalDynArray.CreateOrderedIndex(aIndex, aCompare);
end;

function TDynArrayHashed.SaveToJson(EnumSetsAsText: boolean;
  reformat: TTextWriterJsonFormat): RawUtf8;
begin
  result := InternalDynArray.SaveToJson(EnumSetsAsText, reformat);
end;

procedure TDynArrayHashed.SaveToJson(out result: RawUtf8; EnumSetsAsText: boolean;
  reformat: TTextWriterJsonFormat);
begin
  InternalDynArray.SaveToJson(result, EnumSetsAsText, reformat);
end;

procedure TDynArrayHashed.SaveToJson(W: TTextWriter);
begin
  InternalDynArray.SaveToJson(W);
end;

function TDynArrayHashed.LoadFromJson(P: PUtf8Char; aEndOfObject: PUtf8Char;
  CustomVariantOptions: PDocVariantOptions): PUtf8Char;
begin
  result := InternalDynArray.LoadFromJson(P, aEndOfObject, CustomVariantOptions);
end;

{$endif UNDIRECTDYNARRAY}

procedure TDynArrayHashed.Init(aTypeInfo: PRttiInfo; var aValue;
  aHashItem: TDynArrayHashOne; aCompare: TDynArraySortCompare;
  aHasher: THasher; aCountPointer: PInteger; aCaseInsensitive: boolean);
begin
  InitRtti(Rtti.RegisterType(aTypeInfo), aValue, aHashItem, aCompare,
    aHasher, aCountPointer, aCaseInsensitive);
end;

procedure TDynArrayHashed.InitRtti(aRtti: TRttiCustom; var aValue;
  aHashItem: TDynArrayHashOne; aCompare: TDynArraySortCompare;
  aHasher: THasher; aCountPointer: PInteger; aCaseInsensitive: boolean);
begin
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$else}inherited{$endif}
    InitRtti(aRtti, aValue, aCountPointer);
  fHash.Init(@self, aHashItem, nil, aHasher, aCompare, nil, aCaseInsensitive);
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}fCompare := fHash.fCompare;
end;

procedure TDynArrayHashed.InitSpecific(aTypeInfo: PRttiInfo; var aValue;
  aKind: TRttiParserType; aCountPointer: PInteger; aCaseInsensitive: boolean;
  aHasher: THasher);
begin
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$else}inherited{$endif}
    Init(aTypeInfo, aValue, aCountPointer);
  fHash.InitSpecific(@self, aKind, aCaseInsensitive, aHasher);
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}fCompare := fHash.fCompare;
end;

function TDynArrayHashed.Scan(const Item): PtrInt;
begin
  result := fHash.Scan(@Item);
end;

function TDynArrayHashed.FindHashed(const Item): PtrInt;
begin
  result := fHash.FindOrNew(fHash.HashOne(@Item), @Item, nil);
  if result < 0 then
    result := -1; // for coherency with most methods
end;

function TDynArrayHashed.FindFromHash(const Item; aHashCode: cardinal): PtrInt;
begin
  // overload FindHashed() trigger F2084 Internal Error: C2130 on Delphi XE3
  result := fHash.FindOrNew(aHashCode, @Item, nil); // fallback to Scan() if needed
  if result < 0 then
    result := -1; // for coherency with most methods
end;

function TDynArrayHashed.FindHashedForAdding(const Item; out wasAdded: boolean;
  noAddEntry: boolean): PtrInt;
begin
  result := FindHashedForAdding(Item, wasAdded, fHash.HashOne(@Item), noAddEntry);
end;

function TDynArrayHashed.FindHashedForAdding(const Item; out wasAdded: boolean;
  aHashCode: cardinal; noAddEntry: boolean): PtrInt;
begin
  result := fHash.FindBeforeAdd(@Item, wasAdded, aHashCode);
  if wasAdded and
     not noAddEntry then
    SetCount(result + 1); // reserve space for a void element in array
end;

function TDynArrayHashed.AddAndMakeUniqueName(aName: RawUtf8): pointer;
var
  ndx: PtrInt;
  j: PtrUInt;
  added: boolean;
  aName_: RawUtf8;
begin
  if aName = '' then
    aName := '_';
  ndx := FindHashedForAdding(aName, added);
  if not added then
  begin
    // force unique column name
    aName_ := aName + '_';
    j := 1;
    repeat
      if j > high(SmallUInt32Utf8) then // should never happen - 999 is enough
        raise EDynArray.Create('TDynArrayHashed.AddAndMakeUniqueName overflow');
      aName := aName_ + SmallUInt32Utf8[j];
      ndx := FindHashedForAdding(aName, added);
      inc(j);
    until added;
  end;
  result := PAnsiChar(Value^) + ndx * Info.Cache.ItemSize;
  PRawUtf8(result)^ := aName; // store unique name at 1st position
end;

function TDynArrayHashed.AddUniqueName(const aName: RawUtf8;
  aNewIndex: PPtrInt): pointer;
begin
  result := AddUniqueName(aName, '', [], aNewIndex);
end;

function TDynArrayHashed.AddUniqueName(const aName: RawUtf8; const ExceptionMsg: RawUtf8;
  const ExceptionArgs: array of const; aNewIndex: PPtrInt): pointer;
var
  ndx: PtrInt;
  added: boolean;
begin
  ndx := FindHashedForAdding(aName, added);
  if added then
  begin
    if aNewIndex <> nil then
      aNewIndex^ := ndx;
    result := PAnsiChar(Value^) + ndx * Info.Cache.ItemSize;
    PRawUtf8(result)^ := aName; // store unique name at 1st position
  end
  else if ExceptionMsg = '' then
    raise EDynArray.CreateUtf8('TDynArrayHashed: Duplicated [%] name', [aName])
  else
    raise EDynArray.CreateUtf8(ExceptionMsg, ExceptionArgs);
end;

function TDynArrayHashed.FindHashedAndFill(var ItemToFill): PtrInt;
begin
  result := fHash.FindOrNew(fHash.HashOne(@ItemToFill), @ItemToFill, nil);
  if result < 0 then
    result := -1
  else
    ItemCopy(PAnsiChar(Value^) + result * Info.Cache.ItemSize, @ItemToFill);
end;

function TDynArrayHashed.FindHashedAndUpdate(const Item;
  AddIfNotExisting: boolean): PtrInt;
var
  hc: cardinal;
begin
  if hasHasher in fHash.fState then
  begin
    hc := fHash.HashOne(@Item);
    result := fHash.FindOrNew(hc, @Item, nil);
    if (result < 0) and
       AddIfNotExisting then
    begin
      fHash.HashAdd(hc, result); // ReHash only if necessary
      SetCount(result + 1); // add new item
    end;
  end
  else
    result := -1;
  if result >= 0 then // update
    ItemCopy(@Item, PAnsiChar(Value^) + result * Info.Cache.ItemSize);
end;

function TDynArrayHashed.FindHashedAndDelete(const Item; FillDeleted: pointer;
  noDeleteEntry: boolean): PtrInt;
begin
  result := fHash.FindBeforeDelete(@Item);
  if result >= 0 then
  begin
    if FillDeleted <> nil then
      ItemCopyAt(result, FillDeleted);
    if not noDeleteEntry then
      Delete(result);
  end;
end;

function TDynArrayHashed.GetHashFromIndex(aIndex: PtrInt): cardinal;
begin
  result := fHash.GetHashFromIndex(aIndex);
end;

procedure TDynArrayHashed.ForceReHash;
begin
  fHash.ForceReHash;
end;

{$ifndef PUREMORMOT2}
function TDynArrayHashed.ReHash(forced: boolean): integer;
begin
  fHash.ForceReHash(@result); // always forced
end;
{$endif PUREMORMOT2}

procedure TDynArrayHashed.SetEventCompare(const cmp: TOnDynArraySortCompare);
begin
  fHash.SetEventCompare(cmp);
end;

procedure TDynArrayHashed.SetEventHash(const hsh: TOnDynArrayHashOne);
begin
  fHash.SetEventHash(hsh);
end;


function DynArray(aTypeInfo: PRttiInfo; var aValue; aCountPointer: PInteger): TDynArray;
begin
  result.Init(aTypeInfo, aValue, aCountPointer);
end;



{ *************** Integer Arrays Extended Process }

procedure Exchg32(var A, B: integer); {$ifdef HASINLINE}inline;{$endif}
var
  tmp: integer;
begin
  tmp := A;
  A := B;
  B := tmp;
end;

function MedianQuickSelectInteger(Values: PIntegerArray; n: integer): integer;
var
  low, high, median, middle, ll, hh: PtrInt;
begin
  if n = 0 then
  begin
    result := 0;
    exit;
  end;
  if n = 1 then
  begin
    result := Values[0];
    exit;
  end;
  low := 0;
  high := n - 1;
  median := high shr 1;
  repeat
    if high <= low then
    begin
      // one item left
      result := Values[median];
      exit;
    end;
    if high = low + 1 then
    begin
      // two items -> return the smallest (not average)
      if Values[low] > Values[high] then
        Exchg32(Values[low], Values[high]);
      result := Values[median];
      exit;
    end;
    // find median of low, middle and high items; swap into position low
    middle := (low + high) shr 1;
    if Values[middle] > Values[high] then
      Exchg32(Values[middle], Values[high]);
    if Values[low] > Values[high] then
      Exchg32(Values[low], Values[high]);
    if Values[middle] > Values[low] then
      Exchg32(Values[middle], Values[low]);
    // swap low item (now in position middle) into position (low+1)
    Exchg32(Values[middle], Values[low + 1]);
    // nibble from each end towards middle, swapping items when stuck
    ll := low + 1;
    hh := high;
    repeat
      repeat
        inc(ll);
      until not (Values[low] > Values[ll]);
      repeat
        dec(hh);
      until not (Values[hh] > Values[low]);
      if hh < ll then
        break;
      Exchg32(Values[ll], Values[hh]);
    until false;
    // swap middle item (in position low) back into correct position
    Exchg32(Values[low], Values[hh]);
    // next active partition
    if hh <= median then
      low := ll;
    if hh >= median then
      high := hh - 1;
  until false;
end;

function MedianQuickSelect(const OnCompare: TOnValueGreater; n: integer;
  var TempBuffer: TSynTempBuffer): integer;
var
  low, high, middle, median, ll, hh: PtrInt;
  tmp: integer;
  ndx: PIntegerArray;
begin
  if n <= 1 then
  begin
    TempBuffer.buf := nil; // avoid GPF in TempBuffer.Done
    result := 0;
    exit;
  end;
  low := 0;
  high := n - 1;
  ndx := TempBuffer.InitIncreasing(n * 4); // no heap alloacation until n>1024
  median := high shr 1;
  repeat
    if high <= low then
    begin
      // one item left
      result := ndx[median];
      {%H-}TempBuffer.Done;
      exit;
    end;
    if high = low + 1 then
    begin
      // two items -> return the smallest (not average)
      if OnCompare(ndx[low], ndx[high]) then
        Exchg32(ndx[low], ndx[high]);
      result := ndx[median];
      {%H-}TempBuffer.Done;
      exit;
    end;
    // find median of low, middle and high items; swap into position low
    middle := (low + high) shr 1;
    if OnCompare(ndx[middle], ndx[high]) then
      Exchg32(ndx[middle], ndx[high]);
    if OnCompare(ndx[low], ndx[high]) then
      Exchg32(ndx[low], ndx[high]);
    if OnCompare(ndx[middle], ndx[low]) then
      Exchg32(ndx[middle], ndx[low]);
    // swap low item (now in position middle) into position (low+1)
    Exchg32(ndx[middle], ndx[low + 1]);
    // nibble from each end towards middle, swapping items when stuck
    ll := low + 1;
    hh := high;
    repeat
      tmp := ndx[low];
      repeat
        inc(ll);
      until not OnCompare(tmp, ndx[ll]);
      repeat
        dec(hh);
      until not OnCompare(ndx[hh], tmp);
      if hh < ll then
        break;
      tmp := ndx[ll];
      ndx[ll] := ndx[hh];
      ndx[hh] := tmp; // Exchg32(ndx[ll],ndx[hh]);
    until false;
    // swap middle item (in position low) back into correct position
    Exchg32(ndx[low], ndx[hh]);
    // next active partition
    if hh <= median then
      low := ll;
    if hh >= median then
      high := hh - 1;
  until false;
end;

procedure NotifySortedIntegerChanges(old, new: PIntegerArray; oldn, newn: PtrInt;
  const added, deleted: TOnNotifySortedIntegerChange; const sender);
var
  o, n: PtrInt;
begin
  o := 0;
  n := 0;
  repeat
    while (n < newn) and
          (o < oldn) and
          (old[o] = new[n]) do
    begin
      inc(o);
      inc(n);
    end;
    while (o < oldn) and
          ((n >= newn) or
           (old[o] < new[n])) do
    begin
      if Assigned(deleted) then
        deleted(sender, old[o]);
      inc(o);
    end;
    while (n < newn) and
          ((o >= oldn) or
           (new[n] < old[o])) do
    begin
      if Assigned(added) then
        added(sender, new[n]);
      inc(n);
    end;
  until (o >= oldn) and
        (n >= newn);
end;

procedure CopyAndSortInteger(Values: PIntegerArray; ValuesCount: integer;
  var Dest: TIntegerDynArray);
begin
  if ValuesCount > Length(Dest) then
    SetLength(Dest, ValuesCount);
  MoveFast(Values^[0], Dest[0], ValuesCount * SizeOf(integer));
  QuickSortInteger(pointer(Dest), 0, ValuesCount - 1);
end;

procedure CopyAndSortInt64(Values: PInt64Array; ValuesCount: integer;
  var Dest: TInt64DynArray);
begin
  if ValuesCount > Length(Dest) then
    SetLength(Dest, ValuesCount);
  MoveFast(Values^[0], Dest[0], ValuesCount * SizeOf(Int64));
  QuickSortInt64(pointer(Dest), 0, ValuesCount - 1);
end;

procedure ExcludeInteger(var Values, Excluded: TIntegerDynArray; ExcludedSortSize: integer);
var
  i, v, x, n: PtrInt;
begin
  if (Values = nil) or
     (Excluded = nil) then
    exit; // nothing to exclude
  EnsureUnique(Values);
  EnsureUnique(Excluded);
  v := Length(Values);
  n := 0;
  x := Length(Excluded);
  if (x > ExcludedSortSize) or
     (v > ExcludedSortSize) then
  begin
    // sort if worth it
    dec(x);
    QuickSortInteger(pointer(Excluded), 0, x);
    for i := 0 to v - 1 do
      if FastFindIntegerSorted(pointer(Excluded), x, Values[i]) < 0 then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  end
  else
    for i := 0 to v - 1 do
      if not IntegerScanExists(pointer(Excluded), x, Values[i]) then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  if n <> v then
    SetLength(Values, n);
end;

procedure IncludeInteger(var Values, Included: TIntegerDynArray; IncludedSortSize: integer);
var
  i, v, x, n: PtrInt;
begin
  if (Values = nil) or
     (Included = nil) then
  begin
    Values := nil;
    exit;
  end;
  EnsureUnique(Values);
  EnsureUnique(Included);
  v := Length(Values);
  n := 0;
  x := Length(Included);
  if (x > IncludedSortSize) or
     (v > IncludedSortSize) then
  begin
    // sort if worth it
    dec(x);
    QuickSortInteger(pointer(Included), 0, x);
    for i := 0 to v - 1 do
      if FastFindIntegerSorted(pointer(Included), x, Values[i]) >= 0 then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  end
  else
    for i := 0 to v - 1 do
      if IntegerScanExists(pointer(Included), x, Values[i]) then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  if n <> v then
    SetLength(Values, n);
end;

procedure ExcludeInt64(var Values, Excluded: TInt64DynArray; ExcludedSortSize: integer);
var
  i, v, x, n: PtrInt;
begin
  if (Values = nil) or
     (Excluded = nil) then
    exit; // nothing to exclude
  v := Length(Values);
  n := 0;
  x := Length(Excluded);
  if (x > ExcludedSortSize) or
     (v > ExcludedSortSize) then
  begin
    // sort if worth it
    dec(x);
    QuickSortInt64(pointer(Excluded), 0, x);
    for i := 0 to v - 1 do
      if FastFindInt64Sorted(pointer(Excluded), x, Values[i]) < 0 then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  end
  else
    for i := 0 to v - 1 do
      if not Int64ScanExists(pointer(Excluded), x, Values[i]) then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  if n <> v then
    SetLength(Values, n);
end;

procedure IncludeInt64(var Values, Included: TInt64DynArray; IncludedSortSize: integer);
var
  i, v, x, n: PtrInt;
begin
  if (Values = nil) or
     (Included = nil) then
  begin
    Values := nil;
    exit;
  end;
  v := Length(Values);
  n := 0;
  x := Length(Included);
  if (x > IncludedSortSize) or
     (v > IncludedSortSize) then
  begin
    // sort if worth it
    dec(x);
    QuickSortInt64(pointer(Included), 0, x);
    for i := 0 to v - 1 do
      if FastFindInt64Sorted(pointer(Included), x, Values[i]) >= 0 then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  end
  else
    for i := 0 to v - 1 do
      if Int64ScanExists(pointer(Included), x, Values[i]) then
      begin
        if n <> i then
          Values[n] := Values[i];
        inc(n);
      end;
  if n <> v then
    SetLength(Values, n);
end;

procedure DeduplicateInteger(var Values: TIntegerDynArray);
begin
  DeduplicateInteger(Values, Length(Values));
end;

function DeduplicateIntegerSorted(val: PIntegerArray; last: PtrInt): PtrInt;
var
  i: PtrInt;
begin
  // sub-function for better code generation
  i := 0;
  repeat // here last>0 so i<last
    if val[i] = val[i + 1] then
      break;
    inc(i);
    if i <> last then
      continue;
    result := i;
    exit;
  until false;
  result := i;
  inc(i);
  if i <> last then
  begin
    repeat
      if val[i] <> val[i + 1] then
      begin
        val[result] := val[i];
        inc(result);
      end;
      inc(i);
    until i = last;
    val[result] := val[i];
  end;
end;

function DeduplicateInteger(var Values: TIntegerDynArray; Count: PtrInt): PtrInt;
begin
  result := Count;
  dec(Count);
  if Count > 0 then
  begin
    QuickSortInteger(pointer(Values), 0, Count);
    result := DeduplicateIntegerSorted(pointer(Values), Count) + 1;
  end;
  if result <> Length(Values) then
    SetLength(Values, result);
end;

procedure DeduplicateInt64(var Values: TInt64DynArray);
begin
  DeduplicateInt64(Values, Length(Values));
end;

function DeduplicateInt64Sorted(val: PInt64Array; last: PtrInt): PtrInt;
var
  i: PtrInt;
begin
  // sub-function for better code generation
  i := 0;
  repeat // here last>0 so i<last
    if val[i] = val[i + 1] then
      break;
    inc(i);
    if i <> last then
      continue;
    result := i;
    exit;
  until false;
  result := i;
  inc(i);
  if i <> last then
  begin
    repeat
      if val[i] <> val[i + 1] then
      begin
        val[result] := val[i];
        inc(result);
      end;
      inc(i);
    until i = last;
    val[result] := val[i];
  end;
end;

function DeduplicateInt64(var Values: TInt64DynArray; Count: PtrInt): PtrInt;
begin
  result := Count;
  dec(Count);
  if Count > 0 then
  begin
    QuickSortInt64(pointer(Values), 0, Count);
    result := DeduplicateInt64Sorted(pointer(Values), Count) + 1;
  end;
  if result <> Length(Values) then
    SetLength(Values, result);
end;

procedure CopyInteger(const Source: TIntegerDynArray; out Dest: TIntegerDynArray);
var
  n: integer;
begin
  n := Length(Source);
  SetLength(Dest, n);
  MoveFast(Source[0], Dest[0], n * SizeOf(integer));
end;

procedure CopyInt64(const Source: TInt64DynArray; out Dest: TInt64DynArray);
var
  n: integer;
begin
  n := Length(Source);
  SetLength(Dest, n);
  MoveFast(Source[0], Dest[0], n * SizeOf(Int64));
end;

function MaxInteger(const Values: TIntegerDynArray; ValuesCount: PtrInt; MaxStart: integer): integer;
var
  i: PtrInt;
  v: integer;
begin
  result := MaxStart;
  for i := 0 to ValuesCount - 1 do
  begin
    v := Values[i];
    if v > result then
      result := v; // movca branchless opcode on FPC
  end;
end;

function SumInteger(const Values: TIntegerDynArray; ValuesCount: PtrInt): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to ValuesCount - 1 do
    inc(result, Values[i]);
end;

procedure Reverse(const Values: TIntegerDynArray; ValuesCount: PtrInt; Reversed: PIntegerArray);
var
  i: PtrInt;
begin
  i := 0;
  if ValuesCount >= 4 then
  begin
    dec(ValuesCount, 4);
    while i < ValuesCount do
    begin
      // faster pipelined version
      Reversed[Values[i]] := i;
      Reversed[Values[i + 1]] := i + 1;
      Reversed[Values[i + 2]] := i + 2;
      Reversed[Values[i + 3]] := i + 3;
      inc(i, 4);
    end;
    inc(ValuesCount, 4);
  end;
  while i < ValuesCount do
  begin
    Reversed[Values[i]] := i;
    inc(i);
  end;
  //for i := 0 to Count-1 do Assert(Reverse[Orig[i]]=i);
end;

procedure Int64ToUInt32(Values64: PInt64Array; Values32: PCardinalArray; Count: PtrInt);
var
  i: PtrInt;
begin
  for i := 0 to Count - 1 do
    Values32[i] := Values64[i];
end;

function AnyScanIndex(P, V: pointer; Count, VSize: PtrInt): PtrInt;
begin
  case VSize of
    // optimized versions for arrays of most simple types
    1:
      result := ByteScanIndex(P, Count, PByte(V)^); // SSE2 asm on Intel/AMD
    2:
      result := WordScanIndex(P, Count, PWord(V)^); // may use SSE2 asm
    4:
      result := IntegerScanIndex(P, Count, PInteger(V)^); // may use SSE2 asm
    8:
      result := Int64ScanIndex(P, Count, PInt64(V)^);
    SizeOf(THash128):
      result := Hash128Index(P, Count, V);
    SizeOf(THash256):
      result := Hash256Index(P, Count, V);
    // small VSize version (<SizeOf(PtrInt))
    3, 5..7:
      begin
        for result := 0 to Count - 1 do
          if CompareMemSmall(P, V, VSize) then
            exit
          else
            inc(PByte(P), VSize);
        result := -1;
      end;
  else
    begin
      // generic binary comparison (fast with inlined CompareMemSmall)
      for result := 0 to Count - 1 do
        if (PInt64(P)^ = PInt64(V)^) and // not better using a local Int64 var
           CompareMemSmall(PAnsiChar(P) + 8, PAnsiChar(V) + 8, VSize - 8) then
          exit
        else
          inc(PByte(P), VSize);
      result := -1;
    end;
  end;
end;

function AnyScanExists(P, V: pointer; Count, VSize: PtrInt): boolean;
begin
  result := AnyScanIndex(P, V, Count, VSize) >= 0;
end;


{ ************ Abstract Radix Tree Classes }

{ TRadixTreeNode }

function TRadixTreeNode.ComputeDepth: integer;
var
  i: PtrInt;
begin
  result := 1;
  for i := 0 to high(Child) do
    inc(result, Child[i].ComputeDepth); // recursive calculation
  Depth := result;
end;

function RadixTreeNodeCompare(const A, B): integer;
begin // sort static first, then deeper, by path:, by longest path, by text
  result := ord(TRadixTreeNode(B).Chars[1] <> '<') -
            ord(TRadixTreeNode(A).Chars[1] <> '<');
  if result = 0 then
    result := ord(IdemPChar(pointer(TRadixTreeNode(A).Chars), '<PATH:')) -
              ord(IdemPChar(pointer(TRadixTreeNode(B).Chars), '<PATH:'));
  if result = 0 then
    result := CompareInteger(TRadixTreeNode(B).Depth, TRadixTreeNode(A).Depth);
  if result = 0 then
    result := CompareInteger(length(TRadixTreeNode(B).FullText),
                             length(TRadixTreeNode(A).FullText));
  if result = 0 then
    result := StrComp(pointer(TRadixTreeNode(A).FullText),
                      pointer(TRadixTreeNode(B).FullText));
end;

procedure TRadixTreeNode.SortChildren;
var
  i: PtrInt;
begin
  for i := 0 to high(Child) do
    Child[i].SortChildren; // recursive sorting
  ObjArraySort(Child, RadixTreeNodeCompare);
end;

constructor TRadixTreeNode.Create(aOwner: TRadixTree);
begin
  inherited Create;
  Owner := aOwner;
end;

function TRadixTreeNode.Split(const Text: RawUtf8): TRadixTreeNode;
begin
  result := TRadixTreeNodeClass(PPointer(self)^).Create(Owner);
  result.Chars := Text;
  result.FullText := FullText;
  result.Child := Child;
  result.Flags := Flags;
  Chars := '';
  FullText := '';
  Child := nil;
  Flags := [];
  ObjArrayAdd(Child, result);
end;

destructor TRadixTreeNode.Destroy;
begin
  inherited Destroy;
  ObjArrayClear(Child);
end;

function TRadixTreeNode.Find(P: PUtf8Char): TRadixTreeNode;
var
  c: PUtf8Char;
  n: TDALen;
  t: PNormTable;
  ch: ^TRadixTreeNode;
begin
  result := nil; // no match
  t := Owner.fNormTable;
  c := pointer(Chars);
  repeat
    if (t[P^] <> c^) or // may do LowerCaseSelf(Chars) at Insert()
       (P^ = #0) then
      break;
    inc(P);
    inc(c);
  until false;
  if c^ <> #0 then
    exit; // not enough matched chars
  // if we reached here, the text do match up to now
  if P^ = #0 then
    result := self // exact match found for this entry
  else
  begin
    ch := pointer(Child);
    if ch = nil then
      exit;
    n := PDALen(PAnsiChar(ch) - _DALEN)^ + _DAOFF;
    repeat
      if ch^.Chars[1] = t[P^] then
      begin
        result := ch^.Find(P);
        if result <> nil then
          exit; // match found in children
      end;
      inc(ch);
      dec(n);
    until n = 0;
  end;
end;

procedure TRadixTreeNode.ToText(var Result: RawUtf8; Level: integer);
var
  i: PtrInt;
begin
  Append(Result, [RawUtf8OfChar(' ', Level), Chars, #10]);
  for i := 0 to high(Child) do
    Child[i].ToText(Result, Level + length(Chars));
end;


{ TRadixTree }

constructor TRadixTree.Create(aNodeClass: TRadixTreeNodeClass;
  aOptions: TRadixTreeOptions);
begin
  if aNodeClass = nil then
    raise ERadixTree.CreateUtf8('%.Create with aNodeClass=nil', [self]);
  fDefaultNodeClass := aNodeClass;
  fOptions := aOptions;
  if rtoCaseInsensitiveUri in aOptions then
    fNormTable := @NormToLower
  else
    fNormTable := @NormToNorm;
  fRoot := fDefaultNodeClass.Create(self); // with no text
end;

destructor TRadixTree.Destroy;
begin
  inherited Destroy;
  fRoot.Free; // will recursively free all nested children
end;

procedure TRadixTree.Clear;
begin
  if self = nil then
    exit;
  fRoot.Free;
  fRoot := fDefaultNodeClass.Create(self);
end;

function TRadixTree.Insert(Text: RawUtf8; Node: TRadixTreeNode;
  NodeClass: TRadixTreeNodeClass): TRadixTreeNode;
var
  match, textlen, nodelen, i: PtrInt;
  chars: RawUtf8;
begin
  result := nil;
  if Text = '' then
    exit;
  if Node = nil then
    Node := fRoot;
  if rtoCaseInsensitiveUri in Options then
    LowerCaseSelf(Text);
  textlen := length(Text);
  nodelen := length(Node.Chars);
  // check how many chars of Text are within Node.Chars
  match := 0;
  while (match < textlen) and
        (match < nodelen) and
        (Text[match + 1] = Node.Chars[match + 1]) do
    inc(match);
  // insert the node where fits
  chars := copy(Text, match + 1, maxInt);
  if (match = 0) or
     (Node = fRoot) or
     ((match < textlen) and
      (match >= nodelen)) then
  begin
    // we can just insert a new leaf node
    if chars <> '' then
      for i := 0 to high(Node.Child) do
        if Node.Child[i].Chars[1] = chars[1] then
        begin
          result := Insert(chars, Node.Child[i]); // recursive insertion
          result.FullText := Text;
          exit;
        end;
  end
  else if match <> nodelen then
  begin
    // need to split the existing node
    Node.Split(copy(Node.Chars, match + 1, maxInt)); // split children
    Node.Chars := copy(Text, 1, match); // new shared root
    if chars = '' then
    begin
      result := Node; // don't need a sub child - use shared root
      result.FullText := Text;
      exit;
    end;
  end
  else
  begin
    // match an existing node
    result := Node;
    exit;
  end;
  // create new leaf
  if NodeClass = nil then
    NodeClass := fDefaultNodeClass;
  result := NodeClass.Create(self);
  result.Chars := chars;
  result.FullText := Text;
  ObjArrayAdd(Node.Child, result);
end;

procedure TRadixTree.AfterInsert;
begin
  fRoot.ComputeDepth;
  fRoot.SortChildren;
end;

function TRadixTree.Find(const Text: RawUtf8): TRadixTreeNode;
var
  n: TDALen;
  c: AnsiChar;
  ch: ^TRadixTreeNode;
begin
  result := nil;
  if (self = nil) or
     (Text = '') then
    exit;
  ch := pointer(fRoot.Child);
  if ch = nil then
    exit;
  n := PDALen(PAnsiChar(ch) - _DALEN)^ + _DAOFF;
  c := fNormTable[Text[1]];
  repeat
    if ch^.Chars[1] = c then // recursive call if may match
    begin
      result := ch^.Find(pointer(Text));
      if result <> nil then
        exit;
    end;
    inc(ch);
    dec(n);
  until n = 0;
end;

function TRadixTree.ToText: RawUtf8;
begin
  result := '';
  if self <> nil then
    fRoot.ToText(result, 0);
end;


{ TRadixTreeNodeParams }

function TRadixTreeNodeParams.Split(const Text: RawUtf8): TRadixTreeNode;
begin
  result := inherited Split(Text);
  TRadixTreeNodeParams(result).Names := Names;
  Names := nil;
end;

function TRadixTreeNodeParams.Lookup(P: PUtf8Char; Ctxt: TObject): TRadixTreeNodeParams;
var
  n: TDALen;
  c: PUtf8Char;
  t: PNormTable;
  f: TRadixTreeNodeFlags;
  ch: ^TRadixTreeNodeParams;
begin
  result := nil; // no match
  t := Owner.fNormTable;
  if Names = nil then
  begin
    // static text
    c := pointer(Chars);
    if c <> nil then
    begin
      repeat
        if (t^[P^] <> c^) or // may do LowerCaseSelf(Chars) at Insert()
           (P^ = #0) then
          break;
        inc(P);
        inc(c);
      until false;
      if c^ <> #0 then
        exit; // not enough matched chars
    end;
  end
  else
  begin
    // <named> parameter
    c := P;
    f := Flags;
    if rtfParamInteger in f then // <int:name> or rtoIntegerParams
    begin
      if (P^ < '0') or (P^ > '9') then
        exit; // void <integer> is not allowed
      repeat
        inc(P)
      until (P^ < '0') or (P^ > '9');
      if (P^ <> #0) and (P^ <> '?') and (P^ <> '/') then
        exit; // not an integer
    end
    else if rtfParamPath in f then // <path:filename> or * as <path:path>
      while (P^ <> #0) and (P^ <> '?') do
        inc(P)
    else // regular <param>
      while (P^ <> #0) and (P^ <> '?') and (P^ <> '/') do
        inc(P);
    if (Ctxt <> nil) and not LookupParam(Ctxt, c, P - c) then
      exit; // the parameter is not in the expected format for Ctxt
  end;
  // if we reached here, the URI do match up to now
  if (P^ = #0) or (P^ = '?') then
  begin
    if (P^ = '?') and (Ctxt <> nil) then
      LookupParam(Ctxt, P, -1); // store the inlined parameters position in Ctxt
    result := self; // exact match found for this entry (excluding URI params)
    exit;
  end;
  ch := pointer(Child);
  if ch = nil then
    exit;
  n := PDALen(PAnsiChar(ch) - _DALEN)^ + _DAOFF;
  repeat
    if (ch^.Names <> nil) or
       (ch^.Chars[1] = t^[P^]) then // recursive call only if worth it
    begin
      result := ch^.Lookup(P, Ctxt);
      if result <> nil then
        exit; // match found in children
    end;
    inc(ch);
    dec(n);
  until n = 0;
end;


{ TRadixTreeParams }

function TRadixTreeParams.Setup(const aFromUri: RawUtf8;
  out aNames: TRawUtf8DynArray): TRadixTreeNodeParams;
var
  u: PUtf8Char;
  item, full: RawUtf8;
  flags: TRadixTreeNodeFlags;
begin
  u := pointer(TrimU(aFromUri));
  if PosExChar('<', aFromUri) = 0 then
    // a simple static route
    result := Insert(aFromUri) as TRadixTreeNodeParams
  else
    // parse static..<param1>..static..<param2>..static into static/param nodes
    repeat
      GetNextItem(u, '<', item);
      full := full + item;
      result := Insert(full) as TRadixTreeNodeParams; // static (Names = nil)
      if u = nil then
        break;
      GetNextItem(u, '>', item);
      if item = '' then
        raise ERadixTree.CreateUtf8('Void <> in %.Setup(''%'')', [self, aFromUri]);
      flags := [rtfParam];
      if IdemPChar(pointer(item), 'INT:') then
      begin
        delete(item, 1, 4);
        include(flags, rtfParamInteger);
      end
      else if rtoIntegerParams in Options then
        include(flags, rtfParamInteger);
      if IdemPChar(pointer(item), 'PATH:') then
      begin
        delete(item, 1, 5);
        include(flags, rtfParamPath);
      end;
      if FindRawUtf8(aNames{%H-}, item) >= 0 then
        raise ERadixTree.CreateUtf8('Duplicated <%> in %.Setup(''%'')',
          [item, self, aFromUri]);
      AddRawUtf8(aNames, item);
      full := full + '<' + item + '>'; // avoid name collision with static
      result := Insert(full) as TRadixTreeNodeParams; // param (Names <> nil)
      result.Names := copy(aNames); // each node has its own Names copy
      result.Flags := flags;
      if (u = nil) or
         (u^ = #0) then
        // TODO: detect wildchar incompatibilities with nested searches?
        break;
      if u^ <> '/' then
        raise ERadixTree.CreateUtf8('Unexpected <%>% in %.Setup(''%'')',
          [item, u^, self, aFromUri]);
    until false;
  AfterInsert; // compute Depth and sort by priority
end;



procedure InitializeUnit;
var
  k: TRttiKind;
begin
  // initialize RTTI low-level comparison functions
  RTTI_ORD_COMPARE[roSByte]  := @_BC_SByte;
  RTTI_ORD_COMPARE[roUByte]  := @_BC_UByte;
  RTTI_ORD_COMPARE[roSWord]  := @_BC_SWord;
  RTTI_ORD_COMPARE[roUWord]  := @_BC_UWord;
  RTTI_ORD_COMPARE[roSLong]  := @_BC_SLong;
  RTTI_ORD_COMPARE[roULong]  := @_BC_ULong;
  {$ifdef FPC_NEWRTTI}
  RTTI_ORD_COMPARE[roSQWord] := @_BC_SQWord;
  RTTI_ORD_COMPARE[roUQWord] := @_BC_UQWord;
  {$endif FPC_NEWRTTI}
  RTTI_FLOAT_COMPARE[rfSingle]   := @_BC_Single;
  RTTI_FLOAT_COMPARE[rfDouble]   := @_BC_Double;
  RTTI_FLOAT_COMPARE[rfExtended] := @_BC_Extended;
  RTTI_FLOAT_COMPARE[rfComp]     := @_BC_SQWord; // PInt64 is the best
  RTTI_FLOAT_COMPARE[rfCurr]     := @_BC_SQWord;
  // initialize RTTI binary persistence and high-level comparison functions
  for k := succ(low(k)) to high(k) do
    case k of
      rkInteger,
      rkEnumeration,
      rkSet,
      rkChar,
      rkWChar
      {$ifdef FPC}, rkBool{$endif}:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Ord;
          RTTI_BINARYLOAD[k] := @_BL_Ord;
          RTTI_COMPARE[false, k] := @_BC_Ord;
          RTTI_COMPARE[true,  k] := @_BC_Ord;
        end;
      {$ifdef FPC} rkQWord, {$endif}
      rkInt64:
        begin
          RTTI_BINARYSAVE[k] := @_BS_64;
          RTTI_BINARYLOAD[k] := @_BL_64;
          RTTI_COMPARE[false, k] := @_BC_64;
          RTTI_COMPARE[true,  k] := @_BC_64;
        end;
      rkFloat:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Float;
          RTTI_BINARYLOAD[k] := @_BS_Float;
          RTTI_COMPARE[false, k] := @_BC_Float;
          RTTI_COMPARE[true,  k] := @_BC_Float;
        end;
      rkLString:
        begin
          RTTI_BINARYSAVE[k] := @_BS_String;
          RTTI_BINARYLOAD[k] := @_BL_LString;
          RTTI_COMPARE[false, k] := @_BC_LString;
          RTTI_COMPARE[true,  k] := @_BCI_LString;
        end;
      {$ifdef HASVARUSTRING}
      rkUString:
        begin
          RTTI_BINARYSAVE[k] := @_BS_UString;
          RTTI_BINARYLOAD[k] := @_BL_UString;
          RTTI_COMPARE[false, k] := @_BC_WString;
          RTTI_COMPARE[true,  k] := @_BCI_WString;
        end;
      {$endif HASVARUSTRING}
      rkWString:
        begin
          RTTI_BINARYSAVE[k] := @_BS_WString;
          RTTI_BINARYLOAD[k] := @_BL_WString;
          RTTI_COMPARE[false, k] := @_BC_WString;
          RTTI_COMPARE[true,  k] := @_BCI_WString;
        end;
      {$ifdef FPC}rkObject,{$else}{$ifdef UNICODE}rkMRecord,{$endif}{$endif}
      rkRecord:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Record;
          RTTI_BINARYLOAD[k] := @_BL_Record;
          RTTI_COMPARE[false, k] := @_BC_Record;
          RTTI_COMPARE[true,  k] := @_BCI_Record;
        end;
      rkDynArray:
        begin
          RTTI_BINARYSAVE[k] := @_BS_DynArray;
          RTTI_BINARYLOAD[k] := @_BL_DynArray;
          RTTI_COMPARE[false, k] := @_BC_DynArray;
          RTTI_COMPARE[true,  k] := @_BCI_DynArray;
        end;
      rkArray:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Array;
          RTTI_BINARYLOAD[k] := @_BL_Array;
          RTTI_COMPARE[false, k] := @_BC_Array;
          RTTI_COMPARE[true,  k] := @_BCI_Array;
        end;
      rkVariant:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Variant;
          RTTI_BINARYLOAD[k] := @_BL_Variant;
          RTTI_COMPARE[false, k] := @_BC_Variant;
          RTTI_COMPARE[true,  k] := @_BCI_Variant;
        end;
      rkClass:
        begin
          RTTI_COMPARE[false, k] := @_BC_Object;
          RTTI_COMPARE[true,  k] := @_BCI_Object;
        end;
        // unsupported types will contain nil
    end;
  // setup internal function wrappers
  GetDataFromJson := _GetDataFromJson;
end;


initialization
  InitializeUnit;

end.

