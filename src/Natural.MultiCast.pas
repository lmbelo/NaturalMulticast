unit Natural.MultiCast;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  Natural.EventImplementation;

type
  TEventMultiCast = class abstract
  strict private
    FInstance: TObject;
    FTypeInfo: PTypeInfo;
    FInvoke: TMethodImplementationCallback;
    FImpl: TEventImplementation;
    function GetValue: TValue;
  protected
    function CreateImplementation(): TEventImplementation; virtual;
  public
    constructor Create(const AInstance: TObject; const ATypeInfo: PTypeInfo;
      const AInvoke: TMethodImplementationCallback);
    destructor Destroy(); override;

    function AsEvent<T>(): T;

    property Instance: TObject read FInstance;
    property Value: TValue read GetValue;
  end;

  TPropertyMultiCast = class abstract(TEventMultiCast)
  strict private
    FName: string;
    FAssigned: boolean;
  public
    constructor Create(const AInstance: TObject; const AName: string;
      const AInvoke: TMethodImplementationCallback);

    function CanAssign(): boolean;
    procedure Assign(); virtual;

    property Name: string read FName;
  end;

  //This will save the current event assignment and call it first when triggered.
  TRestorablePropertyMultiCast = class(TPropertyMultiCast)
  strict private
    FInvoke: TMethodImplementationCallback;
    FEvt: TMethod;
  public
    constructor Create(const AInstance: TObject; const AName: string;
      const AInvoke: TMethodImplementationCallback);

    procedure Assign(); override;
  end;

  TAnonymousListener = reference to procedure(const ASelf: TObject; const AArgs: TArray<TValue>);
  TInstanceMultiCast = class
  strict private
    FInstance: TObject;
    FImpls: TObjectDictionary<string, TPropertyMultiCast>;
    FMethodListeners: TObjectDictionary<string, TList<TMethod>>;
    FAnnonListeners: TObjectDictionary<string, TList<TAnonymousListener>>;

    procedure TryAddImpl(const AProperty: string);
    procedure TryAddListener(const AProperty: string; const AMethod: TMethod); overload;
    procedure TryAddListener(const AProperty: string; const AMethod: TAnonymousListener); overload;

    function IsMethod(const ATypeInfo: PTypeInfo): boolean;
    function MakeMethod<T>(const AListener: T): TMethod;
  public
    constructor Create(const AInstance: TObject);
    destructor Destroy(); override;

    procedure AddListener<T>(const AProperty: string; const AListener: T); overload;
    procedure AddListener<T>(const AProperty: string; const AListener: TAnonymousListener); overload;
    procedure RemoveListener<T>(const AProperty: string; const AListener: T); overload;
    procedure RemoveListener<T>(const AProperty: string; const AListener: TAnonymousListener); overload;
  end;

  TGlobalMulticast = class
  private
    class var FInstance: TGlobalMulticast;
    class var FCanIntercept: boolean;
    class constructor Create();
    class destructor Destroy();
  private type
    //Container
    TComponentContainer = class(TComponent)
    private
      FCallback: TProc;
    public
      constructor Create(const AOwner: TObject; const AOnDestroy: TProc); reintroduce;
      destructor Destroy(); override;
      class function CanManage(const AInstance: TObject): boolean; static;
    end;
    //Mocker
    TObjectContainer = class
    private
      FInstance: TObject;
      FCallback: TProc;
      FInterceptor: TVirtualMethodInterceptor;
    public
      constructor Create(const AOwner: TObject; const AOnDestroy: TProc);
      destructor Destroy(); override;
      class function CanManage(const AInstance: TObject): boolean; static;
    end;
  private
    FContainer: TObjectDictionary<TObject, TInstanceMultiCast>;
  public
    constructor Create();
    destructor Destroy(); override;

    function HasProxy(const AInstance: TObject): boolean;
    function Proxify(const AInstance: TObject; const AManage: boolean = true): TInstanceMultiCast;
    procedure Unproxify(const AInstance: TObject);

    function Lookup(const AInstance: TObject): TInstanceMultiCast;

    class property Instance: TGlobalMulticast read FInstance;
    class property CanIntercept: boolean read FCanIntercept write FCanIntercept;
  end;

  TObjectHelper = class helper for TObject
  private
    function GetMultiCast: boolean;
    procedure SetMultiCast(const Value: boolean);
  public
    function AddListener<T>(const AProperty: string; const AListener: T): boolean; overload;
    function AddListener<T>(const AProperty: string; const AListener: TAnonymousListener): boolean; overload;
    procedure RemoveListener<T>(const AProperty: string; const AListener: T); overload;
    procedure RemoveListener<T>(const AProperty: string; const AListener: TAnonymousListener); overload;

    property MultiCast: boolean read GetMultiCast write SetMultiCast;
  end;

  EUnknownProperty = class(Exception)
  end;

  EMultiCastEnabled = class(Exception)
  end;

  EMultiCastDisabled = class(Exception)
  end;

implementation

{ TEventMultiCast }

constructor TEventMultiCast.Create(const AInstance: TObject;
  const ATypeInfo: PTypeInfo; const AInvoke: TMethodImplementationCallback);
begin
  inherited Create();
  FInstance := AInstance;
  FTypeInfo := ATypeInfo;
  FInvoke := AInvoke;
  FImpl := CreateImplementation();
end;

destructor TEventMultiCast.Destroy;
begin
  FImpl.Free();
  inherited;
end;

function TEventMultiCast.GetValue: TValue;
begin
  Assert(Assigned(FImpl), 'Field "FImpl" not assigned.');
  TValue.Make(nil, FTypeInfo, Result);
  Assert(Result.DataSize = SizeOf(Pointer) * 2, 'Invalid generic type for "T".');
  PMethod(Result.GetReferenceToRawData)^.Code := FImpl.CodeAddress;
  PMethod(Result.GetReferenceToRawData)^.Data := FInstance;
end;

function TEventMultiCast.CreateImplementation: TEventImplementation;
begin
  Result := TEventImplementation.Create(FTypeInfo, FInstance, FInvoke);
end;

function TEventMultiCast.AsEvent<T>: T;
begin
  Result := Value.AsType<T>;
end;

{ TPropertyMultiCast }

constructor TPropertyMultiCast.Create(const AInstance: TObject;
  const AName: string; const AInvoke: TMethodImplementationCallback);
var
  LRttiCtx: TRttiContext;
begin
  FName := AName;
  var LRttiType := LRttiCtx.GetType(PTypeInfo(AInstance.ClassInfo));
  var LRttiProp := LRttiType.GetProperty(AName);

  if not Assigned(LRttiProp) then
    raise EUnknownProperty.CreateFmt('Property "%s" not found.', [AName]);

  inherited Create(AInstance, LRttiProp.PropertyType.Handle, AInvoke);
end;

function TPropertyMultiCast.CanAssign: boolean;
begin
  Result := not FAssigned;
end;

procedure TPropertyMultiCast.Assign;
var
  LRttiCtx: TRttiContext;
begin
  if not CanAssign() then
    Exit;

  var LRttiType := LRttiCtx.GetType(PTypeInfo(Instance.ClassInfo));
  var LRttiProp := LRttiType.GetProperty(FName);

  LRttiProp.SetValue(Instance, Value);

  FAssigned := true;
end;

{ TRestorablePropertyMultiCast }

constructor TRestorablePropertyMultiCast.Create(const AInstance: TObject;
  const AName: string; const AInvoke: TMethodImplementationCallback);
begin
  FInvoke := AInvoke;

  inherited Create(AInstance, AName,
    procedure(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue)
    begin
      if Assigned(FEvt.Code) and Assigned(FEvt.Data) then begin
        var LRttiCtx := TRttiContext.Create();
        try
          var LRttiType := LRttiCtx.GetType(PTypeInfo(AInstance.ClassInfo));
          var LRttiProp := LRttiType.GetProperty(Name);
          var LRttiMethodType := LRttiCtx.GetType(LRttiProp.PropertyType.Handle) as TRttiMethodType;

          var LValue := TValue.Empty;
          TValue.Make(@FEvt, LRttiMethodType.Handle, LValue);

          var LArgs: TArray<TValue>;
          SetLength(LArgs, Length(Args) - 1);
          TArray.Copy<TValue>(Args, LArgs, 1, 0, Length(Args) - 1);

          LRttiMethodType.Invoke(LValue, LArgs);
        finally
          LRttiCtx.Free();
        end;
      end;

      FInvoke(UserData, Args, Result);
    end);
end;

procedure TRestorablePropertyMultiCast.Assign;
var
  LRttiCtx: TRttiContext;
begin
  if not CanAssign() then
    Exit;

  var LRttiType := LRttiCtx.GetType(PTypeInfo(Instance.ClassInfo));
  var LRttiProp := LRttiType.GetProperty(Name);

  var LMethod := LRttiProp.GetValue(Instance);
  if not LMethod.IsEmpty then begin
    FEvt.Code := PMethod(LMethod.GetReferenceToRawData())^.Code;
    FEvt.Data := PMethod(LMethod.GetReferenceToRawData())^.Data;
  end;

  inherited;
end;

{ TInstanceMultiCast }

constructor TInstanceMultiCast.Create(const AInstance: TObject);
begin
  inherited Create();
  FInstance := AInstance;
  FImpls := TObjectDictionary<string, TPropertyMultiCast>.Create([doOwnsValues]);
  FMethodListeners := TObjectDictionary<string, TList<TMethod>>.Create([doOwnsValues]);
  FAnnonListeners := TObjectDictionary<string, TList<TAnonymousListener>>.Create([doOwnsValues])
end;

destructor TInstanceMultiCast.Destroy;
begin
  FAnnonListeners.Free();
  FMethodListeners.Free();
  FImpls.Free();
  inherited;
end;

procedure TInstanceMultiCast.TryAddImpl(const AProperty: string);
begin
  if FImpls.ContainsKey(AProperty) then
    Exit;

  var LMultiCastProp := TRestorablePropertyMultiCast.Create(FInstance, AProperty,
    procedure(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue)
    var
      LRttiCtx: TRttiContext;
      LMethodListeners: TList<TMethod>;
      LAnnonListeners: TList<TAnonymousListener>;
      LValue: TValue;
    begin
      var LRttiType := LRttiCtx.GetType(PTypeInfo(FInstance.ClassInfo));
      var LRttiProp := LRttiType.GetProperty(AProperty);
      var LRttiMethodType := LRttiCtx.GetType(LRttiProp.PropertyType.Handle) as TRttiMethodType;      
        
      var LArgs: TArray<TValue>;
      SetLength(LArgs, Length(Args) - 1);
      TArray.Copy<TValue>(Args, LArgs, 1, 0, Length(Args) - 1);
        
      if FMethodListeners.TryGetValue(AProperty, LMethodListeners) then begin                                              
        TValue.Make(nil, LRttiMethodType.Handle, LValue);
        for var LListener in LMethodListeners do begin          
          PMethod(LValue.GetReferenceToRawData())^.Code := LListener.Code;
          PMethod(LValue.GetReferenceToRawData())^.Data := LListener.Data;                    
          LRttiMethodType.Invoke(LValue, LArgs);
        end;                
      end;

      if FAnnonListeners.TryGetValue(AProperty, LAnnonListeners) then
        for var LListener in LAnnonListeners do
          LListener(Args[0].AsObject, LArgs);      
    end);
  FImpls.Add(AProperty, LMultiCastProp);
  LMultiCastProp.Assign();
end;

procedure TInstanceMultiCast.TryAddListener(const AProperty: string;
  const AMethod: TMethod);
begin
  if not FMethodListeners.ContainsKey(AProperty) then
    FMethodListeners.Add(AProperty, TList<TMethod>.Create());

  var LListeners := FMethodListeners.Items[AProperty];
  LListeners.Add(AMethod);
end;

procedure TInstanceMultiCast.TryAddListener(const AProperty: string;
  const AMethod: TAnonymousListener);
begin
  if not FAnnonListeners.ContainsKey(AProperty) then
    FAnnonListeners.Add(AProperty, TList<TAnonymousListener>.Create());

  var LListeners := FAnnonListeners.Items[AProperty];
  LListeners.Add(AMethod);  
end;

function TInstanceMultiCast.IsMethod(const ATypeInfo: PTypeInfo): boolean;
begin
  Result := ATypeInfo.Kind = TTypeKind.tkMethod;
end;

function TInstanceMultiCast.MakeMethod<T>(const AListener: T): TMethod;
var
  LValue: TValue;
begin
  TValue.Make<T>(AListener, LValue);
  Result.Code := PMethod(LValue.GetReferenceToRawData())^.Code;
  Result.Data := PMethod(LValue.GetReferenceToRawData())^.Data;
end;

procedure TInstanceMultiCast.AddListener<T>(const AProperty: string;
  const AListener: T);
begin
  TryAddImpl(AProperty);                                       
  if IsMethod(TypeInfo(T)) then
    TryAddListener(AProperty, MakeMethod<T>(AListener));
end;

procedure TInstanceMultiCast.AddListener<T>(const AProperty: string;
  const AListener: TAnonymousListener);
begin
  TryAddImpl(AProperty);
  TryAddListener(AProperty, AListener);
end;

procedure TInstanceMultiCast.RemoveListener<T>(const AProperty: string;
  const AListener: T);
begin
  if not FMethodListeners.ContainsKey(AProperty) then
    Exit;

  var LMethod := MakeMethod<T>(AListener);
  var LListeners := FMethodListeners.Items[AProperty];
  for var LListener in LListeners do begin
    if (LListener.Code = LMethod.Code) and (LListener.Data = LMethod.Data) then
      LListeners.Remove(LListener);
  end;
end;

procedure TInstanceMultiCast.RemoveListener<T>(const AProperty: string;
  const AListener: TAnonymousListener);
begin
  if not FAnnonListeners.ContainsKey(AProperty) then
    Exit;

  var LListeners := FAnnonListeners.Items[AProperty];
  LListeners.Remove(AListener);
end;

{ TGlobalMulticast }

class constructor TGlobalMulticast.Create;
begin
  FInstance := TGlobalMulticast.Create();
  FCanIntercept := true;
end;

class destructor TGlobalMulticast.Destroy;
begin
  FInstance.Free();
end;

constructor TGlobalMulticast.Create;
begin
  FContainer := TObjectDictionary<TObject, TInstanceMultiCast>.Create([doOwnsValues]);
end;

destructor TGlobalMulticast.Destroy;
begin
  inherited;
  FContainer.Free();
end;

function TGlobalMulticast.HasProxy(const AInstance: TObject): boolean;
begin
  Result := (Lookup(AInstance) <> nil);
end;

function TGlobalMulticast.Proxify(const AInstance: TObject; 
  const AManage: boolean): TInstanceMultiCast;
begin
  if HasProxy(AInstance) then  
    raise EMultiCastEnabled.Create('Multicast alread enabled for instance.');

  Result := TInstanceMultiCast.Create(AInstance);
  FContainer.Add(AInstance, Result);

  if TComponentContainer.CanManage(AInstance) then
    TComponentContainer.Create(AInstance, procedure() begin
      FContainer.Remove(AInstance);
    end)
  else if TObjectContainer.CanManage(AInstance) then
    TObjectContainer.Create(AInstance, procedure() begin
      FContainer.Remove(AInstance);
    end)
end;

procedure TGlobalMulticast.Unproxify(const AInstance: TObject);
begin
  if not HasProxy(AInstance) then
    raise EMultiCastDisabled.Create('Multicast is disabled for instance.');

  FContainer.Remove(AInstance);
end;

function TGlobalMulticast.Lookup(const AInstance: TObject): TInstanceMultiCast;
begin
  FContainer.TryGetValue(AInstance, Result);
end;

{ TGlobalMulticast.TComponentContainer }

class function TGlobalMulticast.TComponentContainer.CanManage(
  const AInstance: TObject): boolean;
begin
  Result := AInstance is TComponent;
end;

constructor TGlobalMulticast.TComponentContainer.Create(const AOwner: TObject;
  const AOnDestroy: TProc);
begin
  Assert(CanManage(AOwner), Format(
    'TComponentContainer can''t manage type %s.', [AOwner.ClassName]));
  inherited Create(TComponent(AOwner));
  ChangeName('_multicastcontainer');
  SetSubComponent(true);
  FCallback := AOnDestroy;
end;

destructor TGlobalMulticast.TComponentContainer.Destroy;
begin
  if Assigned(FCallback) then
    FCallback();
  inherited;
end;

{ TGlobalMulticast.TObjectContainer }

constructor TGlobalMulticast.TObjectContainer.Create(const AOwner: TObject;
  const AOnDestroy: TProc);
begin
  inherited Create();
  FInstance := AOwner;
  FCallback := AOnDestroy;
  FInterceptor := TVirtualMethodInterceptor.Create(AOwner.ClassType);
  FInterceptor.OnAfter := procedure(Instance: TObject; Method: TRttiMethod;
    const Args: TArray<TValue>; var Result: TValue)
    begin
      if Method.Name = 'BeforeDestruction' then begin
        if Assigned(FCallback) then
          FCallback();
        Destroy();
      end;
    end;
  FInterceptor.Proxify(AOwner);
end;

destructor TGlobalMulticast.TObjectContainer.Destroy;
begin
  FInterceptor.Unproxify(FInstance);
  FInterceptor.Free();
  inherited;
end;

class function TGlobalMulticast.TObjectContainer.CanManage(
  const AInstance: TObject): boolean;
begin
  Result := TGlobalMulticast.CanIntercept;
end;

{ TObjectHelper }

function TObjectHelper.GetMultiCast: boolean;
begin
  Result := TGlobalMulticast.Instance.HasProxy(Self);  
end;

procedure TObjectHelper.SetMultiCast(const Value: boolean);
begin
  if Value then
    TGlobalMulticast.Instance.Proxify(Self)
  else
    TGlobalMulticast.Instance.Unproxify(Self);
end;

function TObjectHelper.AddListener<T>(const AProperty: string;
  const AListener: T): boolean;
begin
  if not MultiCast then
    MultiCast := true;
    
  var LImpl := TGlobalMulticast.Instance.Lookup(Self);
  if not Assigned(LImpl) then
    Exit(false);
    
  LImpl.AddListener<T>(AProperty, AListener);

  Result := true;
end;

function TObjectHelper.AddListener<T>(const AProperty: string;
  const AListener: TAnonymousListener): boolean;
begin
  if not MultiCast then
    MultiCast := true;
    
  var LImpl := TGlobalMulticast.Instance.Lookup(Self);
  if not Assigned(LImpl) then
    Exit(false);
    
  LImpl.AddListener<T>(AProperty, AListener);

  Result := true;
end;

procedure TObjectHelper.RemoveListener<T>(const AProperty: string;
  const AListener: T);
begin
  var LImpl := TGlobalMulticast.Instance.Lookup(Self);
  if not Assigned(LImpl) then
    Exit;

  LImpl.RemoveListener<T>(AProperty, AListener);
end;

procedure TObjectHelper.RemoveListener<T>(const AProperty: string;
  const AListener: TAnonymousListener);
begin
  var LImpl := TGlobalMulticast.Instance.Lookup(Self);
  if not Assigned(LImpl) then
    Exit;
  LImpl.RemoveListener<T>(AProperty, AListener);
end;

end.
