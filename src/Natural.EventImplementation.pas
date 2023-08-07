unit Natural.EventImplementation;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Classes;

type
  TMethodImplementation = class(System.Rtti.TMethodImplementation)
  private
    {$IFDEF CPUX86}
    const SEAL_OFFSET = -968;
    const INVOKE_INFO_SIGNATURE_SIZE = 98;
    {$ENDIF CPUX86}
    {$IFDEF CPUX64}
    const SEAL_OFFSET = -672;
    const INVOKE_INFO_SIGNATURE_SIZE = 131;
    {$ENDIF CPUX64}
    {$IFDEF CPUARM}
      const SEAL_OFFSET: integer = 272164;
      const INVOKE_INFO_SIGNATURE_SIZE = 131;
    {$ENDIF CPUARM}
    class var FInvokeInfoClass: TClass;
    class var FSeal: pointer; { TODO : FIGURE OUT A BETTER WAY TO GET THE SEAL METHOD }
  private type
    TSeal = procedure() of object;
  private
    procedure CheckCompatibility();
    function CreateInvokeInfo(ACallConv: TCallConv; AHasSelf: Boolean): TObject;
    function MakeInvokeInfo(const AMethodType: TRttiMethodType): pointer;
    procedure Seal();
  private
    class constructor Create();
    class destructor Destroy();
  protected
    constructor Create(const AMethodType: TRttiMethodType; const AUserData: pointer;
      const AImplementation: TMethodImplementationCallback); reintroduce;
  public
    destructor Destroy(); override;
  end;

  TEventImplementation = class(TMethodImplementation)
  public
    constructor Create(const ATypeInfo: PTypeInfo; const AUserData: pointer;
      const AImplementation: TMethodImplementationCallback); reintroduce; overload;
  end;

  EUnableCreateInvokeInfo = class(Exception)
  end;

  EIncompatibleDelphiVersion = class(Exception)
  end;

implementation

{ TMethodImplementation }

class constructor TMethodImplementation.Create;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(System.Rtti.TMethodImplementation);
  //make access to the invokeinfo class type
  var LRttiMethod := LRttiType.GetMethod('Create') as TRttiMethod; //to get access to the invokeinfo only
  var LImpInfo := LRttiMethod.CreateImplementation(nil, nil);
  try
    FInvokeInfoClass := TMethodImplementation(LImpInfo).FInvokeInfo.ClassType;
  finally
    LImpInfo.Free();
  end;

  //make access to the seal proc
  LRttiType := LRttiCtx.GetType(FInvokeInfoClass);
  LRttiMethod := LRttiType.GetMethod('Seal');
  if Assigned(LRttiMethod) then
    FSeal := LRttiMethod.CodeAddress
  else begin
    var LRttiProp := LRttiType.GetProperty('ReturnType') as TRttiInstanceProperty;
    FSeal := Pointer(NativeInt(LRttiProp.PropInfo^.SetProc) + SEAL_OFFSET);
  end;
end;

class destructor TMethodImplementation.Destroy;
begin
  FInvokeInfoClass := nil;
end;

constructor TMethodImplementation.Create(const AMethodType: TRttiMethodType;
  const AUserData: pointer; const AImplementation: TMethodImplementationCallback);
begin
  inherited Create(AUserData, MakeInvokeInfo(AMethodType), AImplementation);
end;

destructor TMethodImplementation.Destroy;
begin
  if Assigned(FInvokeInfo) then
    FInvokeInfo.Free();
  inherited;
end;

function TMethodImplementation.MakeInvokeInfo(
  const AMethodType: TRttiMethodType): pointer;
begin
  if Assigned(FInvokeInfo) then
    Exit(FInvokeInfo);

  CheckCompatibility();

  FInvokeInfo := Pointer(
    CreateInvokeInfo(AMethodType.CallingConvention,
      AMethodType.MethodKind in [TMethodKind.mkProcedure, TMethodKind.mkFunction]));

  FInvokeInfo.AddParameter(PTypeInfo(Self.ClassInfo()), False, True, False, False);

  for var LParam in AMethodType.GetParameters() do
    FInvokeInfo.AddParameter(
      LParam.ParamType.Handle,
      ([pfVar, pfOut] * LParam.Flags <> []),
      (pfConst in LParam.Flags) or (LParam.ParamType.Handle^.Kind in [tkRecord, tkMRecord, tkString]) and ([pfVar, pfOut] * LParam.Flags = []),
      [pfOut, pfResult] * LParam.Flags <> [],
      False);

  if AMethodType.ReturnType <> nil then
    FInvokeInfo.ReturnType := AMethodType.ReturnType.Handle;

  Seal();

  Result := FInvokeInfo;
end;

procedure TMethodImplementation.CheckCompatibility;
var
  LRttiDataSize: integer;
begin
  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(FInvokeInfoClass);
    LRttiDataSize := Length(LRttiType.GetMethods())
      + Length(LRttiType.GetFields())
      + Length(LRttiType.GetProperties())
      + Length(LRttiType.GetIndexedProperties())
      + Length(LRttiType.GetAttributes())
      + FInvokeInfoClass.InstanceSize;
  finally
    LRttiCtx.Free();
  end;

  if (LRttiDataSize <> INVOKE_INFO_SIGNATURE_SIZE)
    or not Assigned(FInvokeInfoClass)
    or not Assigned(FSeal) then
      raise EIncompatibleDelphiVersion.Create('This Delphi version is not compatible');
end;

function TMethodImplementation.CreateInvokeInfo(ACallConv: TCallConv;
  AHasSelf: Boolean): TObject;
var
  LRttiCtx: TRttiContext;
  LCallConv: TValue;
begin
  var LRttiType := LRttiCtx.GetType(FInvokeInfoClass);
  var LRttiConstructor := LRttiType.GetMethod('Create');
  if not Assigned(LRttiConstructor) then
    raise EUnableCreateInvokeInfo.Create('Unable to create invoke info.');

  TValue.Make<TCallConv>(ACallConv, LCallConv);

  Result := LRttiConstructor.Invoke(FInvokeInfoClass, [LCallConv, AHasSelf]).AsObject();
end;

procedure TMethodImplementation.Seal();
var
  LMethod: TMethod;
begin
  LMethod.Code := FSeal;
  LMethod.Data := FInvokeInfo;
  TSeal(LMethod)();
end;

{ TEventImplementation }

constructor TEventImplementation.Create(const ATypeInfo: PTypeInfo;
  const AUserData: pointer; const AImplementation: TMethodImplementationCallback);
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(ATypeInfo);
  Assert(LRttiType is TRttiMethodType, 'Only TRttiMethodType are supported');
  inherited Create(LRttiType as TRttiMethodType, AUserData, AImplementation);
end;

end.
