unit MulticastEvent;

interface

uses
  Classes, SysUtils, ObjAutoX, TypInfo, Contnrs;

type  
  TDispatcher = class abstract(TInterfacedObject)
  private
    FHandlers: TList;
    FInternalDispatcher: TMethod;
  protected
    property Handlers: TList read FHandlers;
    property InternalDispatcher: TMethod read FInternalDispatcher write FInternalDispatcher;
  public
    constructor Create(const AHandlers: TList);
    destructor Destroy; override;

    function CanHandle(const AMethod: TMethod; const ATypeData: PTypeData): boolean; virtual; abstract;
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData); virtual; abstract;
  end;

  //Works for register calling convention only
  TEventDispatcher = class(TDispatcher)
  private
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
  public
    function CanHandle(const AMethod: TMethod; const ATypeData: PTypeData): boolean; override;
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData); override;
  end;

  TMethodDispatcher = class(TDispatcher, IMethodHandler)
  public
    function CanHandle(const AMethod: TMethod; const ATypeData: PTypeData): boolean; override;
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData); override;

    function Execute(const Args: array of variant): variant;
    function InstanceToVariant(Instance: TObject): variant;
  end;

  TDispatchers = array of TDispatcher;

  TEvent = procedure of object;

  PMethod = ^TMethod;

  TCustomMulticastEvent = class
  strict private
    FHandlers: TList;
    FDispatchers: TDispatchers;
  protected
    procedure SetEventDispatcher(var ADispatcher: TMethod; ATypeData: PTypeData); virtual;
    property Handlers: TList read FHandlers;
  public
    constructor Create(const AList: TList);
    destructor Destroy; override;
  end;

  TMulticastEvent = class(TCustomMulticastEvent)
  strict private
    FInvoke: TMethod;
    FHandlers: TList;
  public
    constructor Create(const ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    function Add(): PMethod; overload;
    function Add(const AMethod: TMethod): integer; overload;
    function IndexOf(const AMethod: TMethod): integer;
    procedure Remove(const AMethod: TMethod);

    procedure Hook(AMethod: PMethod);
    property Invoke: TMethod read FInvoke;
  end;

  TMethodList = class(TList)
  private
    FPtrSize: integer;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const APtrSize: integer);
  end;

implementation

uses
  Windows;

{ TDispatcher }

constructor TDispatcher.Create(const AHandlers: TList);
begin
  FHandlers := AHandlers;
  inherited Create;
end;

destructor TDispatcher.Destroy;
begin
  ReleaseMethodPointer(FInternalDispatcher);
  inherited;
end;

{ TEventDispatcher }

function TEventDispatcher.CanHandle(const AMethod: TMethod; const ATypeData: PTypeData): boolean;
begin
  Result := true; //Check for calling convention, if available...
end;

procedure TEventDispatcher.InternalInvoke(Params: PParameters; StackSize: Integer);
var
  I: integer;
  LMethod: TMethod;
begin
  for I := 0 to Handlers.Count - 1 do begin
    LMethod := PMethod(Handlers[I])^;
    if not Assigned(LMethod.Code) or not Assigned(LMethod.Data) then Continue;
    // Check to see if there is anything on the stack.
    if StackSize > 0 then
      asm
        // if there are items on the stack, allocate the space there and
        // move that data over.
        MOV ECX,StackSize
        SUB ESP,ECX
        MOV EDX,ESP
        MOV EAX,Params
        LEA EAX,[EAX].TParameters.Stack[8]
        CALL System.Move
      end;
    asm
      // Now we need to load up the registers. EDX and ECX may have some data
      // so load them on up.
      MOV EAX,Params
      MOV EDX,[EAX].TParameters.Registers.DWORD[0]
      MOV ECX,[EAX].TParameters.Registers.DWORD[4]
      // EAX is always "Self" and it changes on a per method pointer instance, so
      // grab it out of the method data.
      MOV EAX,LMethod.Data
      // Now we call the method. This depends on the fact that the called method
      // will clean up the stack if we did any manipulations above.
      CALL LMethod.Code
    end;
  end;
end;

procedure TEventDispatcher.SetDispatcher(var AMethod: TMethod;
  ATypeData: PTypeData);
begin
  if Assigned(InternalDispatcher.Code) and Assigned(InternalDispatcher.Data) then
    ReleaseMethodPointer(InternalDispatcher);
  InternalDispatcher := CreateMethodPointer(InternalInvoke, ATypeData);
  AMethod := InternalDispatcher;  
end;

{ TMethodDispatcher }

function TMethodDispatcher.CanHandle(const AMethod: TMethod; const ATypeData: PTypeData): boolean;
begin
  Result := false;
end;

function TMethodDispatcher.Execute(const Args: array of variant): variant;
begin
  //Implement if required...
end;

function TMethodDispatcher.InstanceToVariant(Instance: TObject): variant;
begin
  //Implement if required...
end;

procedure TMethodDispatcher.SetDispatcher(var AMethod: TMethod;
  ATypeData: PTypeData);
begin
  if Assigned(InternalDispatcher.Code) and Assigned(InternalDispatcher.Data) then
    ReleaseMethodPointer(InternalDispatcher);
  InternalDispatcher := CreateMethodPointer(Self, ATypeData);
  AMethod := InternalDispatcher;
end;

{ TCustomMulticastEvent }

constructor TCustomMulticastEvent.Create(const AList: TList);
begin
  inherited Create();
  FHandlers := AList;
  SetLength(FDispatchers, 2);
  FDispatchers[0] := TMethodDispatcher.Create(FHandlers);
  FDispatchers[1] := TEventDispatcher.Create(FHandlers);
end;

destructor TCustomMulticastEvent.Destroy;
var
  LInvoker: TDispatcher;
begin
  for LInvoker in FDispatchers do begin
    LInvoker.Free;
  end;
  inherited;
end;

procedure TCustomMulticastEvent.SetEventDispatcher(var ADispatcher: TMethod;
  ATypeData: PTypeData);
var
  LInvoker: TDispatcher;
begin
  for LInvoker in FDispatchers do begin
    if LInvoker.CanHandle(ADispatcher, ATypeData) then begin
      LInvoker.SetDispatcher(ADispatcher, ATypeData);
      Break;
    end;
  end;
end;

{ TMulticastEvent }

constructor TMulticastEvent.Create(const ATypeInfo: PTypeInfo);
var
  LTypeData: PTypeData;
begin
  LTypeData := GetTypeData(ATypeInfo);
  FHandlers := TMethodList.Create(SizeOf(TMethod));
  inherited Create(FHandlers);
  Assert(ATypeInfo.Kind = tkMethod, 'ATypeInfo must be a method pointer type');
  SetEventDispatcher(FInvoke, LTypeData);
end;

destructor TMulticastEvent.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

procedure TMulticastEvent.Hook(AMethod: PMethod);
begin
  AMethod^ := FInvoke;
end;

function TMulticastEvent.Add: PMethod;
var
  LMethod: TMethod;
begin
  FillChar(LMethod, SizeOf(TMethod), 0);
  Result := FHandlers[FHandlers.Add(@LMethod)];
end;

function TMulticastEvent.Add(const AMethod: TMethod): integer;
begin
  Result := FHandlers.Add(@AMethod);
end;

function TMulticastEvent.IndexOf(const AMethod: TMethod): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FHandlers.Count - 1 do begin
    if (PMethod(FHandlers[I])^.Code = AMethod.Code) and
       (PMethod(FHandlers[I])^.Data = AMethod.Data) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TMulticastEvent.Remove(const AMethod: TMethod);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AMethod);
  if (LIndex >= 0) then begin
    FHandlers.Delete(LIndex);
  end;
end;

{ TMethodList }

constructor TMethodList.Create(const APtrSize: integer);
begin
  inherited Create();
  FPtrSize := APtrSize;
end;

procedure TMethodList.Notify(Ptr: Pointer; Action: TListNotification);
var
  LPtr: Pointer;
begin
  inherited;
  if (Action = lnAdded) then begin
    GetMem(LPtr, FPtrSize);
    CopyMemory(LPtr, Ptr, FPtrSize);
    List^[IndexOf(Ptr)] := LPtr;
  end else if (Action = lnDeleted) then begin
    FreeMem(Ptr, FPtrSize);
  end;
end;

end.
