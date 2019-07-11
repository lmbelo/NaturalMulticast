program MuntiCastEventsSample;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MulticastEvent in '..\..\Source\Integrates\TypeEx\MulticastEvents\MulticastEvent.pas',
  ObjAutoX in '..\..\Source\Integrates\TypeEx\MulticastEvents\ObjAutoX.pas';

type
  TCustomEvent = procedure(Sender: TObject) of object;

  TEventOrientedClass = class
  private
    FOnEvt: TCustomEvent;
  public
    property OnEvt: TCustomEvent read FOnEvt write FOnEvt;
  end;

  TEventListenerClass = class
  public
    procedure ListenTo(Sender: TObject);
  end;

procedure Main;
var
  LEvtOrientedClass: TEventOrientedClass;
  LMulticast: TMulticastEvent;
  LListenerOne, LListenerTwo: TEventListenerClass;
begin
  LMulticast := TMulticastEvent.Create(TypeInfo(TCustomEvent));
  try
    LEvtOrientedClass := TEventOrientedClass.Create();
    try
      LMulticast.Hook(PMethod(@TMethod(LEvtOrientedClass.OnEvt)));
      //or
      LEvtOrientedClass.OnEvt := TCustomEvent(LMulticast.Invoke);
      //Make two instances listening to the target event
      LListenerOne := TEventListenerClass.Create();
      try
        //Add event to event list
        TCustomEvent(LMulticast.Add()^) := LListenerOne.ListenTo;
        
        LListenerTwo := TEventListenerClass.Create();
        try   
          //Add event to event list        
          TCustomEvent(LMulticast.Add()^) := LListenerTwo.ListenTo;

          //Event trigger
          LEvtOrientedClass.OnEvt(LEvtOrientedClass);
        finally
          LListenerTwo.Free;
        end;
      finally
        LListenerOne.Free;
      end;

    finally
      LEvtOrientedClass.Free;
    end;
  finally
    LMulticast.Free;
  end;
  Readln;
end;

{ TEventListenerClass }

procedure TEventListenerClass.ListenTo(Sender: TObject);
begin
  Writeln(Format('Event listener in class %s sent by %s', 
                 [Self.ClassName, Sender.ClassName]));
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  Main;
end.
