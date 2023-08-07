program ObjectEvents1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  Natural.EventImplementation in '..\..\src\Natural.EventImplementation.pas',
  Natural.MultiCast in '..\..\src\Natural.MultiCast.pas';

type
  {$M+}
  TObservable = class
  private
    FOnDoSomething: TNotifyEvent;
  public
    procedure DoSomething;
    property OnDoSomething: TNotifyEvent read FOnDoSomething write FOnDoSomething;
  end;
  {$M-}

  TListener = class
  private
    FId: integer;
  public
    procedure ListenTo(Sender: TObject);
    property Id: integer read FId write FId;
  end;

{ TObservable }

procedure TObservable.DoSomething;
begin
  if Assigned(FOnDoSomething) then
    FOnDoSomething(Self);
end;

{ TListener }

procedure TListener.ListenTo(Sender: TObject);
begin
  Writeln(Format('Listener %d is just listening to DoSomething from %s', [FId, Sender.ClassName]));
end;

begin
  try
    var LObservable := TObservable.Create();
    try
      //Anonymous listeners
      LObservable.AddListener<TNotifyEvent>('OnDoSomething',
        procedure(const ASelf: TObject; const AArgs: TArray<TValue>)
        begin
          Writeln('Notified anonymous listener 1 - Thanks to ' + AArgs[0].AsObject().ClassName);
        end);

      LObservable.AddListener<TNotifyEvent>('OnDoSomething',
        procedure(const ASelf: TObject; const AArgs: TArray<TValue>)
        begin
          Writeln('Notified anonymous listener 2');
        end);

      //Instance listeners
      var LList := TObjectList<TObject>.Create();
      try
        for var I := 0 to 9 do begin
          var LListener := TListener.Create();
          LListener.Id := I + 1;
          LObservable.AddListener<TNotifyEvent>('OnDoSomething', LListener.ListenTo);
          LList.Add(LListener);
        end;

        //Let's trigger the DoSomething event
        LObservable.DoSomething();
      finally
        LList.Free();
      end;
    finally
      LObservable.Free();
    end;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
