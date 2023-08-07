# Natural Multicast Events

Natural Multicast Events have been done to be... natural!

It is still Beta, but you can full-feel its power.

It is simple, but powerfull:

1) Naturally add listeners to any event, even already signed events;
2) Any event property can be extended naturaly to multicast;
3) Anonymous listeners are supported;
4) The multicast container is managed by the library itself;
5) Events assigned in the dfm of a form will be called first;



> First thing is to add the Natural.Multicast unit to the uses section of your unit.

```
uses
  Natural.Multicast;
```

> Create any class type of your flavor, I'd be using a Form, for instance.

```
TForm1 = class(TForm)
end;

var
  Form1 := TForm1.Create(nil);
```

> You can assign a local event method or not.

```
TForm1 = class(TForm)
  procedure FormShow(Sender: TObject);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //I'm the local event that will be called first!
end;
```

> Create a listener class and start listening to events.

```
TListener = class
public
  procedure OnShow(Sender: TObject);
end;

LListener := TListener.Create();
Form1.AddListener<TNotifyEvent>('OnShow', LListener.OnShow);
```

> Or else, listen with an anonymous listener

```
Form1.AddListener<TNotifyEvent>('OnShow',
  procedure(const ASelf: TObject; const AArgs: TArray<TValue>)
  begin
    //Notified by Form1 OnShow event
  end);
```

#### That is all you need! Very natural...
