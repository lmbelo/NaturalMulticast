# Natural Multicast Events

Natural Multicast Events have been done to be... natural!

It is still Beta, but you can full-feel its power.

It is simple, but powerfull:

1) Naturally add listeners to any event, even already signed events;
2) Any event property can be extended naturaly to multicast;
3) Anonymous listeners are supported;
4) The multicast container is managed by the library itself;

Sample1:

> First thing is to add the Natural.Multicast unit to the uses section of your unit.

```
uses
  Natural.Multicast;
```

> Create any class type of your flavor, I'd be using a Form, for instance.

```
TForm1 = class(TForm)
end;
```

> Create a listener class and start listening to events.

```
TListener = class
public
  procedure OnShow(Sender: TObject);
end;


```
