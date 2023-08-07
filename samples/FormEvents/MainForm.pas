unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FListenerCount: integer;
    procedure DoAddListener();
  public
    procedure TellMe(const AText: string);
  end;

  TListener = class(TComponent)
  public
    procedure OnActivate(Sender: TObject);
    procedure OnClick(Sender: TObject);
    procedure OnDblClick(Sender: TObject);
    procedure OnShow(Sender: TObject);
    procedure OnResize(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  Natural.MultiCast;

{$R *.dfm}

procedure TForm1.DoAddListener;
begin
  Inc(FListenerCount);
  var LListener := TListener.Create(Self);
  LListener.ChangeName('listener' + FListenerCount.ToString());
  AddListener<TNotifyEvent>('OnActivate', LListener.OnActivate);
  AddListener<TNotifyEvent>('OnClick', LListener.OnClick);
  AddListener<TNotifyEvent>('OnDblClick', LListener.OnDblClick);
  AddListener<TNotifyEvent>('OnShow', LListener.OnShow);
  AddListener<TNotifyEvent>('OnResize', LListener.OnResize);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DoAddListener;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  TellMe('Form implementation of event OnActivate');
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  TellMe('Form implementation of event OnClick');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoAddListener();
  DoAddListener();
  DoAddListener();
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
  TellMe('Form implementation of event OnDblClick');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TellMe('Form implementation of event OnShow');
end;

procedure TForm1.TellMe(const AText: string);
begin
  Memo1.Lines.Add(AText);
end;

{ TListener }

procedure TListener.OnActivate(Sender: TObject);
begin
  Form1.TellMe(Format('Implementation of event OnActivate on Listener %s', [Self.Name]));
end;

procedure TListener.OnClick(Sender: TObject);
begin
  Form1.TellMe(Format('Implementation of event OnClick on Listener %s', [Self.Name]));
end;

procedure TListener.OnDblClick(Sender: TObject);
begin
  Form1.TellMe(Format('Implementation of event OnDblClick on Listener %s', [Self.Name]));
end;

procedure TListener.OnResize(Sender: TObject);
begin
  Form1.TellMe(Format('Implementation of event OnResize on Listener %s', [Self.Name]));
end;

procedure TListener.OnShow(Sender: TObject);
begin
  Form1.TellMe(Format('Implementation of event OnShow on Listener %s', [Self.Name]));
end;

end.
