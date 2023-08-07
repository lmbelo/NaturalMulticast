program FormEvents1;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  Natural.EventImplementation in '..\..\src\Natural.EventImplementation.pas',
  Natural.MultiCast in '..\..\src\Natural.MultiCast.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutDown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
