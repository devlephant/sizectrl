program DemoSizeCtrl_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  main_fmx in 'main_fmx.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
