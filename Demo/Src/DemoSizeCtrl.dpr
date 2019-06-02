program DemoSizeCtrl;

uses
  Forms,
  main in 'main.pas' {MainForm},
  imageselect in 'imageselect.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TImSelect, ImSelect);
  Application.Run;
end.
