program panalibui;

uses
  Vcl.Forms,
  UnitPanalibuiMainForm in 'UnitPanalibuiMainForm.pas' {PanalibuiMainForm},
  findproc in 'utils\findproc.pas',
  runhostapp in 'runhostapp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPanalibuiMainForm, PanalibuiMainForm);
  Application.Run;
end.
