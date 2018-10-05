program panalibui;

uses
  Vcl.Forms,
  UnitPanalibuiMainForm in 'UnitPanalibuiMainForm.pas' {PanalibuiMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPanalibuiMainForm, PanalibuiMainForm);
  Application.Run;
end.
