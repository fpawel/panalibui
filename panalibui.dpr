program panalibui;

uses
  Vcl.Forms,
  UnitPanalibuiMainForm in 'UnitPanalibuiMainForm.pas' {PanalibuiMainForm},
  findproc in 'utils\findproc.pas',
  runhostapp in 'runhostapp.pas',
  vclutils in 'utils\vclutils.pas',
  model_config in 'models\model_config.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  stringutils in 'utils\stringutils.pas',
  PropertyValueEditors in 'settings\PropertyValueEditors.pas',
  listports in 'utils\listports.pas',
  PropertiesFormUnit in 'settings\PropertiesFormUnit.pas' {PropertiesForm},
  model_initdata in 'models\model_initdata.pas',
  model_network in 'models\model_network.pas',
  UnitFormReadVars in 'UnitFormReadVars.pas' {FormReadVars},
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  UnitServerApp in 'UnitServerApp.pas' {ServerApp: TDataModule},
  serverapp_msg in 'messages\serverapp_msg.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerApp, ServerApp);
  Application.CreateForm(TPanalibuiMainForm, PanalibuiMainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TFormReadVars, FormReadVars);
  Application.CreateForm(TFormPopup, FormPopup);
  Application.Run;
end.
