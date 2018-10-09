unit UnitPanalibuiMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Grids, System.ImageList, Vcl.ImgList, UnitServerApp;

type
    THostAppCommand = (cmdError, cmdInitPeer, cmdStartReadVars, cmdStopReadVars, cmdReadVar);



    TPlace = class
        FAddr: integer;
        FUnchecked: boolean;
    end;

    TVar = class
        FVar: integer;
        FUnchecked: boolean;
    end;

    TPanalibuiMainForm = class(TForm)
        ImageList1: TImageList;
        ImageList2: TImageList;
        ImageList3: TImageList;
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetProducts: TTabSheet;
        StringGrid1: TStringGrid;
        CheckBox1: TCheckBox;
        TabSheetVars: TTabSheet;
        TabSheetCurrentChart: TTabSheet;
        TabSheetSettings: TTabSheet;
        TabSheetArchive: TTabSheet;
        Panel14: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        PanelConsolePlaceholderBottom: TPanel;
        PanelConsole: TPanel;
        RichEdit1: TRichEdit;
        PanelConsoleHeader: TPanel;
        Panel6: TPanel;
        PanelLastMessage: TPanel;
        ToolBar4: TToolBar;
        ToolButtonMoveConsoleDown: TToolButton;
        ToolButtonConsoleHide: TToolButton;
        PanelTopBar: TPanel;
        ToolBarParty: TToolBar;
        ToolButtonParty: TToolButton;
        ToolButtonStop: TToolButton;
        PanelPartyTopMessage: TPanel;
        PopupMenu1: TPopupMenu;
        N4: TMenuItem;
        N5: TMenuItem;
        N1: TMenuItem;
        N2: TMenuItem;
        N3: TMenuItem;
        N8: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        SplitterConsoleHoriz: TSplitter;
        procedure FormCreate(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        
    public
        { Public declarations }
        procedure WndProc(var Message: TMessage); override;
    end;

var
    PanalibuiMainForm: TPanalibuiMainForm;

implementation

{$R *.dfm}

uses serverapp_msg, rest.json, runhostapp, json, vclutils, model_initdata, model_config, PropertiesFormUnit,
  UnitFormReadVars;

procedure TPanalibuiMainForm.FormCreate(Sender: TObject);
begin
    //
end;

procedure TPanalibuiMainForm.WndProc(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    strResponse: string;
    cmd: THostAppCommand;
    x: TInitData;
begin
    inherited;
    if Message.Msg = WM_COPYDATA then
    begin
        cd := PCOPYDATASTRUCT(Message.LParam);
        cmd := THostAppCommand(Message.WParam);
        case cmd of
            cmdInitPeer:
                begin
                    SetString(strResponse, PWideChar(cd.lpData),
                      cd.cbData div 2);
                    x := TJson.JsonToObject<TInitData>(strResponse);
                    PropertiesForm.SetConfig(x.FConfig);
                    FormReadVars.Init(x);
                    // x.Free;
                    Message.Result := 1;
                end;
        end;
    end;
end;

procedure TPanalibuiMainForm.FormShow(Sender: TObject);
begin
    OnShow := nil;
    with PropertiesForm do
    begin
        BorderStyle := bsNone;
        Align := alClient;
        Parent := TabSheetSettings;

        OnValueChanged := procedure(p:TChangedPropertyValue)
        begin
            ServerApp.MustSendJSON(self.Handle, dmsgSetsProp, p);
        end;
        Show;
    end;

    with FormReadVars do
    begin
        BorderStyle := bsNone;
        Align := alClient;
        Parent := TabSheetVars;
        Show;
    end;

    ServerApp.MustSendUserMsg(msgPeer,0,0);
end;

procedure TPanalibuiMainForm.PageControlMainChange(Sender: TObject);
begin
    (Sender as TPageControl).Repaint;
end;

procedure TPanalibuiMainForm.PageControlMainDrawTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: TRect; Active: boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;



end.
