unit UnitPanalibuiMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Grids, System.ImageList, Vcl.ImgList,
    UnitServerApp;

type
    THostAppCommand = (cmdError, cmdComport, cmdNetwork, cmdComportOk, cmdReadVar);

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
        Panel1: TPanel;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
    Button5: TButton;
        procedure FormCreate(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    private
        { Private declarations }
        procedure OnWMCopydata(var Message: TMessage);

    public
        { Public declarations }
        procedure WndProc(var Message: TMessage); override;
    end;

var
    PanalibuiMainForm: TPanalibuiMainForm;

implementation

{$R *.dfm}

uses serverapp_msg, rest.json, runhostapp, json, vclutils,
    model_config, PropertiesFormUnit,
    UnitFormReadVars, stringutils, model_network;

procedure TPanalibuiMainForm.Button1Click(Sender: TObject);
begin
    ServerApp.MustSendUserMsg(msgAddDelVar, 0, 0);
end;

procedure TPanalibuiMainForm.Button2Click(Sender: TObject);
begin
    ServerApp.MustSendUserMsg(msgAddDelPlace, 1, 0);
end;

procedure TPanalibuiMainForm.Button3Click(Sender: TObject);
begin
    ServerApp.MustSendUserMsg(msgAddDelPlace, 0, 0);
end;

procedure TPanalibuiMainForm.Button4Click(Sender: TObject);
begin
    ServerApp.MustSendUserMsg(msgAddDelVar, 1, 0);
end;

procedure TPanalibuiMainForm.Button5Click(Sender: TObject);
begin
    ServerApp.CloseServer;
    Close;
end;

procedure TPanalibuiMainForm.FormCreate(Sender: TObject);
begin
    //  SendMessage(hWndServer, WM_CLOSE, 0, 0);
end;

procedure TPanalibuiMainForm.WndProc(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: THostAppCommand;

begin
    inherited;

    case Message.Msg of
        WM_COPYDATA:
            OnWMCopydata(Message);

    end;

end;

procedure TPanalibuiMainForm.OnWMCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: THostAppCommand;
    cfg: TConfig;
    network: TNetwork;
    read_var:TReadVar;
begin
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := THostAppCommand(Message.WParam);
    case cmd of
        cmdError:
            begin
                PanelPartyTopMessage.Font.Color := clRed;
                PanelPartyTopMessage.Caption := StrFromCopydata(cd);
            end;

        cmdComportOk:
            begin
                PanelPartyTopMessage.Font.Color := clNavy;
                PanelPartyTopMessage.Caption := StrFromCopydata(cd);
            end;

        cmdComport:
            begin
                cfg := TJson.JsonToObject<TConfig>(StrFromCopydata(cd));
                PropertiesForm.SetConfig(cfg);
                //cfg.Free;
                Message.Result := 1;
            end;

        cmdNetwork:
            begin
                network := TJson.JsonToObject<Tnetwork>(StrFromCopydata(cd));
                FormReadVars.Init(network);
                //network.Free;
                Message.Result := 1;
            end;

        cmdReadVar:
            begin
                read_var := TJson.JsonToObject<TReadVar>(StrFromCopydata(cd));
                FormReadVars.HandleReadVar(read_var);
                read_var.Free;
                Message.Result := 1;

            end;
    end;
end;

procedure TPanalibuiMainForm.FormShow(Sender: TObject);
begin
    OnShow := nil;
    with PropertiesForm do
    begin
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Parent := TabSheetSettings;

        OnValueChanged := procedure(p: TChangedPropertyValue)
            begin
                ServerApp.MustSendJSON(self.Handle, dmsgSetsProp, p);
            end;
        Show;
    end;

    with FormReadVars do
    begin
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Parent := TabSheetVars;
        Show;
    end;

    ServerApp.MustSendUserMsg(msgPeer, 0, 0);
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
