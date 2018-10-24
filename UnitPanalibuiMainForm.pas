unit UnitPanalibuiMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Grids, System.ImageList, Vcl.ImgList,
    UnitServerApp;

const
    { User-defined message }
    UM_VALIDATEINPUT = WM_USER + 100;

type
    THostAppCommand = (cmdUserConfig, cmdNetwork, cmdStatusText, cmdConsoleText,
      cmdReadVar);

    TPlace = class
        FAddr: integer;
        FUnchecked: boolean;
    end;

    TVar = class
        FVar: integer;
        FUnchecked: boolean;
    end;

    TPanalibuiMainForm = class(TForm)
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetVars: TTabSheet;
        TabSheetCurrentChart: TTabSheet;
        TabSheetSettings: TTabSheet;
        TabSheetArchive: TTabSheet;
        Panel14: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        PanelStatus: TPanel;
        PanelTopBar: TPanel;
        PopupMenu1: TPopupMenu;
        N4: TMenuItem;
        N5: TMenuItem;
        N1: TMenuItem;
        N2: TMenuItem;
        N3: TMenuItem;
        N8: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        TabSheetConsole: TTabSheet;
        Panel2: TPanel;
    PanelInput: TPanel;
    Panel3: TPanel;
    Panel6: TPanel;
    ComboBox1: TComboBox;
    RichEdit1: TRichEdit;
    PanelNetwork: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    PanelConsoleHeader: TPanel;
    ToolBar4: TToolBar;
    ToolButtonConsoleHide: TToolButton;
    ImageList3: TImageList;
    Splitter2: TSplitter;
    ToolBarParty: TToolBar;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    Button4: TButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ImageList1: TImageList;
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
        procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure ToolButtonConsoleHideClick(Sender: TObject);
    private
        { Private declarations }
        FhWndTip: THandle;

        procedure SetStatusText(Ok: boolean; AText: string);
        procedure AddConsoleText(Ok: boolean; AText: string);

        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

    public
        { Public declarations }

    end;

var
    PanalibuiMainForm: TPanalibuiMainForm;

implementation

{$R *.dfm}

uses serverapp_msg, rest.json, runhostapp, json, vclutils,
    model_config, PropertiesFormUnit,
    UnitFormReadVars, stringutils, model_network, ComponentBaloonHintU,
    richeditutils, UnitFormChartSeries;

function CommandsFileName: string;
begin
    result := ExtractFilePath(ParamStr(0)) + '\commands.txt'
end;

procedure TPanalibuiMainForm.FormCreate(Sender: TObject);
begin
    ToolButtonConsoleHideClick(nil);
    // SendMessage(hWndServer, WM_CLOSE, 0, 0);
    if FileExists(CommandsFileName) then
        ComboBox1.Items.LoadFromFile(CommandsFileName);

end;

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

procedure TPanalibuiMainForm.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    with ComboBox1 do
        case Key of

            VK_DELETE:
                begin
                    if (ssCtrl in Shift) and (Items.IndexOf(Text) > -1) then
                    begin
                        Items.delete(Items.IndexOf(Text));
                        Text := '';
                    end;

                end;
            VK_RETURN:
                begin
                    Text := Trim(Text);
                    if Text <> '' then
                    begin
                        ServerApp.MustSendStr(Handle,
                          dmsgPerformTextCommand, Text);

                        if Items.IndexOf(Text) > -1 then
                            Items.Exchange(Items.IndexOf(Text), 0)
                        else
                            Items.insert(0, Text);
                        Items.SaveToFile(CommandsFileName);
                        Text := '';
                    end;
                    Key := 0;
                end;

        end;
end;

procedure TPanalibuiMainForm.HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: THostAppCommand;
    response: TObject;
    tm: TPanalibTextMessage;
    read_var: TReadVar;
begin
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := THostAppCommand(Message.WParam);
    Message.result := 1;
    case cmd of
        cmdUserConfig:
            begin
                response := TJson.JsonToObject<TConfig>(StrFromCopydata(cd));
                PropertiesForm.SetConfig(TConfig(response));
                // cfg.Free;

            end;

        cmdNetwork:
            begin
                response := TJson.JsonToObject<TNetwork>(StrFromCopydata(cd));
                FormReadVars.Init(TNetwork(response));
                // network.Free;

            end;

        cmdReadVar:
            begin
                read_var := TJson.JsonToObject<TReadVar>(StrFromCopydata(cd));
                FormReadVars.HandleReadVar(read_var);
                if read_var.FError = '' then
                begin


                    SetStatusText(true,
                      Format('%s: %g',
                      [FormReadVars.FormatAddrPlace(read_var.FPlace,
                      read_var.FVarIndex), read_var.FValue]));
                end
                else
                begin
                    SetStatusText(false,
                      Format('%s: %s',
                      [FormReadVars.FormatAddrPlace(read_var.FPlace,
                      read_var.FVarIndex), read_var.FError]));
                end;
                read_var.Free;

            end;
        cmdStatusText:
            begin
                response := TJson.JsonToObject<TPanalibTextMessage>
                  (StrFromCopydata(cd));

                tm := TPanalibTextMessage(response);
                SetStatusText(tm.FOk, tm.FText);
                tm.Free
            end;

        cmdConsoleText:
            begin
                response := TJson.JsonToObject<TPanalibTextMessage>
                  (StrFromCopydata(cd));

                tm := TPanalibTextMessage(response);
                SetStatusText(tm.FOk, tm.FText);
                AddConsoleText(tm.FOk, tm.FText);
                tm.Free
            end;
    end;
end;

procedure TPanalibuiMainForm.FormShow(Sender: TObject);
begin
    OnShow := nil;
    with PropertiesForm do
    begin
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Parent := TabSheetSettings;

        OnValueChanged := procedure(p: TChangedPropertyValue)
            begin
                ServerApp.MustSendJSON(Self.Handle, dmsgSetsProp, p);
            end;
        Show;
    end;

    with FormReadVars do
    begin
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Parent := PanelNetwork;
        Show;
    end;

    with FormChartSeries do
    begin
        Parent := TabSheetVars;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := True;
        Font.Assign(self.Font);
        NewChart;
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

procedure TPanalibuiMainForm.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TPanalibuiMainForm.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TPanalibuiMainForm.SetStatusText(Ok: boolean; AText: string);
begin
    if Ok then
        PanelStatus.Font.Color := clBlack
    else
        PanelStatus.Font.Color := clRed;
    PanelStatus.Caption := AText;
end;

procedure TPanalibuiMainForm.ToolButtonConsoleHideClick(Sender: TObject);
begin
    if PanelInput.Height > 32 then
    begin
        PanelInput.Height := 32;
        ToolButtonConsoleHide.ImageIndex := 1;
        Splitter2.Visible := false;
    end else
    begin
        Splitter2.Visible := true;
                  Splitter2.Top := 0;
        PanelInput.Height := 150;
        ToolButtonConsoleHide.ImageIndex := 0;
    end;


end;

procedure TPanalibuiMainForm.AddConsoleText(Ok: boolean; AText: string);
begin
    with RichEdit1 do
    begin
        SendMessage(Handle, EM_SCROLL, SB_LINEDOWN, 0);
        SelStart := Length(Text);
        if Ok then
        begin
            RichEdit_AddText(RichEdit1, clBlack, AText);

        end
        else
        begin
            RichEdit_AddText2(RichEdit1, clRed, cl3dLight, AText);
        end;
        SendMessage(Handle, EM_SCROLL, SB_LINEDOWN, 0);
    end;
end;

end.
