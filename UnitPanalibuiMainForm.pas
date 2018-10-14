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
    THostAppCommand = (cmdError, cmdUserConfig, cmdNetwork, cmdComportOk,
      cmdReadVar, cmdTextMessage);

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
        RichEdit1: TRichEdit;
        PanelComportStatus: TPanel;
        PanelTopBar: TPanel;
        ToolBarParty: TToolBar;
        ToolButtonParty: TToolButton;
        ToolButtonStop: TToolButton;
        PanelTopMessage: TPanel;
        PopupMenu1: TPopupMenu;
        N4: TMenuItem;
        N5: TMenuItem;
        N1: TMenuItem;
        N2: TMenuItem;
        N3: TMenuItem;
        N8: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        Panel1: TPanel;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Label1: TLabel;
        Edit1: TEdit;
        Label2: TLabel;
        Edit2: TEdit;
        Button6: TButton;
        TabSheetConsole: TTabSheet;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Label3: TLabel;
        ComboBox1: TComboBox;
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
    private
        { Private declarations }
        FhWndTip: THandle;

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
    richeditutils;

function CommandsFileName: string;
begin
    result := ExtractFilePath(ParamStr(0)) + '\commands.txt'
end;

procedure TPanalibuiMainForm.FormCreate(Sender: TObject);
begin
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
begin
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := THostAppCommand(Message.WParam);
    case cmd of
        cmdError:
            with PanelComportStatus do
            begin
                Font.Color := clRed;
                Caption := '   ' + StrFromCopydata(cd);
            end;

        cmdComportOk:
            with PanelComportStatus do
            begin
                Font.Color := clNavy;
                Caption := '   ' + StrFromCopydata(cd);
            end;

        cmdUserConfig:
            begin
                response := TJson.JsonToObject<TConfig>(StrFromCopydata(cd));
                PropertiesForm.SetConfig(TConfig(response));
                // cfg.Free;
                Message.Result := 1;
            end;

        cmdNetwork:
            begin
                response := TJson.JsonToObject<TNetwork>(StrFromCopydata(cd));
                FormReadVars.Init(TNetwork(response));
                // network.Free;
                Message.Result := 1;
            end;

        cmdReadVar:
            begin
                response := TJson.JsonToObject<TReadVar>(StrFromCopydata(cd));
                FormReadVars.HandleReadVar(TReadVar(response));
                TReadVar(response).Free;
                Message.Result := 1;

            end;
        cmdTextMessage:
            begin
                response := TJson.JsonToObject<TPanalibTextMessage>
                  (StrFromCopydata(cd));
                // RichEdit_DeleteEmptiLines(RichEdit1);
                // RichEdit1.Lines.Add('');
                SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
                RichEdit1.SelStart := Length(RichEdit1.Text);

                with TPanalibTextMessage(response) do
                begin

                    if FOk then
                        RichEdit_AddText(RichEdit1, now, clBlack, 0,
                          false, FText)
                    else
                        RichEdit_AddText(RichEdit1, now, clRed, cl3dLight,
                          true, FText);
                    Free;
                end;

                SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);

                Message.Result := 1;
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

end.
