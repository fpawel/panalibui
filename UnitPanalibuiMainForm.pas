unit UnitPanalibuiMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
    THostAppCommand = (cmdError, cmdConfigComport, cmdConfigNetwork,
      cmdConfigVars, cmdStartReadVars, cmdStopReadVars, cmdReadVar);

    THostAppMessage = (msgPeer, msgStartReadVars, msgStopReadVars);

    TPlace = class
        FAddr: integer;
        FUnchecked: boolean;
    end;

    TVar = class
        FVar: integer;
        FUnchecked: boolean;
    end;

    TComportConfig = class
    public
        FName: string;
        FBoud: integer;
        FReadTimeout: integer;
        FReadByteTimeout: integer;
        FMaxAttemptsRead: integer;

    end;

    TPanalibuiMainForm = class(TForm)
        Button1: TButton;
        Label1: TLabel;
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        hWndServer: HWND;
    public
        { Public declarations }
        procedure WndProc(var Message: TMessage); override;
    end;

var
    PanalibuiMainForm: TPanalibuiMainForm;

implementation

{$R *.dfm}

uses rest.json, runhostapp, json;

procedure TPanalibuiMainForm.FormCreate(Sender: TObject);
begin
    hWndServer := FindWindow('PanalibHostAppWindowClass', nil);
    if hWndServer = 0 then
        raise Exception.Create('server not found');
    if SendMessage(hWndServer, WM_USER + integer(msgPeer), 0, 0) = 0 then
        raise Exception.Create('server is not responding');

end;

procedure TPanalibuiMainForm.WndProc(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    strResponse: string;
    cmd: THostAppCommand;
    x: TComportConfig;
begin
    inherited;
    if Message.Msg = WM_COPYDATA then
    begin
        cd := PCOPYDATASTRUCT(Message.LParam);
        cmd := THostAppCommand(Message.WParam);
        case cmd of
            cmdConfigComport:
                begin
                    SetString(strResponse, PWideChar(cd.lpData),
                      cd.cbData div 2);
                    x := TJson.JsonToObject<TComportConfig>(strResponse);
                    x.Free;
                    Message.Result := 1;
                end;
        end;
    end;
end;

procedure TPanalibuiMainForm.Button1Click(Sender: TObject);
var
    hWndServer: HWND;
    cd: COPYDATASTRUCT;
    ptr_bytes: TBytes;
    r:LRESULT;
begin
    hWndServer := FindWindow('PanalibHostAppWindowClass', nil);
    if hWndServer = 0 then
    begin
        Label1.Font.Color := clRed;
        Label1.Caption := 'window not found';
        exit;
    end;
    ptr_bytes := WideBytesOf('shalom, вася, шалом!');
    cd.cbData := Length(ptr_bytes);
    cd.lpData := ptr_bytes;

    // SendMessage(hWndServer, WM_COPYDATA, integer(self.Handle), integer(@cd));
    if SendMessage(hWndServer, WM_COPYDATA, Handle, integer(@cd)) = 0 then
        raise Exception.Create('server is not responding');

end;

end.
