unit UnitPanalibuiMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
    TPanalibuiMainForm = class(TForm)
        Button1: TButton;
        Label1: TLabel;
        procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TPanalibuiMainForm.Button1Click(Sender: TObject);
var
    hWndServer: HWND;
    cd: COPYDATASTRUCT;
    ptr_bytes: TBytes;
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

    //SendMessage(hWndServer, WM_COPYDATA, integer(self.Handle), integer(@cd));
    SendMessage(hWndServer, WM_COPYDATA, 100, integer(@cd));

end;



procedure TPanalibuiMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    hWndServer: HWND;
begin
    hWndServer := FindWindow('PanalibHostAppWindowClass', nil);
    if IsWindow(hWndServer) then
    begin
        SendMessage(hWndServer, WM_CLOSE, 0, 0);
    end;

end;

procedure TPanalibuiMainForm.WndProc(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    str:string;
begin
    inherited;
    if Message.Msg = WM_COPYDATA then
    begin
        cd := PCOPYDATASTRUCT(Message.LParam);
        SetString(str, PWideChar(cd.lpData), cd.cbData div 2);
        Label1.Font.Color := clNavy;
        Label1.Caption := TimeToStr(Now) + ' ' + inttoStr(Message.WParam) + ' ' +  str;
    end;
end;

end.
