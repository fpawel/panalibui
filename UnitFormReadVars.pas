unit UnitFormReadVars;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ExtCtrls, System.Generics.collections, model_initdata, model_network;

type
    TFormReadVars = class(TForm)
        StringGrid2: TStringGrid;
        CheckBox2: TCheckBox;
        procedure CheckBox2Click(Sender: TObject);
        procedure StringGrid2DblClick(Sender: TObject);
        procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid2KeyPress(Sender: TObject; var Key: Char);
        procedure FormCreate(Sender: TObject);
        procedure StringGrid2SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
    private
        { Private declarations }

    public
        { Public declarations }
        FPlace: Integer;
        FVarIndex: Integer;
        FErrors: TDictionary<string, string>;
        FVars: TArray<TDeviceVar>;

        procedure Init(d: TInitData);
        procedure HandleReadVar(x: TReadVar);
        procedure reset;
    end;

var
    FormReadVars: TFormReadVars;

implementation

{$R *.dfm}

uses stringgridutils, stringutils, UnitFormPopup;

function nplace(col: Integer): Integer;
begin
    Result := col - 2;
end;

function nvar(row: Integer): Integer;
begin
    Result := row - 3;
end;

function pvk(place, varIndex: Integer): string;
begin
    Result := inttostr(place) + '_' + inttostr(varIndex);
end;

function pvkk(col, row: Integer): string;
begin
    Result := pvk(nplace(col), nvar(row));
end;

procedure TFormReadVars.FormCreate(Sender: TObject);
begin
    FPlace := -1;
    FVarIndex := -1;
    with CheckBox2 do
    begin
        Visible := false;
        Caption := '';
        Width := 15;
        Height := 15;
    end;
    FErrors := TDictionary<string, string>.Create;

end;

procedure TFormReadVars.StringGrid2DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    place, varInd: Integer;
    k: string;
begin
    varInd := nvar(StringGrid2.row);
    place := nplace(StringGrid2.col);
    with StringGrid2 do
    begin
        k := pvkk(col, row);
        r := CellRect(col, row);
        pt := ClientToScreen(r.BottomRight);
    end;

    if FErrors.ContainsKey(k) then
        with FormPopup do
        begin
            RichEdit1.Font.Color := clRed;
            RichEdit1.Text := FErrors[k];
            Left := pt.x + 5;
            Top := pt.Y + 5;
            Show;
        end;

end;

procedure TFormReadVars.StringGrid2DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    x, Y: Integer;
    txt_width, txt_height: double;
    s: string;
    Checked: Boolean;
    // pv: RProductVarOrder;
    place, var_ind: Integer;
const
    lineColor: TColor = $00BCBCBC;
begin
    place := nplace(ACol);
    var_ind := nvar(ARow);

    grd := TStringGrid(Sender);
    s := grd.Cells[ACol, ARow];
    if (ACol = 0) and (ARow > 0) then
        s := '';
    cnv := grd.Canvas;
    cnv.Font := grd.Font;
    if (ARow > 0) and (ACol = 1) then
    begin
        cnv.Font.Size := 10;
        cnv.Font.Color := clNavy;
    end;

    Checked := false;
    if var_ind > -1 then
        Checked := not FVars[var_ind].FUnchecked;

    if gdFixed in State then
        cnv.Brush.Color := cl3DLight
    else if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if Checked then
        cnv.Brush.Color := grd.Color
    else
        cnv.Brush.Color := clBtnFace;

    if (place >= 0) and (var_ind >= 0) and (place = FPlace) and
      (var_ind = FVarIndex) then
        cnv.Brush.Color := clInfoBk;

    if FErrors.ContainsKey(pvk(place, var_ind)) then
    begin
        cnv.Brush.Color := clBlack;
        cnv.Font.Color := clYellow;
        cnv.Font.Size := 10;
        if gdSelected in State then
            cnv.Brush.Color := clGray;
    end;

    if cnv.TextWidth(s) + 3 > Rect.Width then
        s := cut_str(s, cnv, Rect.Width);

    txt_width := cnv.TextWidth(s);
    txt_height := cnv.TextHeight(s);

    x := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol <> 1) then
        x := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, x, Y, s);

    if (ACol = 0) and (ARow > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormReadVars.StringGrid2KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    i: Integer;
    v: Boolean;

begin
    g := Sender as TStringGrid;
    if (g.row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := DataModule1.DeviceVars[g.Selection.Top - 1].FChecked;
        for i := g.Selection.Top to g.Selection.Bottom do
        begin
            DataModule1.InvertDeviceVarChecked(i - 1);
        end;
        StringGrid_Redraw(g);
    end;

    if ord(Key) = 1 then
    begin
        v := DataModule1.InvertVarsChecked;
        for i := 0 to length(DataModule1.DeviceVars) - 1 do
            DataModule1.DeviceVars[i].FChecked := v;
        StringGrid_Redraw(g);
    end;

end;

procedure TFormReadVars.StringGrid2SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    grd := Sender as TStringGrid;

    case ACol of
        0:
            begin
                r := grd.CellRect(ACol, ARow);
                r.Left := r.Left + grd.Left + 10;
                r.Right := r.Right + grd.Left;
                r.Top := r.Top + grd.Top + 7;
                r.Bottom := r.Bottom + grd.Top;

                with CheckBox2 do
                begin

                    OnClick := nil;
                    if GetAsyncKeyState(VK_LBUTTON) < 0 then
                    begin
                        DataModule1.InvertDeviceVarChecked(ARow - 1);
                        StringGrid_Redraw(grd);
                    end;
                    Checked := DataModule1.DeviceVars[ARow - 1].FChecked;
                    OnClick := CheckBox2Click;

                    Left := r.Left - 6;
                    Top := r.Top - 3;
                    Visible := true;
                end;
            end;

    else
        begin
            CheckBox2.Visible := false;

        end;
    end;

end;

procedure TFormReadVars.CheckBox2Click(Sender: TObject);
begin
    DataModule1.UpdateDeviceVarChecked(StringGrid2.row - 1, CheckBox2.Checked);
    StringGrid_Redraw(StringGrid2);
    StringGrid2.SetFocus;
end;

procedure TFormReadVars.SetCurrentParty(Products: TArray<TProduct>);
var
    i, ARow: Integer;
    v: TDeviceVar;
begin

    FProductVarValues.Clear;
    with StringGrid2 do
    begin
        RowCount := length(DataModule1.DeviceVars) + 1;
        ColCount := 2 + length(Products);
        FixedRows := 1;
        Cells[0, 0] := '№';
        Cells[1, 0] := 'Параметр';
        for ARow := 1 to length(DataModule1.DeviceVars) do
        begin
            v := DataModule1.DeviceVars[ARow - 1];
            Cells[0, ARow] := inttostr(v.FVar);
            Cells[1, ARow] := v.FName;
        end;

        for i := 0 to length(Products) - 1 do
        begin
            Cells[i + 2, 0] := inttostr(Products[i].FSerial);
        end;
    end;
end;

procedure TFormReadVars.reset;
var
    PrevProductVar: RProductVarOrder;
begin
    PrevProductVar := FCurentProductVar;
    FCurentProductVar.FProductOrder := -1;
    FCurentProductVar.FVarOrder := -1;
    if (PrevProductVar.FProductOrder >= 0) and (PrevProductVar.FVarOrder >= 0)
    then
        StringGrid_RedrawCell(StringGrid2, PrevProductVar.FProductOrder + 2,
          PrevProductVar.FVarOrder + 1);

    if (FCurentProductVar.FProductOrder >= 0) and
      (FCurentProductVar.FVarOrder >= 0) then
        StringGrid_RedrawCell(StringGrid2, FCurentProductVar.FProductOrder + 2,
          FCurentProductVar.FVarOrder + 1);

end;

procedure TFormReadVars.HandleReadVar(x: TReadVar);
var
    PrevProductVar: RProductVarOrder;

begin
    PrevProductVar := FCurentProductVar;
    FCurentProductVar := x.ProductVarOrder;

    FCurentProductVar.FProductOrder := x.FProductOrder;
    FCurentProductVar.FVarOrder := x.FVarOrder;
    FProductVarValues.AddOrSetValue(x.ProductVarOrder, x.ValueError);
    StringGrid2.Cells[x.ProductVarOrder.FProductOrder + 2,
      x.ProductVarOrder.FVarOrder + 1] := x.ValueError.FValue;

    if (PrevProductVar.FProductOrder >= 0) and (PrevProductVar.FVarOrder >= 0)
    then
        StringGrid_RedrawCell(StringGrid2, PrevProductVar.FProductOrder + 2,
          PrevProductVar.FVarOrder + 1);

    if (FCurentProductVar.FProductOrder >= 0) and
      (FCurentProductVar.FVarOrder >= 0) then
        StringGrid_RedrawCell(StringGrid2, FCurentProductVar.FProductOrder + 2,
          FCurentProductVar.FVarOrder + 1);

end;

end.
