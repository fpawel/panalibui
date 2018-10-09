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

        procedure SetRowChecked(row: Integer; v: Boolean);
        procedure ToggleRowChecked(row: Integer);

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

uses stringgridutils, stringutils, UnitFormPopup, UnitServerApp, serverapp_msg;

function col2place(col: Integer): Integer;
begin
    Result := col - 2;
end;

function row2var(row: Integer): Integer;
begin
    Result := row - 1;
end;

function place2col(place: Integer): Integer;
begin
    Result := place + 2;
end;

function var2row(varIndex: Integer): Integer;
begin
    Result := varIndex + 1;
end;

function pvk(place, varIndex: Integer): string;
begin
    Result := inttostr(place) + '_' + inttostr(varIndex);
end;

function pvkk(col, row: Integer): string;
begin
    Result := pvk(col2place(col), row2var(row));
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
    varInd := row2var(StringGrid2.row);
    place := col2place(StringGrid2.col);
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
    place := col2place(ACol);
    var_ind := row2var(ARow);

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
    ARow: Integer;
    v: Boolean;

begin
    g := Sender as TStringGrid;
    if (g.row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := FVars[row2var(g.Selection.Top)].FUnchecked;
        for ARow := g.Selection.Top to g.Selection.Bottom do
            SetRowChecked(ARow, not v);
    end;

    if ord(Key) = 1 then
    begin
        v := FVars[row2var(g.Selection.Top)].FUnchecked;
        for ARow := 2 to StringGrid2.RowCount - 1 do
            SetRowChecked(ARow, not v);

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
                        ToggleRowChecked(ARow);

                    Checked := not FVars[row2var(ARow)].FUnchecked;
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
    SetRowChecked(StringGrid2.row, not CheckBox2.Checked);
    StringGrid2.SetFocus;
end;

procedure TFormReadVars.Init(d: TInitData);
var
    ACol, ARow, place, varInd: Integer;
begin
    FVars := d.FVars;
    FErrors.Clear;
    with StringGrid2 do
    begin
        RowCount := length(d.FVars) + 1;
        ColCount :=  length(d.FNetwork)  + 2;
        FixedRows := 1;
        Cells[0, 0] := '№';
        Cells[1, 0] := 'Параметр';
        for varInd := 0 to length(d.FVars) - 1 do
        begin
            ARow := var2row(varInd);
            Cells[0, ARow] := inttostr(d.FVars[varInd].FVar);
            Cells[1, ARow] := d.FVars[varInd].FName;
        end;

        for place := 0 to length(d.FNetwork)-1 do
        begin
            ACol := place2col(place);
            Cells[ACol, 0] := inttostr(d.FNetwork[place].FAddr);
        end;
    end;
end;

procedure TFormReadVars.reset;
var
    prev_place, prev_var: Integer;
begin
    prev_place := FPlace;
    prev_var := FVarIndex;
    FPlace := -1;
    FVarIndex := -1;
    if (prev_place >= 0) and (prev_var >= 0) then
        StringGrid_RedrawCell(StringGrid2, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid2, place2col(FPlace),
          var2row(FVarIndex));

end;

procedure TFormReadVars.HandleReadVar(x: TReadVar);
var
    prev_place, prev_var: Integer;
begin
    prev_place := FPlace;
    prev_var := FVarIndex;
    FPlace := x.FPlace;
    FVarIndex := x.FVarIndex;
    if x.FError <>'' then
        FErrors.AddOrSetValue(pvk(FPlace, FVarIndex), x.FError);

    StringGrid2.Cells[place2col(FPlace), var2row(FVarIndex)] := floattostr(x.FValue);

    if (prev_place >= 0) and (prev_var >= 0) then
        StringGrid_RedrawCell(StringGrid2, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid2, place2col(FPlace),
          var2row(FVarIndex));

end;

procedure TFormReadVars.SetRowChecked(row: Integer; v: Boolean);
begin
    FVars[row2var(row)].FUnchecked := v;
    ServerApp.SendMsg(msgSetVarChecked, row2var(row), lParam(v) );
    StringGrid_RedrawRow(StringGrid2, row);
end;

procedure TFormReadVars.ToggleRowChecked(row: Integer);
begin
    SetRowChecked(row, not FVars[row2var(row)].FUnchecked);

end;

end.
