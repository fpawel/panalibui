unit UnitFormReadVars;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ExtCtrls, System.Generics.collections, model_network;

type
    TStringGridEx = class helper for TStringGrid
    public
        function GetInplaceEditor(): TInplaceEdit;
    end;

    TFormReadVars = class(TForm)
        StringGrid1: TStringGrid;
        procedure StringGrid1DblClick(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
        procedure FormCreate(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
          const Value: string);
    private
        { Private declarations }
        Last_Edited_Col, Last_Edited_Row: Integer;

        FPlace: Integer;
        FVarIndex: Integer;
        FErrors: TDictionary<string, string>;
        FNetwork: TNetwork;



        procedure SetRowChecked(row: Integer; v: Boolean);
        procedure ToggleRowChecked(row: Integer);
        procedure ToggleColChecked(col: Integer);

    public
        { Public declarations }

        function FormatAddrPlace(place,varindex:integer):string;


        procedure Init(ANetwork: TNetwork);
        procedure HandleReadVar(X: TReadVar);
        procedure reset;
    end;

var
    FormReadVars: TFormReadVars;

implementation

{$R *.dfm}

uses stringgridutils, stringutils, UnitFormPopup, UnitServerApp, serverapp_msg;

function TStringGridEx.GetInplaceEditor: TInplaceEdit;
begin
    Result := InplaceEditor; // get access to InplaceEditor
end;

function col2place(col: Integer): Integer;
begin
    Result := col - 1;
end;

function row2var(row: Integer): Integer;
begin
    Result := row - 1;
end;

function place2col(place: Integer): Integer;
begin
    Result := place + 1;
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
    FErrors := TDictionary<string, string>.Create;

end;

procedure TFormReadVars.StringGrid1DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    place, varInd: Integer;
    k: string;
    ACol, ARow: Integer;

begin

    with StringGrid1 do
    begin
        GetCursorPos(pt);
        pt := ScreenToClient(pt);
        MouseToCell(pt.X, pt.Y, ACol, ARow);

        if (ACol=0) AND (ARow = 0) then
        begin
            ServerApp.SendMsg(msgToggle, 0,0);
            exit;
        end;


        varInd := row2var(ACol);
        place := col2place(ARow);
        k := pvkk(ACol, ARow);
        r := CellRect(ACol, ARow);
        pt := ClientToScreen(r.BottomRight);
    end;

    if FErrors.ContainsKey(k) then
        with FormPopup do
        begin
            RichEdit1.Font.Color := clRed;
            RichEdit1.Text := FErrors[k];
            Left := pt.X + 5;
            Top := pt.Y + 5;
            Show;
        end;

end;

procedure TFormReadVars.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    X, Y: Integer;
    txt_width, txt_height: double;
    s: string;
    Checked_col, Checked_row: Boolean;
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
    cnv.Font.Assign(self.Font);
    

    Checked_row := false;
    if var_ind > -1 then
        Checked_row := not FNetwork.FVars[var_ind].FUnchecked;

    Checked_col := false;
    if place > -1 then
        Checked_col := not FNetwork.FPlaces[place].FUnchecked;

    if gdFixed in State then
        cnv.Brush.Color := cl3DLight
    else if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if Checked_row and Checked_col then
        cnv.Brush.Color := grd.Color
    else
        cnv.Brush.Color := clBtnFace;

    if (place >= 0) and (var_ind >= 0) and (place = FPlace) and
      (var_ind = FVarIndex) then
        cnv.Brush.Color := clInfoBk;

    if FErrors.ContainsKey(pvk(place, var_ind)) then
        cnv.Font.Color := clRed;

    if cnv.TextWidth(s) + 3 > Rect.Width then
        s := cut_str(s, cnv, Rect.Width);

    txt_width := cnv.TextWidth(s);
    txt_height := cnv.TextHeight(s);

    X := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol <> 1) then
        X := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, X, Y, s);

    if (ACol = 0) and (ARow > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked_row);

    if (ARow = 0) and (ACol > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked_col);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormReadVars.StringGrid1KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    ARow: Integer;
    v: Boolean;

begin
    g := Sender as TStringGrid;
    if (g.row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := FNetwork.FVars[row2var(g.Selection.Top)].FUnchecked;
        for ARow := g.Selection.Top to g.Selection.Bottom do
            SetRowChecked(ARow, not v);
    end;

    if ord(Key) = 1 then
    begin
        v := FNetwork.FVars[row2var(g.Selection.Top)].FUnchecked;
        for ARow := 2 to StringGrid1.RowCount - 1 do
            SetRowChecked(ARow, not v);

    end;

end;

procedure TFormReadVars.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    ACol, ARow: Integer;
    Rect: TRect;
begin
    if (GetAsyncKeyState(VK_LBUTTON) >= 0) then
        exit;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if ((ACol = 0) and (ARow = 0)) OR ((ACol <> 0) AND (ARow <> 0)) then
        exit;

    Rect := StringGrid1.CellRect(ACol, ARow);

    if X > (Rect.Right + Rect.Left) div 2 then
        with StringGrid1 do
        begin
            fixedrows := 0;
            if True then

            col := ACol;
            row := ARow;
            Options := Options + [goEditing];
            EditorMode := true;
            with GetInplaceEditor() do
            begin
                SelStart := 0;
                SelLength := Length(Cells[ACol, ARow]);
            end;
            exit;
        end;

    if ACol = 0 then
    begin
        ToggleRowChecked(ARow);
        StringGrid_RedrawRow(StringGrid1, ARow);
    end
    else
    begin
        ToggleColChecked(ACol);
        StringGrid_RedrawCol(StringGrid1, ACol);
    end;
end;

procedure TFormReadVars.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    with Sender as TStringGrid do
    begin
        // When selecting a cell
        if EditorMode then
        begin // It was a cell being edited
            EditorMode := false; // Deactivate the editor
            // Do an extra check if the LastEdited_ACol and LastEdited_ARow are not -1 already.
            // This is to be able to use also the arrow-keys up and down in the Grid.
            if (Last_Edited_Col <> -1) and (Last_Edited_Row <> -1) then
                StringGrid1SetEditText(grd, Last_Edited_Col, Last_Edited_Row,
                  Cells[Last_Edited_Col, Last_Edited_Row]);
            // Just make the call
        end;
        // Do whatever else wanted
        Options := Options - [goEditing];

    end;

end;

procedure TFormReadVars.StringGrid1SetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
    s: string;
    v: extended;
    n: Integer;
begin

    With StringGrid1 do
        // Fired on every change
        if Not EditorMode // goEditing must be 'True' in Options
        then
        begin // Only after user ends editing the cell
            Last_Edited_Col := -1; // Indicate no cell is edited
            Last_Edited_Row := -1; // Indicate no cell is edited
            // Do whatever wanted after user has finish editing a cell
            fixedrows := 1;
            if ARow = 0 then
            begin
                if TryStrToInt(Value, n) and (n > 0) and (n < 256) then
                    ServerApp.MustSendUserMsg(msgSetAddr, col2place(ACol), n)
                else
                    ServerApp.MustSendUserMsg(msgPeer, 0, 0);
            end;

            if ACol = 0 then
            begin
                if TryStrToInt(Value, n) and (n > -1) then
                    ServerApp.MustSendUserMsg(msgSetvar, row2var(ARow), n)
                else
                    ServerApp.MustSendUserMsg(msgPeer, 0, 0);
            end;
        end
        else
        begin // The cell is being editted
            Last_Edited_Col := ACol; // Remember column of cell being edited
            Last_Edited_Row := ARow; // Remember row of cell being edited
        end;

end;

procedure TFormReadVars.Init(ANetwork: TNetwork);
var
    ACol, ARow, place, varInd: Integer;
begin
    if Assigned(FNetwork) then
    begin
        for place := 0 to Length(FNetwork.FPlaces) - 1 do
            FNetwork.FPlaces[place].Free;
        for varInd := 0 to Length(FNetwork.FVars) - 1 do
            FNetwork.FVars[varInd].Free;
        FNetwork.Free;
    end;

    FNetwork := ANetwork;
    FErrors.Clear;
    with StringGrid1 do
    begin
        RowCount := Length(FNetwork.FVars) + 1;
        fixedrows := 1;
        ColCount := Length(FNetwork.FPlaces) + 1;
        Cells[0, 0] := '�';
        Cells[1, 0] := '��������';
        for varInd := 0 to Length(FNetwork.FVars) - 1 do
        begin
            ARow := var2row(varInd);
            Cells[0, ARow] := inttostr(FNetwork.FVars[varInd].FVar);
        end;

        for place := 0 to Length(FNetwork.FPlaces) - 1 do
        begin
            ACol := place2col(place);
            Cells[ACol, 0] := inttostr(FNetwork.FPlaces[place].FAddr);
        end;
    end;
    StringGrid_Redraw(StringGrid1);
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
        StringGrid_RedrawCell(StringGrid1, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(FPlace),
          var2row(FVarIndex));

end;

procedure TFormReadVars.HandleReadVar(X: TReadVar);
var
    prev_place, prev_var: Integer;
begin
    prev_place := FPlace;
    prev_var := FVarIndex;
    FPlace := X.FPlace;
    FVarIndex := X.FVarIndex;
    if X.FError <> '' then
    begin
        FErrors.AddOrSetValue(pvk(FPlace, FVarIndex), X.FError);
        StringGrid1.Cells[place2col(FPlace), var2row(FVarIndex)] := X.FError;
    end
    else
    begin
        FErrors.Remove(pvk(FPlace, FVarIndex));
        StringGrid1.Cells[place2col(FPlace), var2row(FVarIndex)] :=
          floattostr(X.FValue);
    end;

    if (prev_place >= 0) and (prev_var >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(FPlace),
          var2row(FVarIndex));

end;

procedure TFormReadVars.SetRowChecked(row: Integer; v: Boolean);
begin
    FNetwork.FVars[row2var(row)].FUnchecked := v;
    ServerApp.SendMsg(msgSetVarChecked, row2var(row), lParam(v));
    StringGrid_RedrawRow(StringGrid1, row);
end;

procedure TFormReadVars.ToggleRowChecked(row: Integer);
begin
    SetRowChecked(row, not FNetwork.FVars[row2var(row)].FUnchecked);
end;

procedure TFormReadVars.ToggleColChecked(col: Integer);
begin
    FNetwork.FPlaces[col2place(col)].FUnchecked := not FNetwork.FPlaces
      [col2place(col)].FUnchecked;
    ServerApp.SendMsg(msgSetPlaceChecked, col2place(col),
      lParam(FNetwork.FPlaces[col2place(col)].FUnchecked));
    StringGrid_RedrawCol(StringGrid1, col);
end;

function TFormReadVars.FormatAddrPlace(place, varindex:integer):string;
var cl,ro:integer;
    s1, s2:string;
begin
    cl := place2col(place);
    ro := var2row(varindex);
    if (cl >-1) AND (cl < StringGrid1.ColCount) then
        s1 := StringGrid1.Cells[place2col(place), 0]
    else
        s1 := format('�%d', [place+1]);
    if (ro > -1 ) AND (ro < StringGrid1.RowCount) then
        s2 := StringGrid1.Cells[ 0, var2row(varindex)]
    else
        s2 := format('�%d', [varindex+1]);
    result := s1 + ': ' + s2;
end;


end.
