object FormReadVars: TFormReadVars
  Left = 0
  Top = 0
  Caption = 'FormReadVars'
  ClientHeight = 507
  ClientWidth = 874
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid2: TStringGrid
    Left = 0
    Top = 0
    Width = 874
    Height = 507
    Align = alClient
    BorderStyle = bsNone
    ColCount = 3
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
    OnDblClick = StringGrid2DblClick
    OnDrawCell = StringGrid2DrawCell
    OnKeyPress = StringGrid2KeyPress
    OnSelectCell = StringGrid2SelectCell
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24)
  end
  object CheckBox2: TCheckBox
    Left = 28
    Top = 91
    Width = 200
    Height = 34
    Caption = 'CheckBox1'
    Constraints.MinWidth = 200
    TabOrder = 1
    OnClick = CheckBox2Click
  end
end
