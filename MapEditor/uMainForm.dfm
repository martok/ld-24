object Form1: TForm1
  Left = 441
  Top = 235
  Width = 657
  Height = 623
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 144
    Top = 8
    Width = 353
    Height = 417
    OnDblClick = PaintBoxDblClick
    OnMouseDown = PaintBoxMouseDown
    OnMouseMove = PaintBoxMouseMove
    OnMouseUp = PaintBoxMouseUp
    OnPaint = PaintBoxPaint
  end
  object WidthEd: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '5'
  end
  object HeightEd: TEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '5'
  end
  object ChangeSizeBt: TButton
    Left = 8
    Top = 56
    Width = 121
    Height = 25
    Caption = 'Change Size'
    TabOrder = 2
    OnClick = ChangeSizeBtClick
  end
  object SaveBt: TButton
    Left = 8
    Top = 152
    Width = 121
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = SaveBtClick
  end
  object LoadBt: TButton
    Left = 8
    Top = 184
    Width = 121
    Height = 25
    Caption = 'Load'
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Left = 24
    Top = 248
  end
  object SaveDialog: TSaveDialog
    Left = 56
    Top = 248
  end
end
