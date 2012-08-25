object ViewFrame: TViewFrame
  Left = 192
  Top = 135
  Width = 727
  Height = 333
  Caption = 'ViewFrame'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 668
    Top = 64
  end
end
