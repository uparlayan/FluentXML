object Ana: TAna
  Left = 0
  Top = 0
  Caption = 'Ana'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 49
    Align = alTop
    Caption = 'Panel'
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    object bt_Olustur: TButton
      Left = 9
      Top = 9
      Width = 64
      Height = 31
      Align = alLeft
      Caption = 'Olu'#351'tur'
      TabOrder = 0
      OnClick = KlikManager
    end
  end
  object MM: TMemo
    Left = 0
    Top = 49
    Width = 635
    Height = 250
    Align = alClient
    Lines.Strings = (
      'MM')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
