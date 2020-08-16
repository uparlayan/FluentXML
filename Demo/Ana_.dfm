object Ana: TAna
  Left = 0
  Top = 0
  Caption = 'Ana'
  ClientHeight = 587
  ClientWidth = 898
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 898
    Height = 49
    Align = alTop
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    ExplicitWidth = 635
    object bt_Kitap: TButton
      Left = 9
      Top = 9
      Width = 64
      Height = 31
      Align = alLeft
      Caption = 'Kitap'
      TabOrder = 0
      OnClick = KlikManager
    end
    object bt_EFatura: TButton
      Left = 73
      Top = 9
      Width = 72
      Height = 31
      Align = alLeft
      Caption = 'E-Fatura'
      TabOrder = 1
      OnClick = KlikManager
    end
  end
  object MM: TMemo
    Left = 0
    Top = 49
    Width = 898
    Height = 538
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 635
    ExplicitHeight = 250
  end
end
