object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'BLP Viewer'
  ClientHeight = 543
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 55
    Top = 107
    Width = 105
    Height = 105
    Stretch = True
  end
  object Label1: TLabel
    Left = 8
    Top = 107
    Width = 30
    Height = 13
    Caption = 'Image'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 41
    Height = 13
    Caption = 'MipLevel'
  end
  object Label3: TLabel
    Left = 211
    Top = 56
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open BLP'
    TabOrder = 0
    OnClick = Button1Click
  end
  object TrackBar1: TTrackBar
    Left = 55
    Top = 56
    Width = 150
    Height = 45
    TabOrder = 1
    OnChange = TrackBar1Change
  end
  object OpenDialog1: TOpenDialog
    Left = 240
    Top = 8
  end
end
