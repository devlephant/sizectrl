object ImSelect: TImSelect
  Left = 0
  Top = 0
  Caption = 'Image Select'
  ClientHeight = 327
  ClientWidth = 368
  Color = clBtnFace
  Constraints.MinHeight = 248
  Constraints.MinWidth = 368
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 368
    Height = 272
    Align = alClient
    Center = True
    ExplicitWidth = 356
  end
  object Panel1: TPanel
    Left = 0
    Top = 272
    Width = 368
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      368
      55)
    object BitBtn3: TBitBtn
      Left = 281
      Top = 23
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 89
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 2
      OnClick = BitBtn2Click
    end
    object BitBtn4: TBitBtn
      Left = 200
      Top = 23
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object CheckBox1: TCheckBox
      Left = 135
      Top = 2
      Width = 81
      Height = 17
      Anchors = []
      Caption = 'Resize Image'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
  end
  object opd1: TOpenPictureDialog
    Left = 48
    Top = 200
  end
end
