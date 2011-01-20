object FmProjectSettings: TFmProjectSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project settings'
  ClientHeight = 178
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblDirectories: TLabel
    Left = 8
    Top = 13
    Width = 64
    Height = 13
    Caption = 'LblDirectories'
  end
  object LbDirectories: TListBox
    Left = 8
    Top = 39
    Width = 329
    Height = 97
    ItemHeight = 13
    TabOrder = 0
  end
  object BtnAdd: TButton
    Left = 265
    Top = 8
    Width = 33
    Height = 25
    Caption = '+'
    TabOrder = 1
    OnClick = BtnAddClick
  end
  object BtnRemove: TButton
    Left = 304
    Top = 8
    Width = 33
    Height = 25
    Caption = '-'
    TabOrder = 2
    OnClick = BtnRemoveClick
  end
  object BtnOk: TButton
    Left = 181
    Top = 142
    Width = 75
    Height = 25
    Caption = 'BtnOk'
    TabOrder = 3
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 262
    Top = 142
    Width = 75
    Height = 25
    Caption = 'BtnCancel'
    TabOrder = 4
    OnClick = BtnCancelClick
  end
  object OpenDialog: TOpenDialog
    FileName = 'FOLDER'
    Filter = '*.*|*.*'
    Options = [ofPathMustExist, ofEnableSizing]
    Left = 80
  end
end
