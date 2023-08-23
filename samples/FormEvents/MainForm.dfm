object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 676
  ClientWidth = 1120
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnActivate = FormActivate
  OnClick = FormClick
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnShow = FormShow
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 616
    Width = 129
    Height = 25
    Caption = 'Add Listener'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 1120
    Height = 593
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 1106
  end
  object btnInvalidAddListener: TButton
    Left = 143
    Top = 616
    Width = 129
    Height = 25
    Caption = 'Add Invalid Listener'
    TabOrder = 2
    OnClick = btnInvalidAddListenerClick
  end
end
