object Component_Slider: TComponent_Slider
  Left = 0
  Top = 0
  Width = 112
  Height = 237
  TabOrder = 0
  TabStop = True
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 112
    Height = 237
    Align = alClient
    TabOrder = 0
    DesignSize = (
      112
      237)
    object Label1: TLabel
      Left = 57
      Top = 40
      Width = 39
      Height = 13
      Caption = '&Channel'
      FocusControl = Combo_Channel
    end
    object Label2: TLabel
      Left = 57
      Top = 90
      Width = 26
      Height = 13
      Caption = '&Value'
    end
    object Button_CTl_Names: TSpeedButton
      Left = 1
      Top = 1
      Width = 110
      Height = 33
      Align = alTop
      Caption = 'Controller'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label_CaptionClick
      ExplicitLeft = 0
      ExplicitTop = -3
      ExplicitWidth = 129
    end
    object Label_Min: TLabel
      Left = 33
      Top = 40
      Width = 47
      Height = 13
      Caption = 'Label_Min'
    end
    object Label_Max: TLabel
      Left = 33
      Top = 215
      Width = 51
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Label_Max'
      ExplicitTop = 218
    end
    object Combo_Channel: TComboBox
      Left = 57
      Top = 63
      Width = 49
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = '1'
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '10'
        '11'
        '12'
        '13'
        '14'
        '15'
        '16')
    end
    object Edit_Value: TEdit
      Left = 56
      Top = 109
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object Tracker: TTrackBar
      Left = 2
      Top = 32
      Width = 25
      Height = 196
      Anchors = [akLeft, akTop, akBottom]
      Orientation = trVertical
      TabOrder = 2
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TrackerChange
    end
    object Combo_Ctl: TComboBox
      Left = 2
      Top = 5
      Width = 110
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      TabOrder = 3
      Text = 'Combo_Ctl'
      Visible = False
      OnClick = Combo_CtlClick
    end
  end
  object Popup: TPopupMenu
    Left = 80
    Top = 184
  end
end
