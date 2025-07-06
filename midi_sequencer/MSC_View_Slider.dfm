inherited View_Slider: TView_Slider
  Height = 51
  Ctl3D = False
  ParentCtl3D = False
  OnResize = FrameResize
  ExplicitHeight = 51
  object Slider: TTrackBar
    Left = 0
    Top = 0
    Width = 499
    Height = 25
    Margins.Left = 0
    Margins.Right = 0
    Align = alTop
    Frequency = 5
    TabOrder = 0
    TickMarks = tmBoth
    TickStyle = tsNone
  end
  object Panel: TPanel
    Left = 0
    Top = 25
    Width = 499
    Height = 26
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object Image: TImage
      Left = 0
      Top = 0
      Width = 499
      Height = 26
      Align = alClient
      Transparent = True
      ExplicitLeft = 224
      ExplicitTop = 16
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
end
