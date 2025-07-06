object Demo_Piano_View: TDemo_Piano_View
  Left = 0
  Top = 0
  Width = 490
  Height = 388
  TabOrder = 0
  OnResize = FormResize
  object Panel_Legenda: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 36
    Align = alTop
    TabOrder = 0
    object Legenda: TListView
      Left = 1
      Top = 1
      Width = 488
      Height = 34
      Hint = 'Legenda of used MIDI channels'
      Align = alClient
      BorderStyle = bsNone
      Color = clMenu
      Columns = <>
      ReadOnly = True
      ParentShowHint = False
      ShowColumnHeaders = False
      ShowHint = False
      SmallImages = Channel_Colors
      StateImages = Channel_Colors
      TabOrder = 0
      ViewStyle = vsSmallIcon
    end
  end
  object Panel_Event: TPanel
    Left = 0
    Top = 347
    Width = 490
    Height = 41
    Align = alBottom
    TabOrder = 1
    Visible = False
  end
  object Panel_Roll: TPanel
    Left = 75
    Top = 36
    Width = 415
    Height = 311
    Align = alClient
    TabOrder = 2
    object Image: TImage
      Left = 1
      Top = 1
      Width = 413
      Height = 309
      Align = alClient
      AutoSize = True
      ExplicitLeft = 5
      ExplicitTop = 5
    end
    object Bevel_Track: TBevel
      Left = 10
      Top = 0
      Width = 3
      Height = 306
      Shape = bsLeftLine
      Style = bsRaised
    end
  end
  object Panel_Front: TPanel
    Left = 0
    Top = 36
    Width = 75
    Height = 311
    Align = alLeft
    TabOrder = 3
    object Image_Front: TImage
      Left = 1
      Top = 1
      Width = 73
      Height = 309
      Align = alClient
      AutoSize = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 137
    end
  end
  object Channel_Colors: TImageList
    Masked = False
    Left = 416
    Top = 91
  end
end
