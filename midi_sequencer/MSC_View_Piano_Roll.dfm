inherited View_Piano_Roll: TView_Piano_Roll
  Width = 656
  Height = 448
  OnResize = FormResize
  ExplicitWidth = 656
  ExplicitHeight = 448
  object Panel_Event: TPanel
    Left = 0
    Top = 407
    Width = 656
    Height = 41
    Align = alBottom
    TabOrder = 0
    Visible = False
  end
  object Panel_Front: TPanel
    Left = 0
    Top = 36
    Width = 75
    Height = 371
    Align = alLeft
    TabOrder = 1
    object Image_Front: TImage
      Left = 1
      Top = 1
      Width = 73
      Height = 369
      Align = alClient
      AutoSize = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 137
    end
  end
  object Panel_Legenda: TPanel
    Left = 0
    Top = 0
    Width = 656
    Height = 36
    Align = alTop
    TabOrder = 2
    object Legenda: TListView
      Left = 1
      Top = 1
      Width = 654
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
  object Panel_Roll: TPanel
    Left = 75
    Top = 36
    Width = 581
    Height = 371
    Align = alClient
    TabOrder = 3
    object Image: TImage
      Left = 1
      Top = 1
      Width = 579
      Height = 369
      Align = alClient
      AutoSize = True
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 413
      ExplicitHeight = 309
    end
    object Bevel_Track: TBevel
      Left = 6
      Top = 5
      Width = 1
      Height = 306
      Shape = bsLeftLine
      Style = bsRaised
    end
  end
  object Channel_Colors: TImageList
    Masked = False
    Left = 416
    Top = 91
  end
end
