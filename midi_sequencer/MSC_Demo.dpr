program MSC_Demo;

uses
  Forms,
  Demo_Controller_View in 'Demo_Controller_View.pas',
  Demo_Main in 'Demo_Main.pas' {Demo_Form},
  Demo_Resources_View in 'Demo_Resources_View.pas',
  Demo_Slider in 'Demo_Slider.pas' {Component_Slider: TFrame},
  MSC_Circular_Buffer in 'MSC_Circular_Buffer.pas',
  MSC_Component in 'MSC_Component.pas',
  MSC_Container in 'MSC_Container.pas',
  MSC_Controllers in 'MSC_Controllers.pas',
  MSC_Definitions in 'MSC_Definitions.pas',
  MSC_Delphi_MIDI_Callback in 'MSC_Delphi_MIDI_Callback.pas',
  MSC_Device in 'MSC_Device.pas',
  MSC_In_Device in 'MSC_In_Device.pas',
  MSC_In_File in 'MSC_In_File.pas',
  MSC_Out_Device in 'MSC_Out_Device.pas',
  MSC_Out_File in 'MSC_Out_File.pas',
  MSC_Player in 'MSC_Player.pas',
  MSC_Recorder in 'MSC_Recorder.pas',
  MSC_Resources in 'MSC_Resources.pas',
  MSC_Sequencer in 'MSC_Sequencer.pas',
  MSC_Timer in 'MSC_Timer.pas',
  MSC_View_Base in 'MSC_View_Base.pas' {View_Base: TFrame},
  MSC_View_Piano_Roll in 'MSC_View_Piano_Roll.pas' {View_Piano_Roll: TFrame},
  MSC_View_Events in 'MSC_View_Events.pas' {View_Events: TFrame},
  MSC_View_Slider in 'MSC_View_Slider.pas' {View_Slider: TFrame},
  Publish_Subscribe in 'Publish_Subscribe.pas',
  utilities_logging in 'utilities_logging.pas',
  utilities_math in 'utilities_math.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemo_Form, Demo_Form);
  Application.Run;
end.
