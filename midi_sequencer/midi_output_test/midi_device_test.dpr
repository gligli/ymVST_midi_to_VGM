program midi_device_test;

uses
  Forms,
  Main in 'Main.pas' {Example},
  MSC_Definitions in '..\MSC_Definitions.pas',
  MSC_Device in '..\MSC_Device.pas',
  MSC_Out_Device in '..\MSC_Out_Device.pas',
  MSC_Circular_Buffer in '..\MSC_Circular_Buffer.pas',
  utilities_math in '..\utilities_math.pas',
  MSC_Component in '..\MSC_Component.pas',
  MSC_Delphi_MIDI_Callback in '..\MSC_Delphi_MIDI_Callback.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExample, Example);
  Application.Run;
end.
