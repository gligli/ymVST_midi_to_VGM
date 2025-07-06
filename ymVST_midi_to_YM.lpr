program ymVST_midi_to_YM;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
 {$IFDEF HASAMIGA}
 athreads,
 {$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, MainForm, ymexport, ymsynth;

{$R *.res}

begin
 RequireDerivedFormResource := True;
 Application.Scaled := True;
 {$PUSH}{$WARN 5044 OFF}
 Application.MainFormOnTaskbar := True;
 {$POP}
 Application.Initialize;
 Application.CreateForm(TFormMain, FormMain);
 Application.Run;
end.

