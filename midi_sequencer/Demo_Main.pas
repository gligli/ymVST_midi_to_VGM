unit Demo_Main;

{
   Copyright 1998-2011 Arnold Reinders

   This file is part of the MIDI Sequencer Components (MSC).

   MIDI Sequencer Components (MSC) is free software: you can redistribute it
   and/or modify it under the terms of the Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the License,
   or (at your option) any later version.

   MIDI Sequencer Components (MSC) is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License and the
   Lesser General Public License along with MIDI Sequencer Components (MSC).
   If not, see <http://www.gnu.org/licenses/>.

   Any changes in the MIDI Sequencer Components (MSC) should be reported
   to Arnold Reinders at musoft@musoft.com
}

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   MPlayer, StdCtrls, ComCtrls, ImgList, Menus,
   ExtCtrls, Buttons, Printers, Grids, Spin, ToolWin,
   MSC_Definitions, MSC_In_Device,
   MSC_Container, MSC_Player, MSC_Controllers,
   MSC_Sequencer, MSC_View_Piano_Roll, MSC_View_Events,
   MSC_View_Slider,
   utilities_logging,
   Demo_Slider, Demo_Resources_View, Demo_Controller_View;

type
   TDemo_Form = class (TForm)
      OpenDialog: TOpenDialog;
      SaveDialog: TSaveDialog;
      Image_List: TImageList;
      Pager: TPageControl;
      Tab_Events: TTabSheet;
      Tab_Res_Out: TTabSheet;
      Tab_Res_In: TTabSheet;
      Menu_Main: TMainMenu;
      Menu_File: TMenuItem;
      File_Open: TMenuItem;
      File_Save_As: TMenuItem;
      Menu_Edit: TMenuItem;
      Menu_View: TMenuItem;
      View_Events: TMenuItem;
      N1: TMenuItem;
      View_MIDI: TMenuItem;
      View_Input: TMenuItem;
      View_Output: TMenuItem;
      View_Controllers: TMenuItem;
      Menu_PLay: TMenuItem;
      Play_Play: TMenuItem;
      Play_Stop: TMenuItem;
      Play_Pause: TMenuItem;
      Play_Step: TMenuItem;
      Play_Rewind: TMenuItem;
      Play_Record: TMenuItem;
      Status: TStatusBar;
      Tab_Controllers: TTabSheet;
      State_Images: TImageList;
      File_Exit: TMenuItem;
      Tab_Raw_MIDI: TTabSheet;
      Raw_MIDI_Viewer: TRichEdit;
      N3: TMenuItem;
      Tab_Piano_Roll: TTabSheet;
      Edit_Clear: TMenuItem;
      File_Print: TMenuItem;
      N2: TMenuItem;
      Panel2: TPanel;
      Label3: TLabel;
      Edit_Controller: TSpinEdit;
      Edit_Val: TEdit;
      Label4: TLabel;
      Button_Assign: TButton;
      Panel3: TPanel;
      Panel4: TPanel;
      GroupBox3: TGroupBox;
      Panel5: TPanel;
      Panel_Status: TPanel;
      L_BPM: TLabel;
      Label_BPM: TLabel;
      L_Beats: TLabel;
      Label_Beats: TLabel;
      L_Denom: TLabel;
      Label_Denominator: TLabel;
      L_Key: TLabel;
      L_Mood: TLabel;
      Label_Key: TLabel;
      Label_Major: TLabel;
      L_Tick: TLabel;
      L_Time: TLabel;
      Label_Tick: TLabel;
      Label_Time: TLabel;
      L_realtime: TLabel;
      Label_Real_Time: TLabel;
      L_Duration: TLabel;
      Label_Duration: TLabel;
      GroupBox2: TGroupBox;
      Track_Tempo: TTrackBar;
      Panel1: TPanel;
      Panel_Slider: TPanel;
      Panel6: TPanel;
      Grid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;

      procedure FormCreate (Sender: TObject);
      procedure FormDestroy (Sender: TObject);

      procedure OnTime (Sender: TObject; time,  current_event: Int32);

      procedure File_OpenClick (Sender: TObject);
      procedure File_Save_AsClick (Sender: TObject);
      procedure File_PrintClick(Sender: TObject);
      procedure File_ExitClick (Sender: TObject);
      procedure Edit_ClearClick(Sender: TObject);
      procedure View_EventsClick (Sender: TObject);
      procedure View_MIDIClick (Sender: TObject);
      procedure View_InputClick (Sender: TObject);
      procedure View_OutputClick (Sender: TObject);
      procedure View_ControllersClick (Sender: TObject);
      procedure Play_PlayClick (Sender: TObject);
      procedure Play_StopClick (Sender: TObject);
      procedure Play_PauseClick (Sender: TObject);
      procedure Play_StepClick (Sender: TObject);
      procedure Play_RewindClick (Sender: TObject);
      procedure Play_RecordClick (Sender: TObject);

      procedure Position_Change (Sender: TObject);
      procedure Track_TempoChange (Sender: TObject);
      procedure Button_AssignClick(Sender: TObject);
      procedure FormActivate(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);

   private
      active: boolean;

   protected
      MIDI_Sequencer: TMIDI_Sequencer;
      MIDI_Resources_View_In: TDemo_Resources_View;
      MIDI_Resources_View_Out: TDemo_Resources_View;
      Demo_Controller_View: TDemo_Controller_View;
      Event_Viewer: TView_Events;
      Piano_Viewer: TView_Piano_Roll;
      Slide_Viewer: TView_Slider;

      procedure xeq_record;
      procedure xeq_play;
      procedure xeq_pause;
      procedure xeq_step;
      procedure xeq_stop;
      procedure xeq_rewind;
      procedure container_properties;
   end; // Class: TDemo_Form //

var
  Demo_Form: TDemo_Form;

implementation

{$R *.DFM}

procedure TDemo_Form.FormCreate (Sender: TObject);
begin
   log_start ('MSC_Demo.log');
   MIDI_Sequencer := TMIDI_Sequencer.Create (Self);
   MIDI_Sequencer.OnTimeChange := OnTime;

   MIDI_Resources_View_In  := TDemo_Resources_View.Create (Tab_Res_In);
   MIDI_Resources_View_In.Parent := Tab_Res_In;
   MIDI_Resources_View_In.Align  := alClient;
   MIDI_Resources_View_In .MIDI_Res := MIDI_Sequencer.MIDI_Res_In;

   MIDI_Resources_View_Out := TDemo_Resources_View.Create (Tab_Res_Out);
   MIDI_Resources_View_Out.Parent := Tab_Res_Out;
   MIDI_Resources_View_Out.Align  := alClient;
   MIDI_Resources_View_Out.MIDI_Res := MIDI_Sequencer.MIDI_Res_Out;
   MIDI_Resources_View_Out.Show;

   Demo_Controller_View := TDemo_Controller_View.Create (Tab_Controllers);
   Demo_Controller_View.Parent := Tab_Controllers;
   Demo_Controller_View.Align  := alClient;
   Demo_Controller_View.Holder := MIDI_Sequencer.MIDI_Controllers;
   Demo_Controller_View.Show;

   MIDI_Sequencer.connect_MIDI_Thru (0, 0, True);

   Label_BPM        .Caption := '';
   Label_Beats      .Caption := '';
   Label_Denominator.Caption := '';
   Label_Key        .Caption := '';
   Label_Major      .Caption := '';
   Label_Tick       .Caption := '';
   Label_Time       .Caption := '';
   Label_Real_Time  .Caption := '';
   Label_Duration   .Caption := '';
end; // FormCreate //

procedure TDemo_Form.FormActivate (Sender: TObject);
var eTab: Int32;
begin
   if not active then
   begin
      active := True;

      Piano_Viewer := TView_Piano_Roll.Create (Tab_Piano_Roll);
      Piano_Viewer.Parent   := Tab_Piano_Roll;
      Piano_Viewer.Align    := alClient;
      Piano_Viewer.Raw_Copy := Raw_MIDI_Viewer.Lines;
      Piano_Viewer.Color_Odd  := clWhite;//clGradientInactiveCaption;
      Piano_Viewer.Color_Even := clGradientActiveCaption;
      Piano_Viewer.Subscriber.Publisher := MIDI_Sequencer.MIDI_Container;
      Application.ProcessMessages;

      Event_Viewer := TView_Events.Create (Panel4);
      Event_Viewer.Parent := Panel4;
      Event_Viewer.Align  := alClient;
      Event_Viewer.Subscriber.Publisher := MIDI_Sequencer.MIDI_Container;

      Slide_Viewer := TView_Slider.Create (Panel_Slider);
      Slide_Viewer.Parent   := Panel_Slider;
      Slide_Viewer.Align    := alClient;
      Slide_Viewer.Ticks    := 10;
      Slide_Viewer.OnChange := Position_Change;
      Slide_Viewer.Subscriber.Publisher := MIDI_Sequencer.MIDI_Container;

      for eTab := 0 to Pager.PageCount - 1 do
      begin
         Pager.TabIndex := eTab;
         Application.ProcessMessages;
      end;
      Pager.TabIndex := 0;
   end; // if
end; // FormActivate //

procedure TDemo_Form.FormDestroy (Sender: TObject);
var delay: uInt32;
begin
   MIDI_Sequencer.Status := seq_Stop;
   delay := GetTickCount + 100;
   while GetTickCount < delay do Application.ProcessMessages;
   log_halt;
end; // FormDestroy //

procedure TDemo_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   MIDI_Sequencer.Status := seq_Stop;
   Application.ProcessMessages;
end;

procedure TDemo_Form.OnTime (Sender: TObject; time, current_event: Int32);
var s: string;
begin
   Label_Time.Caption := Format ('%d', [time]);
   s := MIDI_Sequencer.MIDI_Container.status;
end; // OnTime //

procedure TDemo_Form.File_OpenClick (Sender: TObject);
var view: TStringList;
begin
   if OpenDialog.Execute then
   begin
      view := TStringList.Create;
      Raw_MIDI_Viewer.Text := '';
      Status.SimpleText := 'Loading ' + OpenDialog.FileName + '...';
      MIDI_Sequencer.Reset;
      MIDI_Sequencer.LoadFromFile (OpenDialog.FileName, view);
      MIDI_Sequencer.MIDI_Player.BPM := MIDI_Sequencer.MIDI_Container.BPM;
      MIDI_Sequencer.MIDI_Container.set_max_time_to_container;
      Raw_MIDI_Viewer.Lines.AddStrings (view);
      container_properties;
      Status.SimpleText := 'Loaded ' + OpenDialog.FileName;
      view.Free;
   end; // if
end; // File_OpenClick //

procedure TDemo_Form.File_Save_AsClick (Sender: TObject);
begin
   if SaveDialog.Execute then
   begin
      MIDI_Sequencer.SaveToFile (SaveDialog.FileName);
      Status.SimpleText := SaveDialog.FileName + ' saved...';
   end; // if
end; // File_Save_AsClick //

procedure TDemo_Form.File_PrintClick (Sender: TObject);
var i, h: Int32;
begin
   with Printer do
   begin
      BeginDoc;
      try
         for i := 0 to Raw_MIDI_Viewer.Lines.Count - 1 do
         begin
            h := Canvas.TextHeight (Raw_MIDI_Viewer.Lines.Strings [i]);
            Canvas.TextOut (100, 100 + i * h, Raw_MIDI_Viewer.Lines.Strings [i]);
         end; // for
      finally
         EndDoc;
      end; // try
   end; // with
end; // File_PrintClick //

procedure TDemo_Form.File_ExitClick (Sender: TObject);
begin
   Application.Terminate;
end; // File_ExitClick //

procedure TDemo_Form.Edit_ClearClick (Sender: TObject);
begin
   MIDI_Sequencer.Reset;
   Raw_MIDI_Viewer.Lines.Clear;
end; // Edit_ClearClick //

procedure TDemo_Form.View_EventsClick (Sender: TObject);
begin
//   MIDI_Events_View.show_events;
end; // View_EventsClick //

procedure TDemo_Form.View_MIDIClick (Sender: TObject);
begin
   Pager.ActivePage := Tab_Events;
end; // View_MIDIClick //

procedure TDemo_Form.View_InputClick (Sender: TObject);
begin
   Pager.ActivePage := Tab_Res_In;
end; // View_InputClick //

procedure TDemo_Form.View_OutputClick (Sender: TObject);
begin
   Pager.ActivePage := Tab_Res_Out;
end; // View_OutputClick //

procedure TDemo_Form.View_ControllersClick (Sender: TObject);
begin
   Pager.ActivePage := Tab_Controllers;
end; // View_ControllersClick //

procedure TDemo_Form.Play_PlayClick (Sender: TObject);
begin
   Status.SimpleText := 'Playing: ' +  MIDI_Sequencer.MIDI_Container.status;
   xeq_play;
end; // Play_PlayClick //

procedure TDemo_Form.Play_StopClick (Sender: TObject);
begin
   Status.SimpleText := '';
   xeq_stop;
   MIDI_Sequencer.MIDI_Container.status;
end; // Play_StopClick //

procedure TDemo_Form.Play_PauseClick (Sender: TObject);
begin
   Status.SimpleText := 'Pausing...';
   xeq_pause;
end; // Play_PauseClick //

procedure TDemo_Form.Play_StepClick (Sender: TObject);
begin
   Status.SimpleText := 'Stepping...';
   xeq_step;
end; // Play_StepClick //

procedure TDemo_Form.Play_RewindClick (Sender: TObject);
begin
   Status.SimpleText := 'Rewinding...';
   xeq_rewind;
end; // Play_RewindClick //

procedure TDemo_Form.Play_RecordClick (Sender: TObject);
begin
   Status.SimpleText := 'Recording...';
   xeq_record;
end; // Play_RecordClick //

procedure TDemo_Form.Position_Change (Sender: TObject);
var temp: TNotifyEvent;
    pos: Int32;
begin
// Disable the change routine during the update of the position
// This prevents the update generating new onchange events
   temp := Slide_Viewer.OnChange;
   try
      Slide_Viewer.OnChange := nil;
      pos := Slide_Viewer.Position;
      MIDI_Sequencer.MIDI_Player.MIDI_Ticks := pos;
   finally
      Slide_Viewer.OnChange := temp;
   end; // try
end; // Position_Change //

procedure TDemo_Form.Track_TempoChange (Sender: TObject);
var temp: TNotifyEvent;
begin
   temp := Track_Tempo.OnChange;
   Track_Tempo.OnChange := nil;
   MIDI_Sequencer.BPM := Track_Tempo.Position;
   Status.SimpleText  := MIDI_Sequencer.MIDI_Container.status;
   Label_BPM .Caption := IntToStr (MIDI_Sequencer.BPM);
   Track_Tempo.OnChange := temp;
end; // Track_TempoChange //

{
procedure TDemo_Form.midi_seq_initiated_bpm_change (Sender: TObject);
var bpm: Int32;
begin
   bpm := TMIDI_Sequencer (Sender).BPM;
   Label_BPM.Caption := IntToStr (bpm);
   Track_Tempo.Position := bpm;
end; // tempo_bar_initiated_bpm_change //
}
procedure TDemo_Form.Button_AssignClick (Sender: TObject);
var controller: TMIDI_Controller;
    value, cntrl: Int32;
begin
   cntrl := Edit_Controller.Value;
   value := StrToInt (Edit_Val.Text);
   controller := MIDI_Sequencer.MIDI_Controllers [cntrl];
   MIDI_Sequencer.MIDI_Controllers.Value [cntrl]:= value;
   Grid.Cells [0, 0] := IntToStr (cntrl);
   Grid.Cells [1, 0] := IntToStr (MIDI_Sequencer.MIDI_Controllers.Value    [cntrl]);
   Grid.Cells [2, 0] := IntToStr (MIDI_Sequencer.MIDI_Controllers.Byte_Val [cntrl]);
   if controller.cnt_Type = ct14Bit then
   begin
      cntrl := controller.Aux_ID;
      Grid.Cells [0, 1] := IntToStr (cntrl);
      Grid.Cells [1, 1] := IntToStr (MIDI_Sequencer.MIDI_Controllers.Value    [cntrl]);
      Grid.Cells [2, 1] := IntToStr (MIDI_Sequencer.MIDI_Controllers.Byte_Val [cntrl]);
   end else
   begin
      Grid.Cells [0, 1] := '';
      Grid.Cells [1, 1] := '';
      Grid.Cells [2, 1] := '';
   end; // if
end; // Button_AssignClick //

procedure TDemo_Form.container_properties;
var max: Int32;
begin
   label6 .Caption := IntToStr (MIDI_Sequencer.MIDI_Container.Ticks_Per_Quarter);
   label7 .Caption := IntToStr (MIDI_Sequencer.MIDI_Player.Ticks_Per_32nd);
   label8 .Caption := IntToStr (MIDI_Sequencer.MIDI_Container.Micro_SPQ);
   label10.Caption := IntToStr (MIDI_Sequencer.MIDI_Container.Division);

   max := MIDI_Sequencer.MIDI_Container.Max_Time;
   Label_BPM        .Caption := IntToStr (MIDI_Sequencer.BPM);
   Label_Beats      .Caption := IntToStr (MIDI_Sequencer.Beats);
   Label_Denominator.Caption := IntToStr (MIDI_Sequencer.Rythm);
   Label_Key        .Caption := WideString (MIDI_Note_Names [MIDI_Sequencer.Key]);
   Label_Tick       .Caption := '0';
   Label_Time       .Caption := '0';
   Label_Real_Time  .Caption := '0';
   Label_Duration   .Caption := Format ('%d - %.3fs', [max,
          MIDI_Sequencer.MIDI_Container.time_to_seconds (max)]);
   if MIDI_Sequencer.MIDI_Container.Major
      then Label_Major.Caption := 'Major'
      else Label_Major.Caption := 'Minor';
end; // container_properties //

procedure TDemo_Form.xeq_record;
begin
   MIDI_Sequencer.Status := seq_Record;
end; // xeq_record //

procedure TDemo_Form.xeq_play;
begin
   MIDI_Sequencer.Status := seq_Play;
end; // xeq_play //

procedure TDemo_Form.xeq_pause;
begin
   MIDI_Sequencer.Status := seq_Pause;
end; // xeq_pause //

procedure TDemo_Form.xeq_step;
begin
   MIDI_Sequencer.Status := seq_Step;
end; // xeq_step //

procedure TDemo_Form.xeq_stop;
begin
   MIDI_Sequencer.Status := seq_Stop;
end; // xeq_stop //

procedure TDemo_Form.xeq_rewind;
begin
   MIDI_Sequencer.Status := seq_Rewind;
end; // xeq_rewind //

end. // Unit: Demo //
