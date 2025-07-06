unit MSC_Sequencer;

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
   Windows, Messages, SysUtils, Classes,
   MSC_Definitions, MSC_Component, MSC_In_Device, MSC_Resources, MSC_Container,
   MSC_Player, MSC_Recorder, MSC_Controllers;

type
   TSequencer_Status = (seq_Play, seq_Pause, seq_Step, seq_Record, seq_Rewind,
                        seq_Stop, seq_Generate);
   TPatch_Names = array [T7bit] of string;

   TMIDI_Sequencer = class (TMSC_Component)
   private
      FMIDI_Container: TMIDI_Container;
      FMIDI_Res_In: TMIDI_Resources_In;
      FMIDI_Res_Out: TMIDI_Resources_Out;
      FMIDI_Player: TMIDI_Player;
      FMIDI_Recorder: TMIDI_Recorder;
      FMIDI_Controllers: TController_Holder;
      FStatus: TSequencer_Status;
      FInstrument_Name: TPatch_Names;
      FDrum_Name:       TPatch_Names;
      FController_Name: TPatch_Names;

      procedure send_controller (cnt: T7bit; channel: TMIDI_Range; value: T7bit);
      function  get_controller (index: T7Bit): TMIDI_Controller;
      procedure set_pitch_bend (channel: TMIDI_Range; value: Int32);
      procedure set_status (value: TSequencer_Status);
      function  get_Port_In_Count: Int32;
      function  get_Port_Out_Count: Int32;
      function  get_bpm: Int32;
      procedure set_bpm (value: Int32);
      function  get_beats: uInt8;
      procedure set_beats (value: uInt8);
      function  get_denominator: uInt8;
      procedure set_denominator (value: uInt8);
      function  get_key: TMIDI_Key_Signature;
      procedure set_key (value: TMIDI_Key_Signature);
      function  get_major: boolean;
      procedure set_major (value: boolean);
      function  get_max_time: Int32;
      procedure set_max_time (time: Int32);
      function  get_Time_Change: TTime_Change_Event;
      procedure set_time_change (value: TTime_Change_Event);
      function  get_prepare: TTime_Change_Event;
      procedure set_prepare (value: TTime_Change_Event);
      procedure set_patch (channel: TMIDI_Range; patch: TMIDI_Patch_Range);
      function  get_patch_name (index: T7bit): string;
      function  get_drum_name (index: T7bit): string;
      function  get_ctrl_name (index: T7bit): string;

   protected
      MIDI_Thru: array of array of boolean;

      procedure do_play;
      procedure do_pause;
      procedure do_step;
      procedure do_record;
      procedure do_rewind;
      procedure do_stop;
      procedure do_Generate;
      procedure Record_MIDI_Input (Sender: TObject);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset;

      procedure load_patch_names (file_name: string);
      procedure load_drum_names  (file_name: string);
      procedure load_ctrl_names  (file_name: string);
      procedure LoadFromFile (file_name: string; raw_copy: TStringList);
      procedure SaveToFile   (file_name: string);
      procedure connect_MIDI_thru (from, towards: Int32; setting: boolean = True);
      function  append_event (event: TMIDI_Event): Int32;
      function  insert_event (event: TMIDI_Event): Int32;

      property MIDI_Container:   TMIDI_Container     read FMIDI_Container;
      property MIDI_Res_In:      TMIDI_Resources_In  read FMIDI_Res_In;
      property MIDI_Res_Out:     TMIDI_Resources_Out read FMIDI_Res_Out;
      property MIDI_Player:      TMIDI_PLayer        read FMIDI_Player;
      property MIDI_Recorder:    TMIDI_Recorder      read FMIDI_Recorder;
      property MIDI_Controllers: TController_Holder  read FMIDI_Controllers;

      property Send_Controller_Event [cnt: T7bit; channel: TMIDI_Range]: T7bit write send_controller;
      property Pitch_Bend [channel: TMIDI_Range]: Int32 write set_pitch_bend;
      property Status: TSequencer_Status read FStatus write set_status;
      property Port_In_Count: Int32  read get_Port_In_Count;
      property Port_Out_Count: Int32 read get_Port_Out_Count;
      property BPM: Int32 read get_bpm write set_bpm;
      property Beats: uInt8 read get_beats write set_beats;
      property Rythm: uInt8 read get_denominator write set_denominator;
      property Key: TMIDI_Key_Signature read get_key write set_key;
      property Major: boolean read get_major write set_major;
      property Max_Time: Int32 read get_max_time write set_max_time;
      property Patch [channel: TMIDI_Range]: TMIDI_Patch_Range write set_patch;
      property Patch_Name [index: T7bit]: string read get_patch_name;
      property Drum_Name  [index: T7bit]: string read get_drum_name;
      property Ctrl_Name  [index: T7bit]: string read get_ctrl_name;

      property OnTimeChange: TTime_Change_Event
               read get_Time_Change write set_time_change;
      property OnPrepare: TTime_Change_Event
               read get_prepare write set_prepare;
   end; // Class: TMIDI_Sequencer //

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents(MSC_Package_Name, [TMIDI_Sequencer]);
end; // Register //

function get_item (var s: string; substr: string): string;
var p, len: longint;
    store: string;
begin
   p := Pos (substr, s);
   if p = 0 then
   begin
      store := trim (copy (s, 1, length (s)));
      s := '';
   end else
   begin
      len := Length (substr);
      store := trim (Copy (s, 1, p - 1));
      s     := trim (copy (s, p + len, Length (s) - p - len + 1));
   end; // if
   Result := store;
end; // get_item //

{*******************************************************************
*                                                                  *
* Class TMIDI_Sequencer                                            *
*                                                                  *
********************************************************************}

constructor TMIDI_Sequencer.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

   FStatus := seq_Stop;
   if not (csDesigning in ComponentState) then
   begin
      FMIDI_Container   := TMIDI_Container    .Create;
//      FMIDI_Res_In      := TMIDI_Resources_In .Create (Self);
      FMIDI_Res_Out     := TMIDI_Resources_Out.Create (Self);
      FMIDI_Player      := TMIDI_Player       .Create (Self);
      FMIDI_Recorder    := TMIDI_Recorder     .Create (Self);
      FMIDI_Controllers := TController_Holder .Create;

      MIDI_Player.MIDI_Res_Out     := MIDI_Res_Out;
      MIDI_Player.MIDI_Container   := MIDI_Container;
      MIDI_Recorder.MIDI_Res_In    := MIDI_Res_In;
      MIDI_Recorder.MIDI_Container := MIDI_Container;
      MIDI_Recorder.OnMidiInput    := Record_MIDI_Input;

      Reset;
   end; // if
end; // Create //

destructor TMIDI_Sequencer.Destroy;
begin
   if not (csDesigning in ComponentState) then
   begin
      FMIDI_Player.stop_now;
      FMIDI_Controllers.Free;
      FMIDI_Recorder   .Free;
      FMIDI_Player     .Free;
      FMIDI_Res_Out    .Free;
      FMIDI_Res_In     .Free;
      FMIDI_Container  .Free;
   end; // if

   inherited Destroy;
end; // Destroy //

procedure TMIDI_Sequencer.Reset;
// Clear all MIDI events from the container and reset the sequencer

var i, j: Int32;
begin
   Status := seq_Stop;
   if (Port_In_Count > 0) and (Port_Out_Count > 0) then
   begin
      SetLength (MIDI_Thru, Port_In_Count, Port_Out_Count);
      for i := 0 to Port_In_Count  - 1 do
      for j := 0 to Port_Out_Count - 1 do
      begin
         MIDI_Thru [i, j] := False;
      end; // for
   end; // if
   MIDI_Container.Clear;
end; // Clear //

procedure TMIDI_Sequencer.load_patch_names (file_name: string);
var infile: TextFile;
    index: Int32;
    line: string;
begin
   try
      AssignFile (infile, file_name);
      System.Reset (infile);
      try
         while not SeekEOF (infile) do
         begin
            ReadLn (infile, line);
            index := StrToInt (get_item (line, ' '));
            get_item (line, ']');
            FInstrument_Name [index] := Trim (line);
         end; // while
      finally
         CloseFile (infile);
      end; // try
   except
      on exception do ; // nothing
   end;
end; // load_patch_name //

procedure TMIDI_Sequencer.load_drum_names (file_name: string);
var infile: TextFile;
    index: Int32;
    line, number: string;
begin
   try
      AssignFile (infile, file_name);
      System.Reset (infile);
      try
         while not SeekEOF (infile) do
         begin
            ReadLn (infile, line);
            number := line;
            index := StrToInt (get_item (number, '='));
            FDrum_Name [index] := Trim (line);
         end; // while
      finally
         CloseFile (infile);
      end; // try
   except
      on exception do ; // nothing
   end;
end; // load_drum_names //

procedure TMIDI_Sequencer.load_ctrl_names (file_name: string);
var infile: TextFile;
    index: Int32;
    line: string;
begin
   try
      AssignFile (infile, file_name);
      System.Reset (infile);
      try
         index := 0;
         while not SeekEOF (infile) do
         begin
            ReadLn (infile, line);
            get_item (line, ']');
            FController_Name [index] := Trim (line);
            index := index + 1;
         end; // while
      finally
         CloseFile (infile);
      end; // try
   except
      on exception do ; // nothing
   end;
end; // load_controller_names //

procedure TMIDI_Sequencer.send_controller (cnt: T7bit; channel: TMIDI_Range; value: T7bit);
var event: TMIDI_Event;
begin
   event := TMIDI_Event.Create;
   event.Event_Type  := mc_MIDI_Control_Change;
   event.Port        := 0;
   event.Channel     := channel;
   event.Data_Byte_1 := cnt;
   event.Data_Byte_2 := value;
   MIDI_Res_Out.send_MIDI_message (event);
   event.Free;
end; // set_controller //

function  TMIDI_Sequencer.get_controller (index: T7Bit): TMIDI_Controller;
begin
   get_controller := MIDI_Controllers [index];
end; // get_controller //

procedure TMIDI_Sequencer.set_pitch_bend (channel: TMIDI_Range; value: Int32);
var event: TMIDI_Event;
begin
   event := TMIDI_Event.Create;
   event.Event_Type  := mc_MIDI_Pitch_Bend;
   event.Channel     := channel;
   event.Data_Byte_1 := value div C7bit; // MSB
   event.Data_Byte_2 := value mod C7bit; // LSB
   MIDI_Res_Out.send_MIDI_message (event);
   event.Free;
end; // set_pitch_bend //

procedure TMIDI_Sequencer.set_status (value: TSequencer_Status);
begin
   case value of
      seq_Play:      do_play;
      seq_Pause:     do_pause;
      seq_Step:      do_step;
      seq_Record:    do_record;
      seq_Rewind:    do_rewind;
      seq_Stop:      do_stop;
      seq_Generate:  do_generate;
   end; // cdase
end; // set_status //

function TMIDI_Sequencer.get_Port_In_Count: Int32;
begin
   Result := -1;
   if MIDI_Res_In <> nil then Result := MIDI_Res_In.Count;
end; // get_Port_In_Count //

function TMIDI_Sequencer.get_Port_Out_Count: Int32;
begin
   Result := -1;
   if MIDI_Res_Out <> nil then Result := MIDI_Res_Out.Count;
end; // get_Port_Out_Count //

function TMIDI_Sequencer.get_bpm: Int32;
begin
   Result := MIDI_Container.BPM;
end; // get_bpm //

procedure TMIDI_Sequencer.set_bpm (value: Int32);
begin
   MIDI_Player.BPM := value;
end; // set_bpm //

function  TMIDI_Sequencer.get_beats: uInt8;
begin
   Result := MIDI_Container.Beats;
end; // get_beats //

procedure TMIDI_Sequencer.set_beats (value: uInt8);
begin
   MIDI_Container.Beats := value;
end; // set_beats //

function  TMIDI_Sequencer.get_denominator: uInt8;
begin
   Result := MIDI_Container.Rythm;
end; // get_denominator //

procedure TMIDI_Sequencer.set_denominator (value: uInt8);
begin
   MIDI_Container.Rythm := value;
end; // set_denominator //

function  TMIDI_Sequencer.get_key: TMIDI_Key_Signature;
begin
   Result := MIDI_Container.Key;
end; // get_key //

procedure TMIDI_Sequencer.set_key (value: TMIDI_Key_Signature);
begin
   MIDI_Container.Key := value;
end; // set_key //

function  TMIDI_Sequencer.get_major: boolean;
begin
   Result := MIDI_Container.Major;
end; // get_major //

procedure TMIDI_Sequencer.set_major (value: boolean);
begin
   MIDI_Container.Major := value;
end; // set_major //

function TMIDI_Sequencer.get_max_time: Int32;
begin
   Result := MIDI_Container.Max_Time;
end; // get_major //

procedure TMIDI_Sequencer.set_max_time (time: Int32);
begin
   MIDI_Container.Max_Time := time;
end; // set_max_time //

function TMIDI_Sequencer.get_time_change: TTime_Change_Event;
begin
   Result := MIDI_Player.OnTimeChange;
end; // get_time_change //

procedure TMIDI_Sequencer.set_time_change (value: TTime_Change_Event);
begin
   MIDI_Player.OnTimeChange := value;
end; // set_time_change //

procedure TMIDI_Sequencer.set_patch (channel: TMIDI_Range; patch: TMIDI_Patch_Range);
var event: TMIDI_Event;
begin
   event := TMIDI_Event.Create;
   event.Event_Type  := mc_MIDI_Program_Change;
   event.Channel     := channel;
   event.Data_Byte_1 := patch;
   MIDI_Res_Out.send_MIDI_message (event);
   event.Free;
end; // set_patch //

function TMIDI_Sequencer.get_prepare: TTime_Change_Event;
begin
   Result := MIDI_Player.OnPrepare;
end; // get_prepare //

procedure TMIDI_Sequencer.set_prepare (value: TTime_Change_Event);
begin
   MIDI_Player.OnPrepare := value;
end; // set_prepare //

function TMIDI_Sequencer.get_patch_name (index: T7bit): string;
begin
   Result := FInstrument_Name [index];
end; // get_patch_name //

function TMIDI_Sequencer.get_drum_name (index: T7bit): string;
begin
   Result := FDrum_Name [index];
end; // get_drum_name //

function TMIDI_Sequencer.get_ctrl_name (index: T7bit): string;
begin
   Result := FController_Name [index];
end; // get_controller_name //

procedure TMIDI_Sequencer.do_play;
begin
   FStatus := seq_Play;
   if MIDI_Recorder.Recording then MIDI_Recorder.Recording := False;
   MIDI_Container.Trace := True;
   MIDI_PLayer.play_start;
end; // do_play //

procedure TMIDI_Sequencer.do_pause;
begin
   MIDI_Container.Trace := False;
   FStatus := seq_Pause;
   MIDI_Player.play_pause;
end; // do_pause //

procedure TMIDI_Sequencer.do_step;
begin
   MIDI_Container.Trace := True;
   FStatus := seq_Step;
   MIDI_Player.play_step;
   FStatus := seq_Pause;
end; // do_step //

procedure TMIDI_Sequencer.do_record;
begin
   MIDI_Container.Trace := False;
   FStatus := seq_Record;
   if MIDI_Player.is_playing then MIDI_Player.play_stop;
   MIDI_Recorder.Recording := True;
end; // do_record //

procedure TMIDI_Sequencer.do_rewind;
begin
   MIDI_Player.play_rewind;
end; // do_rewind //

procedure TMIDI_Sequencer.do_stop;
begin
   MIDI_Container.Trace := False;
   if MIDI_Player.is_playing or MIDI_player.is_pausing
      then MIDI_Player.play_stop;
   if MIDI_Recorder.Recording then MIDI_Recorder.Recording := False;
   FStatus := seq_Stop;
end; // do_stop //

procedure TMIDI_Sequencer.do_generate;
begin
   FStatus := seq_Generate;
   if MIDI_Recorder.Recording then MIDI_Recorder.Recording := False;
   MIDI_Container.Trace := True;
   MIDI_PLayer.play_generate;
end; // do_generate //

procedure TMIDI_Sequencer.LoadFromFile (file_name: string; raw_copy: TStringList);
begin
   MIDI_Container.LoadFromFile (file_name, raw_copy);
end; // LoadFromFile //

procedure TMIDI_Sequencer.SaveToFile (file_name: string);
begin
   MIDI_Container.SaveToFile (file_name);
end; // SaveToFile //

procedure TMIDI_Sequencer.connect_MIDI_thru (from, towards: Int32; setting: boolean);
begin
   if (from >= 0)   and (from    < Port_In_Count) and
      (towards >=0) and (towards < Port_Out_Count)
      then MIDI_Thru [from, towards] := setting;
end; // connect_MIDI_thru //

function TMIDI_Sequencer.append_event (event: TMIDI_Event): Int32;
begin
   Result := MIDI_Container.store_event (event);
end; // append_event //

function TMIDI_Sequencer.insert_event (event: TMIDI_Event): Int32;
begin
   Result := MIDI_Container.store_event (event);
end; // insert_event //

procedure TMIDI_Sequencer.Record_MIDI_Input (Sender: TObject);
var event: TMIDI_Event;
    i: Int32;
begin
   with (Sender as TMIDI_Device_Input) do
   begin
      while (MIDI_Recorder.Recording) and (MessageCount > 0) do
      begin
         event := GetMidiEvent;
         if MIDI_Container <> nil then
         begin
            MIDI_Container.store_event (event);
         end; // if
         if (Port_In_Count > 0) and (Port_Out_Count > 0) then
         begin
            for i := 0 to Port_Out_Count - 1 do
               if MIDI_Thru [DeviceID, i]
                  then MIDI_Res_Out.send_MIDI_message_to_port (i, event);
         end; // if
      end; // while
   end; // with
end; // MIDI_RecorderMidiInput //

end. // Unit: MSC_Sequencer //
