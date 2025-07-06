unit MSC_Player;

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

{
   Timing
   ------
   There are two important parameters ruling MIDI playback: microseconds
   per quarter note and ticks per quarter note. The first one is translated
   to BPM (BPM = 60,000,000 / microseconds). Typically, if the microseconds
   per quarternote is 500,000 then BPM is 120.
   The second parameter is the number of MIDI ticks (clocks) per quarternote
   (PPQN). A typical value is 24 which is three MIDI clock ticks in this
   example. The smallest unit of tempo this squencer can handle is 1/32nd.
   This is kept in the property Time_Unit.

   Implementation
   --------------
   The timer is set to trigger for each MIDI tick. This value is set from
   MIDI_Container.Milli_Seconds_per_Tick. Each trigger increments MIDI_Ticks.
   When enough ticks have been counted for a 32nd note (MIDI_Ticks mod
   Ticks_per_32nd = 0) Time_Unit is incremented by one. Next events that
   have their time less than or equal to Time_Unit are played.
}

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     StdCtrls, ComCtrls, Buttons, ToolWin, ExtCtrls, MMSystem,
     utilities_logging, utilities_math,
     MSC_Timer, MSC_Definitions, MSC_Component, MSC_Container, MSC_Resources;

type
   EMIDI_Player = class (Exception);

   TMIDI_Player = class (TMSC_Component)
   private
      FMIDI_Container: TMIDI_Container;   // Ref to MIDI container
      FMIDI_Res_Out: TMIDI_Resources_Out; // Ref to MIDI output device
      FLoop: boolean;                     // When true, MIDI file is rewound when stop condition is reached
      FTicks_per_32nd: Int32;             // Current 'time' in MIDI ticks
      FMIDI_Ticks: Int32;                 // Current 'time' in milli seconds

      FOnTimeChange: TTime_Change_Event;  // Callback when time in timer changes
      FOnPrepare:    TTime_Change_Event;  // Callback for preparing MIDI container

      Timer: THiResTimer;           // Multi media timer
      playing: boolean;             // Is player playing or not
      stepping: boolean;            // If true, user can manually step thru music
      inside_callback: boolean;     // Prevents the callback being entered twice

      function  get_play_track (port: Int32; channel: TMIDI_Range): boolean;
      procedure set_play_track (port: Int32; channel: TMIDI_Range; val: boolean);
      procedure set_bpm (value: Int32);
      procedure set_loop (value: boolean);
      procedure set_ticks_per_32nd (value: Int32);
      procedure set_MIDI_ticks (new_ticks: Int32);
      function  get_time_units: Int32;
      function  get_ms_per_tick: Int32;
      procedure set_ms_per_tick (ms: Int32);
      procedure set_MIDI_container (value: TMIDI_Container);
      procedure set_MIDI_sound_cards (value: TMIDI_Resources_Out);

   protected
      procedure Notification (AComponent: TComponent; Operation: TOperation); override;
      function can_play: boolean;

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure stop_now;
      procedure Timer_Callback (Sender: TObject);
      procedure Timer_Direct   (Sender: TObject);
      procedure play_start;
      procedure play_stop;
      procedure play_pause;
      procedure play_step;
      procedure play_rewind;
      procedure play_generate;
      function  is_playing: boolean;
      function  is_pausing: boolean;
      procedure patch_change (track: Int32; id: TMIDI_Range; patch: Int8);

      property Ticks_per_32nd: Int32 read FTicks_per_32nd write set_ticks_per_32nd;
      property MIDI_Ticks: Int32 read FMIDI_Ticks write set_MIDI_ticks;
      property Time_Unit: Int32 read get_time_units;
      property Milli_Seconds_per_Tick: Int32 read get_ms_per_tick write set_ms_per_tick;
      property BPM: Int32 write set_bpm;
      property Loop: boolean read FLoop write set_loop;

      property Play_Track [port: Int32; channel: TMIDI_Range]: boolean
               read get_play_track write set_play_track;
      property MIDI_Container: TMIDI_Container
               read FMIDI_Container write set_MIDI_Container;
      property MIDI_Res_Out: TMIDI_Resources_Out
               read FMIDI_Res_Out write set_MIDI_sound_cards;
      property OnTimeChange: TTime_Change_Event
               read FOnTimeChange write FOnTimeChange;
      property OnPrepare: TTime_Change_Event
               read FOnPrepare write FOnPrepare;
   end; // Class: TMIDI_Player //

implementation

{*******************************************************************
*                                                                  *
* Class TMIDI_Player                                               *
*                                                                  *
********************************************************************}

constructor TMIDI_Player.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

   MIDI_Container := nil;
   MIDI_Res_Out   := nil;
   FOnTimeChange  := nil;

   BPM      := 120; // MIDI default
   Loop     := False;
   playing  := False;
   stepping := False;

   Timer            := THiresTimer.Create;
   Timer.Enabled    := False;
   Timer.OnTimer    := Timer_Callback;
   Timer.Interval   := 20;
   Timer.Resolution := 2;
   FTicks_per_32nd  := 0;
   FMIDI_Ticks      := 0;
end; // Create //

destructor TMIDI_Player.Destroy;
begin
   stop_now;
   Timer.Free;

   inherited destroy;
end; // Destroy //

procedure TMIDI_Player.Notification (AComponent: TComponent; Operation: TOperation);
begin
   if Operation = opRemove then
   begin
      if AComponent is TMIDI_Resources_Out then MIDI_Res_Out := nil;
   end; // if
end; // Notification //

procedure TMIDI_Player.stop_now;
begin
   Timer.Enabled := False;
   if playing then
   begin
      while inside_callback
         do Application.ProcessMessages;
   end;
   Application.ProcessMessages;
end; // stop_now //

procedure TMIDI_Player.Timer_Callback (Sender: TObject);
   procedure handle_tempo (s: AnsiString);
   var ms: Int32; // Microseconds per quarternote
   begin
      if Length (s) = 3 then
      begin
         ms := convert_to_cardinal (0, Ord (s [1]), Ord (s [2]), Ord (s [3]));
         BPM := Round (mspq / ms);
      end else
      begin
         BPM := 120;
      end; // if
   end; // handle_tempo //

   procedure check_for_events (time: Int32);
   var event: TMIDI_Event;
       notes_played: boolean;
   begin
      notes_played := False;
      event := MIDI_Container.Event [MIDI_Container.Current_Event];

// Sync external sequencers
      if (Sender <> nil) and (Event <> nil)
         then MIDI_Res_Out.send_sync_signal;

// event's time should be < ticks and the event must exist
      while (event <> nil) and (event.Time <= cardinal (Time_Unit)) do
      begin
         log_write_fmt ('[player]%12d %4d %2x %3d %3d', [gettickcount, event.Time, event.MIDI_Message, event.Data_Byte_1, event.Data_Byte_2]);
         log_writeln;
         if is_note_on (event) then notes_played := True else
         if event.MIDI_Message = mc_MIDI_Meta_Event then
         begin
            case event.Data_Byte_1 of
               mc_Meta_Set_Tempo: handle_tempo (TMeta_Event (event).Text);
            end; // case
         end; // if
         if Play_Track [event.Port, event.Channel] and
            (event.Event_Type < mc_MIDI_System_Message) then
         begin
// when no sender, file is just being saved. Play back is not necessary
            if Sender <> nil then
            begin
               MIDI_Res_Out.send_MIDI_Message (event.Port, event.MIDI_Message,
                                               event.Data_Byte_1, event.Data_Byte_2);
               MIDI_Res_Out.send_sync_message (event.MIDI_Message,
                                               event.Data_Byte_1, event.Data_Byte_2);
            end; // if
         end; // if
         MIDI_Container.Current_Event := MIDI_Container.Current_Event + 1;
         event := MIDI_Container.Event [MIDI_Container.Current_Event];
      end; // while

      if notes_played then
      begin
         MIDI_Container.publish_command (pm_Update_Time);
         if stepping then Timer.Enabled := False;
      end; // if

      if (Time > MIDI_Container.Max_Time) then
      begin
         if loop
            then play_rewind
            else play_stop;
      end; // if
   end; // check_for_events //

begin
   if inside_callback then Exit;
   inside_callback := True;
   MIDI_Container.Current_Time := Time_Unit;
   try
// prepare the notes (first tick of same time)
      log_write_fmt ('[callback] ms = %d, MIDI_Ticks = %d', [GetTickCount, MIDI_Ticks]);
      if (MIDI_Ticks mod Ticks_per_32nd = 0) then
      begin
         log_write_fmt (', mod %d = 0', [Ticks_per_32nd]);
         log_writeln;
         if Assigned (FOnPrepare)
            then FOnPrepare (MIDI_Container, Time_Unit, -1);
         if Assigned (FOnTimeChange)
            then FOnTimeChange (Self, Time_Unit, -1);
      end else

// play the notes (last tick of same time)
      if (MIDI_Ticks mod Ticks_per_32nd = Ticks_per_32nd - 1) then
      begin
         log_write_fmt (', mod %d = %d', [Ticks_per_32nd, Ticks_per_32nd - 1]);
         log_writeln;
         check_for_events (Time_Unit);
      end else
      begin
         log_writeln;
      end; // if

      FMIDI_Ticks := FMIDI_Ticks + 1;
      inside_callback := False;
      Application.ProcessMessages;
   except
      on e:exception do
      begin
         Timer.Enabled := False;
         inside_callback := False;
         raise EMidi_PLayer.Create ('Error ' + e.classname + ' encountered. ' +
                      'Sorry, the music cannot be played. The error was: ' +
                      e.message);
      end; // on
   end; // try
end; // Timer_Callback //

procedure TMIDI_Player.Timer_Direct (Sender: TObject);
begin
// fill out
end; // Time_Direct //

function TMIDI_Player.can_play: boolean;
begin
   if MIDI_Container = nil then
      EMIDI_PLayer.Create ('MIDI_Player can''t play. There is no MIDI ' +
                           'container assigned.');
   if MIDI_Res_Out = nil then
      EMIDI_PLayer.Create ('MIDI_Player can''t play. There are no MIDI ' +
                           'output resources assigned.');
   if MIDI_Res_Out.Count <= 0 then
      EMIDI_PLayer.Create ('MIDI_Player can''t play. There are no MIDI ' +
                           'resources available or free.');
   can_play := true;
end; // can_play //

procedure TMIDI_Player.play_start;
begin
   if can_play then
   begin
      playing  := true;
      stepping := false;
      MIDI_Container.Current_Event := 0;

      log_write_fmt ('[play_start] ms = %d, microspq = %d', [GetTickCount, MIDI_Container.Micro_SPQ]);
      log_writeln;
      inside_callback := False;
      Ticks_per_32nd  := MIDI_Container.Ticks_Per_Quarter div 8;
      Timer.Interval  := Milli_Seconds_per_Tick;
      Timer.Enabled   := True;
      log_write_fmt ('[play_start] ticks/q = %d, MIDI_Ticks = %d, ticks/32 = %d, interval = %d',
                     [MIDI_Container.Ticks_per_Quarter, MIDI_Ticks, Ticks_per_32nd, Timer.Interval]);
      log_writeln;
   end; // if
end; // play_start //

procedure TMIDI_Player.play_pause;
begin
   Timer.Enabled := False;
   if MIDI_Res_Out <> nil then MIDI_Res_Out.notes_off;
end; // play_pause //

procedure TMIDI_Player.play_step;
begin
   if can_play then
   begin
      Timer.Enabled := true;
      stepping := true;
   end; // if
end; // play_step //

procedure TMIDI_Player.play_rewind;
begin
   FMIDI_Ticks := 0;
   MIDI_Container.Current_Event := 0;
end; // play_rewind //

procedure TMIDI_Player.play_stop;
var was_playing: boolean;
begin
   log_writeln ('>>TMIDI_Player.play_stop');
   if can_play then
   begin
      log_writeln ('stopped');
      was_playing := Timer.Enabled;
      Timer.Enabled := False;
      MIDI_Container.Reset;
      playing := false;
      play_rewind;
      if (MIDI_Res_Out <> nil) and was_playing then
      begin
         MIDI_Res_Out.notes_off;
         MIDI_Res_Out.synchronize (false);
      end; // if
   end; // if
end; // play_stop //

procedure TMIDI_Player.play_generate;
begin
   if can_play then
   begin
      playing  := true;
      stepping := false;
      MIDI_Container.Current_Event := 0;
      inside_callback := False;
      Ticks_per_32nd  := MIDI_Container.Ticks_Per_Quarter div 8;

// fill the container
      while playing do
      begin
         timer_callback (nil);
      end; // while
   end; // if
end; // play_generate //

function TMIDI_Player.is_playing: boolean;
begin
   Result := playing;
end; // is_playing //

function TMIDI_Player.is_pausing: boolean;
begin
   Result := not Timer.Enabled;
end; // is_pausing //

procedure TMIDI_Player.patch_change (track: Int32; id: TMIDI_Range; patch: Int8);
begin
   MIDI_Res_Out.send_MIDI_message (track, mc_MIDI_Program_Change + (id - 1), patch, 0);
end; // patch_change //

procedure TMIDI_Player.set_bpm (value: Int32);
var temp: boolean;
begin
   if (value > 1) and (MIDI_Container <> nil) then
   begin
      temp := Timer.Enabled;
      MIDI_Container.bpm := value;
      try
         Ticks_per_32nd := MIDI_Container.Ticks_Per_Quarter div 8;
         Timer.Enabled  := False;
         Timer.Interval := Milli_Seconds_per_Tick;
         log_write_fmt ('[set_bpm] MIDI_Ticks = %d, ticks/32 = %d, interval = %d', [MIDI_Ticks, Ticks_per_32nd, Timer.Interval]);
         log_writeln;
      finally
         Timer.Enabled  := temp
      end; // try
   end; // if
end; // set_bpm //

procedure TMIDI_Player.set_loop (value: boolean);
begin
   FLoop := value;
end; // set_loop //

procedure TMIDI_Player.set_ticks_per_32nd (value: Int32);
begin
   FTicks_per_32nd := value;
end; // set_ticks_per_32nd //

procedure TMIDI_Player.set_play_track (port: Int32; channel: TMIDI_Range; val: boolean);
begin
   if (MIDI_Res_Out <> nil) then
   begin
      if val then MIDI_Res_Out.Capability [port, channel] := pcPlay else
      begin
         MIDI_Res_Out.Capability [port, channel] := pcMute;
         MIDI_Res_Out.send_MIDI_message (port, 175 + channel - 1, mc_MIDI_All_Notes_Off, 0);
      end; // if
   end; // if
end; // set_play_track //

function TMIDI_Player.get_play_track (port: Int32; channel: TMIDI_Range): boolean;
begin
   Result := MIDI_Res_Out.Capability [port, channel] = pcPlay;
end; // get_play_track //

function TMIDI_Player.get_ms_per_tick: Int32;
begin
   Result := MIDI_Container.Milli_Seconds_Per_Tick;
end; // get_ms_per_tick //

procedure TMIDI_Player.set_ms_per_tick (ms: Int32);
begin
   MIDI_Container.Milli_Seconds_Per_Tick := ms;
end; // set_ms_per_tick //

function TMIDI_Player.get_time_units: Int32;
begin
   Result := FMIDI_Ticks div Ticks_per_32nd;
end; // get_time_units //

procedure TMIDI_Player.set_MIDI_ticks (new_ticks: Int32);
begin
   FMIDI_Ticks := new_ticks;
end; // set_MIDI_ticks //

procedure TMIDI_Player.set_MIDI_container (value: TMIDI_Container);
begin
   FMIDI_Container := value;
end; // set_MIDI_container //

procedure TMIDI_Player.set_MIDI_sound_cards (value: TMIDI_Resources_Out);
begin
   FMIDI_Res_Out := value;
end; // set_MIDI_sound_cards //

end. // Unit: MSC_Player //
