unit MSC_Container;

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
   TMIDI_Container is the central class in a MIDI_Sequencer.
   It contains all events and handles timing. Timing is is important for correct
   MIDI playback and is treated below.

   Timing
   ------
   There are two important parameters ruling MIDI playback: microseconds
   per quarter note (Micro_SPQ) and ticks per quarter note. The first one
   can be translated to BPM (= 60,000,000 / microseconds per quarter note).
   Typically, if microseconds per quarternote is 500,000 then BPM is 120
   (meaning half a second per quarter note).

   The second parameter is the number of MIDI ticks (clocks) per quarternote.
   A typical value is 24. Each tick will take microseconds per quarter
   divided bij ticks per quarter. In the current example 500000 / 24
   = 20833 microseconds = 21 (rounded) milli seconds per MIDI tick.
   In this example a quarter note needs 24 midi ticks, each lasting 21
   milliseconds which is 504 milliseconds (about half a second) per
   quarter note.

   Two variables are stored: Micro_SPQ and Ticks_Per_Quarter.
   All other variables (BPM or Milli_Seconds_Per_Tick) are either derived
   from these two variables or set these two variables.

   Class TMIDI_Container receives MIDI events and sorts these in order of time.
   Special insert and append routines should do these quickly.

   TMIDI_Container is a 'publisher' of events. As soon as an event is inserted
   it is published to subscribers (for example the demo view).

   Below some conversions (http://home.roadrunner.com/~jgglatt/tech/midifile/ppqn.htm)
   useful for SMTP conversions (not yet implemented)
   BPM = 60,000,000/MicroTempo
      MicrosPerPPQN = MicroTempo/TimeBase
      MicrosPerMIDIClock = MicroTempo/24
      PPQNPerMIDIClock = TimeBase/24
      MicrosPerSubFrame = 1000000 * Frames * SubFrames
      SubFramesPerQuarterNote = MicroTempo/(Frames * SubFrames)
      SubFramesPerPPQN = SubFramesPerQuarterNote/TimeBase
      MicrosPerPPQN = SubFramesPerPPQN * Frames * SubFrames
}

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs,
  utilities_math, utilities_logging, Publish_Subscribe, MSC_Definitions;

const pm_Begin_Update = Maxint - 1;
      pm_End_Update   = Maxint - 2;
      pm_Insert_Event = Maxint - 3;
      pm_Update_Time  = Maxint - 4;
      pm_Clear_Events = Maxint - 5;

type
   TReal_Array = array of double;
   TInsertion_Mode = (imInsert, imAppend);

   TMIDI_Container = class (TPublisher, IPublish)
   private
      FBeats: uInt8;             // Number of beats per measure
      FDenominator: uInt8;       // Rythmic value of the measure
      FKey: TMIDI_Key_Signature; // Key of the MIDI piece
      FMajor: boolean;           // True = major key, false is minor
      FSharps: Int32;            // number of sharps (< 0 = flats)
      FChanged: boolean;         // Indicates whether the event list has changed
      FSlice_Begin: Int32;       // beginning of a slice
      FSlice_End: Int32;         // end of a slice
      FLast_Index: Int32;        // Index of last inserted event
      FCommand: Int32;           // Instructions for the subscriber
      FTrace: boolean;           // When true, events are traced in subscribers
      FCurrent_Event: Int32;     // Index to event to be played
      FCurrent_Time: Int32;      // Current time
      FMax_Time: Int32;          // Total time of composition in ticks
      FMicro_SPQ: Int32;         // micro seconds per quarter note
      FTicks_per_Quarter: Int32; // MIDI clocks per quarter note
      FDivision: Int32;
      FTrack_Count: array [TMIDI_Range] of Int32;    // Number of events per track
      FVoice_Count: array [TMIDI_Range] of Int32;    // Number of note on events per track
      FTrack_Name:  array [TMIDI_Range] of string;   // Name of each track

      last_time: uInt32;         // Stores time of last inserted event
      Event_List: TStringList;   // List containing the MIDI events
      qn32: Int32;               // # of 32'nds in a quarter note

      procedure set_beats (value: uInt8);
      procedure set_denominator (value: uInt8);
      procedure set_track_count (channel: TMIDI_Range; value: Int32);
      function  get_track_count (channel: TMIDI_Range): Int32;
      procedure set_voice_count (channel: TMIDI_Range; value: Int32);
      function  get_voice_count (channel: TMIDI_Range): Int32;
      procedure set_track_name  (channel: TMIDI_Range; value: string);
      function  get_track_name  (channel: TMIDI_Range): string;
      function  get_ticks_per_quarter: Int32;
      procedure set_ticks_per_quarter (value: Int32);
      function  get_milli_seconds_per_tick: Int32;
      procedure set_milli_seconds_per_tick (value: Int32);
      function  get_micro_spq: Int32;
      procedure set_micro_spq (value: Int32);
      function  get_bpm: Int32;
      procedure set_bpm (value: Int32);
      function  get_current_event: Int32;
      procedure set_current_event (value: Int32);
      function  get_max_time: Int32;
      procedure set_max_time (value: Int32);
      function  get_count: Int32;
      function  get_event (index: Int32): TMIDI_Event;
      procedure set_last_index (index: Int32);
      procedure set_trace (trace: boolean);

   public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Reset;

      function  status: string;
      function  time_to_seconds (time: Int32): double;
      function  n_tracks: Int32;
      procedure assign_events (events: TStringList);
      procedure get_vectors (channel: Int32; var n, d, s, t: TReal_Array);
      procedure handle_event (event: TMIDI_Event);
      function  store_event (event: TMIDI_Event): Int32;
      function  add_event (event: TMIDI_Event): Int32;
      function  remove_event (index: Int32): TMIDI_Event;
      procedure publish_command (published_command: Int32);
      function  find_duration (ev: TMIDI_Event; pos: Int32): Int32;
      function  set_max_time_to_container: Int32;
      procedure LoadFromFile (file_name: string; raw_copy: TStringList);
      procedure SaveToFile   (file_name: string);

      property Beats: uInt8 read FBeats write set_beats;
      property Rythm: uInt8 read FDenominator write set_denominator;
      property Key: TMIDI_Key_Signature read FKey write FKey;
      property Major: boolean read FMajor write FMajor;
      property Sharps: Int32 read FSharps;
      property Max_Time: Int32 read get_max_time write set_max_time;
      property Changed: boolean read FChanged write FChanged;
      property Track_Count [channel: TMIDI_Range]: Int32  read get_track_count write set_track_count;
      property Voice_Count [channel: TMIDI_Range]: Int32  read get_voice_count write set_voice_count;
      property Track_Name  [channel: TMIDI_Range]: string read get_track_name  write set_track_name;
      property Event [index: Int32]: TMIDI_Event read get_event; default;
      property Count: Int32 read get_count;
      property Last_Index: Int32 read FLast_Index write set_last_index;
      property Command: Int32 read FCommand;
      property Trace: boolean read FTrace write set_trace;
      property Slice_Begin: Int32 read FSlice_Begin;
      property Slice_End:   Int32 read FSlice_End;
      property Current_Time: Int32 read FCurrent_Time write FCurrent_Time;
      property Current_Event: Int32 read get_current_event write set_current_event;

// Properties concerning MIDI tempo
      property Milli_Seconds_Per_Tick: Int32 read get_milli_seconds_per_tick write set_milli_seconds_per_tick;
      property Micro_SPQ: Int32 read get_micro_spq write set_micro_spq;
      property Ticks_Per_Quarter: Int32 read get_ticks_per_quarter write set_ticks_per_quarter;
      property Division: Int32 read FDivision write FDivision;
      property BPM: Int32 read get_bpm write set_bpm;
   end; // Class: TMIDI_Container //

implementation

uses MSC_In_File, MSC_Out_File;

{*******************************************************************
*                                                                  *
* Class TMIDI_Container                                            *
*                                                                  *
********************************************************************}

constructor TMIDI_Container.Create;
begin
   inherited Create;

   Event_List := TStringList.Create;
   event_List.Capacity  := 100;
   Event_List.Delimiter :='%';
   Clear;
end; // init //

destructor TMIDI_Container.Destroy;
begin
   Clear;
   Event_List.Free;

   inherited Destroy;
end; // Destroy //

procedure TMIDI_Container.reset;
begin
   Last_Index := 0;
   last_time  := 0;
   Current_Time := 0;
end; // reset //

procedure TMIDI_Container.Clear;

// Clears all MIDI events from the event list and resets all parameters

var eChannel: Int32;
begin
   for eChannel := 0 to Event_List.Count - 1
      do Event_List.Objects [eChannel].Free;
   Event_List.Clear;

   for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
   begin
      Voice_Count [eChannel] := 0;
      Track_Count [eChannel] := 0;
      Track_Name  [eChannel] := Format ('%d', [eChannel]);
   end; // for
   reset;
   Micro_SPQ         := 500000;
   Beats             := 4;
   Rythm             := 4;
   Ticks_per_Quarter := 32;
   Key               := 0;
   Major             := True;
   Changed           := False;
   Publish_command (pm_Clear_Events);
end; // clear //

function TMIDI_Container.status: string;
begin
   Result := Format ('µsec/quart = %d, Milsec/tick = %d, ' +
                     'ticks/quart = %d, BPM = %d ',
                     [Micro_SPQ, Milli_Seconds_Per_Tick, Ticks_Per_Quarter, BPM]);
end; // status //

function TMIDI_Container.time_to_seconds (time: Int32): double;
begin
   Result := Milli_Seconds_Per_Tick * (ticks_per_quarter / 8.0) * time / 1000.0;
end; // time_to_seconds //

procedure TMIDI_Container.set_beats (value: uInt8);
begin
   FBeats := value;
end; // set_beats //

procedure TMIDI_Container.set_denominator (value: uInt8);
begin
   FDenominator := value;
end; // set_denominator //

procedure TMIDI_Container.set_track_count (channel: TMIDI_Range; value: Int32);
begin
   FTrack_Count [channel] := value;
end; // set_track_count //

function TMIDI_Container.get_track_count (channel: TMIDI_Range): Int32;
begin
   Result := FTrack_Count [channel];
end; // get_track_count //

procedure TMIDI_Container.set_voice_count (channel: TMIDI_Range; value: Int32);
begin
   FVoice_Count [channel] := value;
end; // set_voice_count //

function  TMIDI_Container.get_voice_count (channel: TMIDI_Range): Int32;
begin
   Result := FVoice_Count [channel];
end; // get_voice_count //

procedure TMIDI_Container.set_track_name (channel: TMIDI_Range; value: string);
begin
   FTrack_Name [channel] := value;
end; // set_track_name //

function  TMIDI_Container.get_track_name (channel: TMIDI_Range): string;
begin
   Result := FTrack_Name [channel];
end; // get_track_name //

function  TMIDI_Container.get_ticks_per_quarter: Int32;
begin
   Result := FTicks_Per_Quarter;
end; // get_time_format //

procedure TMIDI_Container.set_ticks_per_quarter (value: Int32);
begin
   FTicks_Per_Quarter := value;
end; // set_time_format //

function  TMIDI_Container.get_milli_seconds_per_tick: Int32;
begin
   Result := Round (Micro_SPQ / 1000.0 / Ticks_Per_Quarter);
end; // get_milli_seconds_per_tick //

// Routine computes the number of milliseconds per tick.
// Tested for MIDI durations, but not for SMPTE
// value < 0 SMPTE, > 0 ms per MIDI tick, equals time format in MIDI header
procedure TMIDI_Container.set_milli_seconds_per_tick (value: Int32);
var frame_code, ticks_per_frame: Int32;
    frames_per_second: double;
begin
   if value < 0 then // SMPTE
   begin
      frame_code := value shr 8;
      case frame_code of
         24: frames_per_second := 24.00;
         25: frames_per_second := 25.00;
         29: frames_per_second := 29.97;
        else frames_per_second := 30.00;
      end; // case

      ticks_per_frame := value and $FF;
      Milli_Seconds_Per_Tick := Round ({sampling_rate} 1000.0 * frames_per_second * ticks_per_frame);
   end else // MIDI durations
   begin
      Micro_SPQ := value * Ticks_Per_Quarter * 1000;
   end; // if
end; // set_milli_seconds_per_tick //

function  TMIDI_Container.get_micro_spq: Int32;
begin
   Result := FMicro_SPQ;
end; // get_micro_spq //

procedure TMIDI_Container.set_micro_spq (value: Int32);
begin
   FMicro_SPQ := value;
end; // set_micro_spq //

function  TMIDI_Container.get_bpm: Int32;
begin
   Result := Round (mspq / Micro_SPQ);
end; // get_bpm //

procedure TMIDI_Container.set_bpm (value: Int32);
begin
   Micro_SPQ := Round (mspq / value);
   log_write_fmt ('[MIDI_Container.set_bpm] micro_spq = %d, mspq = %d, value = %d',
      [micro_spq, mspq, value]);
   log_writeln;
end; // set_bpm //

function TMIDI_Container.get_max_time: Int32;
begin
   Result := FMax_Time;
end; // get_max_time //

procedure TMIDI_Container.set_max_time (value: Int32);
begin
   FMax_Time := value;
end; // set_max_time //

procedure TMIDI_Container.set_last_index (index: Int32);
begin
   FLast_Index := index;
end; // set_last_index //

function TMIDI_Container.get_current_event: Int32;
begin
   Result := FCurrent_Event;
end; // get_current_event //

procedure TMIDI_Container.set_current_event (value: Int32);
begin
   FCurrent_Event := value;
   publish_command (pm_Update_Time);
end; // set_current_event //

function TMIDI_Container.get_count: Int32;
begin
   Result := Event_List.Count;
end; // get_count //

function TMIDI_Container.get_event (index: Int32): TMIDI_Event;
begin
   Result := nil;
   if (index >= 0) and (index < Count) then
   begin
      Result := TMIDI_Event (Event_List.Objects [index]);
   end; // if
end; // get_event //

procedure TMIDI_Container.set_trace (trace: boolean);
begin
   FTrace := trace;
end; // set_trace //

function TMIDI_Container.n_tracks: Int32;
var eChannel: TMIDI_Range;
    n: Int32;
begin
   n := 0;
   for eChannel := Low (TMIDI_Range) to High (TMIDI_Range)
      do if Track_Count [eChannel] > 0 then n:= n + 1;
   Result := n;
end; // n_tracks //

procedure TMIDI_Container.publish_command (published_command: Int32);
begin
   FCommand := published_command;
   Publish;
end; // publish_command //

// assign_events merges a TStringlist containing MIDI events.
// The events are inserted and sorted using store_event
procedure TMIDI_Container.assign_events
(
   events: TStringList // Events to merge with this container
);
var i: Int32;
begin
   Clear;
   for i := 0 to events.Count - 1 do
   begin
      store_event (TMIDI_Event (events.Objects [i]));
   end; // for
end; // assign_events //

// Fetches converts the note-on and -off messages to note values (n)
// with their duration (d), speed (s) at a time t.
procedure TMIDI_Container.get_vectors (channel: Int32; var n, d, s, t: TReal_Array);
var event_: TMIDI_Event;
    i, c, size: Int32;
    rescaler: Int32;
    Timer_Ticks_per_Q: Int32;
begin
   size := Track_Count [channel];
   Timer_Ticks_per_Q := Ticks_Per_Quarter;
   rescaler := 1;
//   if Timer_Ticks_per_Q > 8
//      then rescaler := Timer_Ticks_per_Q div 8;

   if size > 1 then
   begin
      SetLength (n, size);
      SetLength (d, size);
      SetLength (s, size);
      SetLength (t, size);
      c := 0;
      for i := 0 to Count - 1 do
      begin
         event_ := TMIDI_Event (Event_List.Objects [i]);
         if (event_.Channel = channel) and (event_.Event_Type = mc_MIDI_Note_On) and
            (event_.Data_Byte_2 > 0) then
         begin
            n [c] := event_.Data_Byte_1;
            d [c] := find_duration (event_, i + 1) div rescaler;
            s [c] := event_.Data_Byte_2;
            t [c] := Int32 (event_.Time) div rescaler;
            c := c + 1;
         end; // if
      end; // for
      SetLength (n, c);
      SetLength (d, c);
      SetLength (s, c);
      SetLength (t, c);
   end; // if
end; // get_vectors //

procedure TMIDI_Container.handle_event (event: TMIDI_Event);

// This routine handles special events that set tempo, bpm, and so on.
// This may result in properties of the MIDI container being changed.

   procedure handle_track_name (event: TMeta_Event);
   begin
      Track_Name [event.Channel] := string (event.Text);
   end; // handle_track_name //

   procedure handle_tempo (s: AnsiString);
   begin
      Micro_SPQ := 500000; // set default
      if Length (s) = 3
         then Micro_SPQ := convert_to_cardinal (0, Ord (s [1]), Ord (s [2]), Ord (s [3]));
   end; // handle_tempo //

   procedure handle_time_signature (s: AnsiString);
   begin
// Set defaults
      Beats             := 4;
      Rythm             := 4;
      Ticks_Per_Quarter := Ord (s [3]);
      qn32              := Ord (s [4]);

      if Length (s) = 4 then
      begin
         Beats             := Ord (s [1]);
         Rythm             := 1 shl Abs (Ord (s [2]));
         Ticks_Per_Quarter := Ord (s [3]);
         qn32              := Ord (s [4]);
      end; // if
   end; // handle_tempo //

   procedure handle_key_signature (s: AnsiString);
   var i: Int32;
   begin
// Set defaults
      Key   := 0; // 'C'
      Major := True;

      if Length (s) = 2 then
      begin
         i := Ord (s [1]);
         if (i >= Low (TMIDI_Key_Signature)) and (i <= High (TMIDI_Key_Signature))
            then Key := MIDI_Note_Translation [i]
            else Key := 0;
         Major := Ord (s [2]) = 0;
      end; // if
   end; // handle_tempo //

var meta_event: TMeta_Event;
begin
   if event.MIDI_Message = $FF then
   begin
      meta_event := TMeta_Event (event);
      case event.Data_Byte_1 of
         mc_Meta_Track_Name:      handle_track_name (meta_event);
         mc_Meta_Instrument_Name: if Track_Name [meta_event.Port] = '' then handle_track_name (meta_event);
         mc_Meta_Set_Tempo:       handle_tempo (meta_event.Text);
         mc_Meta_Time_Signature:  handle_time_signature (meta_event.Text);
         mc_Meta_Key_Signature:   handle_key_signature (meta_event.Text);
         else ; // ignore
      end; // case
   end; // if
end; // handle_event //

{
   Inserting an event depends on the fact that MIDI events usually follow
   each other in time. To reduce processing time, last_time keeps track of the
   time of the last event inserted. When a new event is inserted, searching
   will start from this time backward or forward, depending on the time of
   the event to be inserted.
   There are three categories of events: note-on, note-off and other.
   At a given time these three categories are ordered as follows:
      1. note off
      2. other
      3. note on
   This ensures that a note off does not accidentally switch off a note that
   has been generated before at the same time. Putting the "other"
   category before a note on (for example a program change) ensures that it is
   in effect as soon as the note on is effected.
}
function TMIDI_Container.store_event (event: TMIDI_Event): Int32;
var ev: TMIDI_Event;
    index: Int32;
    found_equal, stop: boolean;
begin
   index := 0;
   handle_event (event); // Handle special MIDI events
   Track_Count [event.Channel] := Track_Count [event.Channel] + 1;
   if is_note_on (event)
      then Voice_Count [event.Channel] := Voice_Count [event.Channel] + 1;
   if Count = 0 then
   begin
      Event_List.InsertObject (0, '', event);
      Last_Index := 0;
      last_time  := event.Time;
   end else
   begin
      found_equal := False;
      index := Last_Index;
      ev := TMIDI_Event (Event_List.Objects [index]);

// Position index at the first event that has a time < than event.Time
// All other operation assume this condition fullfilled
      while (index < Count) and (ev.Time < event.Time) do
      begin
         found_equal := ev.Time = event.Time;
         index := index + 1;
         if index < Count
            then ev := TMIDI_Event (Event_List.Objects [index]);
      end; // while

      while (index >= 0) and (ev.Time >= event.Time) do
      begin
         found_equal := ev.Time = event.Time;
         index := index - 1;
         if index >= 0
            then ev := TMIDI_Event (Event_List.Objects [index]);
      end; // while

// Index points at the first event with time < event.time
      if not found_equal then
      begin
         if index < Count
            then index := index + 1;
         Event_List.InsertObject (index, '', event);
         publish_command (pm_Insert_Event);
      end else if is_note_off (event) then
      begin
         if index < Count
            then index := index + 1;
         Event_List.InsertObject (index, '', event);
         publish_command (pm_Insert_Event);
      end else
      begin
         stop := False;
         repeat
            index := index + 1;
            if index >= Count then
            begin
               index := Count;
               Event_List.InsertObject (index, '', event);
               publish_command (pm_Insert_Event);
               stop := True;
            end else
            begin
               ev := TMIDI_Event (Event_List.Objects [index]);
               if ev.Time > event.Time then
               begin
                  Event_List.InsertObject (index, '', event);
                  publish_command (pm_Insert_Event);
                  stop := True;
               end else if is_note_on (ev) then
               begin
                  if is_note_on (event)
                     then index := index + 1;
                  Event_List.InsertObject (index, '', event);
                  publish_command (pm_Insert_Event);
                  stop := True;
               end; // if
            end; // if
         until stop or (index >= Count);
      end; // if
      Last_Index := index;
      last_time  := event.Time;
   end; // if
   Result := index;
end; // store_event //

function TMIDI_Container.add_event (event: TMIDI_Event): Int32;
begin
   Result := Event_List.AddObject ('', event);
end; // add_event //

function TMIDI_Container.remove_event (index: Int32): TMIDI_Event;
begin
   if (index >= 0) and (index < Event_List.Count) then
   begin
      Result := get_event (index);
      Event_List.Delete (index);
   end else
   begin
      Result := nil;
   end;
end; // remove_event //

// Finds the duration of an event by looking for an event with the same note
// and a speed of zero. The search starts from posistion pos.
function TMIDI_Container.find_duration (ev: TMIDI_Event; pos: Int32): Int32;
var event_: TMIDI_Event;
    i: Int32;
    found: boolean;
begin
   Result := 0;
   if is_note_on (ev) then
   begin
      i := pos;
      found := false;
      while (i < Count) and not found do
      begin
         event_ := get_event (i);
         if (event_ <> nil) and is_note_off (event_) and
            (event_.Data_Byte_1 = ev.Data_Byte_1) and
            (event_.Port = ev.Port) then
         begin
            found := true;
            Result := Int32 (event_.Time) - Int32 (ev.Time);
         end; // if
         i := i + 1;
      end; // while
   end; // if
end; // find_duration //

// Sets the maximum time of the container to the last event found.
// As the events in the MIDI_Container are ordered by time, this
// event contains the maximum time. The max_time is returned as function result.
function TMIDI_Container.set_max_time_to_container: Int32;
var ev: TMIDI_Event;
begin
   ev := Event [Count - 1];
   if ev <> nil then
   begin
      Max_Time := ev.Time;
      Result   := ev.Time;
   end else
   begin
      raise EMSC_Error.Create ('set_max_time_to_container: Non existing event: ' +
                               IntToStr (Count - 1));
   end; // if
end; // set_max_time_to_container //

procedure TMIDI_Container.LoadFromFile (file_name: string; raw_copy: TStringList);
var MIDI_In: TMIDI_In_File;
begin
   Clear;
   publish_command (pm_Begin_Update);
   MIDI_In := nil; // Avoid warnings
   try
      MIDI_In := TMIDI_In_File.Open (file_name);
      MIDI_In.load (Self, raw_copy);
   finally
      MIDI_In.Close;
      publish_command (pm_End_Update);
   end; // try
end; // LoadFromFile //

procedure TMIDI_Container.SaveToFile (file_name: string);
var MIDI_Out: TMIDI_Out_File;
begin
   MIDI_Out := nil; // Avoid warnings
   try
      MIDI_Out := TMIDI_Out_File.Open (file_name);
      MIDI_Out.Save (Self);
   finally
      MIDI_Out.Close;
   end; // try
end; // SaveToFile //

end. // Unit: TMIDI_Container //
