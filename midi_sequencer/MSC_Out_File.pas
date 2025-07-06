unit MSC_Out_File;

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

uses Classes, SysUtils,
     MSC_Definitions, MSC_Container;

type
   EMIDI_Out_File_Error = class (EMSC_Error);

   TMIDI_Out_File = class (TObject)
   private
      MIDI_Out: TMIDI_File;     // File to write MIDI events into
      bytes_written: uInt32;  // Number of bytes written by put_byte
      file_pos: uInt32;       // Position in MIDI file
      delta: uInt32;          // Delta time
      n_tracks: Int32;   // Number of tracks

      procedure write_header (container: TMIDI_Container);
      procedure write_track_header;
      procedure write_track (container: TMIDI_Container; tr: Int32);
      procedure write_eot;
      procedure write_delta_time (value: uInt32);
      procedure write_meta_event (meta_event, length: uInt8);
      procedure write_time_signature (a, b, c, d: uInt8);
      procedure write_tempo (tempo: uInt32);
      procedure put_string (s: string);
      procedure put_cardinal (c: uInt32);
      procedure put_word (w: uInt16);
      procedure put_byte (b: uInt8);

   public
      constructor open (filename: string);
      destructor close;
      procedure save (container: TMIDI_Container);
   end; // Class: TMIDI_Out_File //

implementation

constructor TMIDI_Out_File.open (filename: string);
begin
   inherited Create;
   AssignFile (MIDI_Out, filename);
   rewrite (MIDI_Out);
   bytes_written := 0;
end; // open //

procedure TMIDI_Out_File.save (container: TMIDI_Container);
var eChannel: Int32;
begin
// Count number of tracks with events
   n_tracks := 0;
   for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
      if container.Voice_Count [eChannel] > 0 then n_tracks := n_tracks + 1;

// Write header and tempo track
   write_header (container);

// Write each separate track
   for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
      if container.Voice_Count [eChannel] > 0
         then write_track (container, eChannel);
end; // save //

procedure TMIDI_Out_File.write_header (container: TMIDI_Container);
begin
   delta := 0;

// Write header chunk
   put_string ('MThd');
   put_cardinal (6);        // Length of header chunk
   put_word (1);            // MIDI format 1
   put_word (n_tracks + 1); // n of tracks, one extra for tempo track
   put_word (8);            // Delta's per quarter note

// Write tempo track
   write_track_header;
   write_time_signature (container.Beats, container.Rythm div 2,
                         container.Ticks_per_Quarter, 8);
   write_tempo (container.Micro_SPQ);
   write_eot;
end; // write_header //

procedure TMIDI_Out_File.write_track_header;
begin
   put_string ('MTrk');
   file_pos := filepos (MIDI_Out);
   put_cardinal (0);
   bytes_written := 0;
end; // write_track_header //

procedure TMIDI_Out_File.write_track (container: TMIDI_Container; tr: Int32);
var ev: TMIDI_Event;
    i: longint;
    time: uInt32;
// Write events to track for track # id
begin
   write_track_header;
   ev := TMeta_Event.Create (3, AnsiString (container.Track_Name [tr]));
   ev.Port := tr;
   write_delta_time (0);
   ev.write_event (MIDI_Out);
   bytes_written := bytes_written + ev.Size;
   ev.Free;
   time := 0;

   for i := 0 to container.Count - 1 do
   begin
      ev := container.Event [i];
      if ev.Port = tr then
      begin
         delta := ev.Time - time;
         time := ev.Time;
         write_delta_time (delta);
         ev.write_event (MIDI_Out);
         bytes_written := bytes_written + ev.Size;
      end; // if
   end; // for
   delta := 0;
   write_eot;
end; // write_track //

procedure TMIDI_Out_File.write_eot;
var pos: uInt32;
begin
   write_meta_event (mc_Meta_End_Of_Track, 0);
   pos := filepos (MIDI_Out);
   seek (MIDI_Out, file_pos);
   put_cardinal (bytes_written);
   seek (MIDI_Out, pos);
end; // write_eot //

procedure TMIDI_Out_File.write_delta_time (value: uInt32);
var b: uInt8;
    buffer: uInt32;
begin
   buffer := value and $7F;
   value  := value shr 7;
   while (value > 0) do
   begin
      buffer := buffer shl 8;
      buffer := buffer or $80;
      buffer := buffer + (value and $7F);
      value := value shr 7;
   end; // while
   b := buffer and $FF;
   while (b >= $80) do
   begin
      put_byte (b);
      buffer := buffer shr 8;
      b := buffer and $FF;
   end; // while
   put_byte (b);
end; // write_delta_time //

procedure TMIDI_Out_File.write_meta_event (meta_event, length: uInt8);
begin
   write_delta_time (delta);
   put_byte ($FF);
   put_byte (meta_event);
   put_byte (length);
end; // write_meta_event //

procedure TMIDI_Out_File.write_time_signature
(
   a,      // Numerator of time signature
   b,      // Denominator of time signature
   c,      // MIDI clocks in metronome click
   d: uInt8 // # of notated 32'nds in quarter note (8)
);
begin
   write_meta_event (mc_Meta_Time_Signature, 4);
   put_byte (a);
   put_byte (b);
   put_byte (c);
   put_byte (d);
end; // write_time_signature //

procedure TMIDI_Out_File.write_tempo (tempo: uInt32);
begin
   write_meta_event (mc_Meta_Set_Tempo, 3);
   put_byte ($FF and (tempo shr 16));
   put_byte ($FF and (tempo shr 8));
   put_byte ($FF and tempo);
end; // write_tempo //

procedure TMIDI_Out_File.put_string (s: string);
var i: uInt32;
begin
   for i := 1 to Length (s) do put_byte (ord (s [i]));
end; // put_string //

procedure TMIDI_Out_File.put_cardinal (c: uInt32);
begin
   put_byte ($FF and (c shr 24));
   put_byte ($FF and (c shr 16));
   put_byte ($FF and (c shr 8));
   put_byte ($FF and c);
end; // put_cardinal //

procedure TMIDI_Out_File.put_word (w: uInt16);
begin
   put_byte ($FF and (w shr 8));
   put_byte ($FF and w);
end; // put_word //

procedure TMIDI_Out_File.put_byte (b: uInt8);
begin
   write (MIDI_Out, b);
   Inc (bytes_written);
end; // put_byte //

destructor TMIDI_Out_File.close;
begin
   CloseFile (MIDI_Out);
   inherited destroy;
end; // close //

end. // Unit: TMIDI_Out_File //
