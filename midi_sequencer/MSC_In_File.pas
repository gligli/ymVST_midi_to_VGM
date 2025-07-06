unit MSC_In_File;

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

uses Classes, SysUtils, StdCtrls, ComCtrls, AnsiStrings,
     utilities_math, MSC_Definitions, MSC_Container;

type
   EMIDI_In_File_Error = class (EMSC_Error);

   TMIDI_In_File = class (TObject)
   private
      FRaw_Copy: TStringList;       // Viewer of MIDI events
      MIDI_In: TMIDI_File;          // File to write MIDI events into
      Container: TMIDI_Container;   // Storage for MIDI events
      bytes_to_be_read: Int32;      // Number of bytes written by put_byte
      MIDI_Format: Int32;           // Format of MIDI file: 0..2
      n_tracks: Int32;              // Number of tracks in the file
      nomerge: boolean;             // Indicates continued sysex's
      line: AnsiString;

   private
      procedure read_header;
      procedure read_track_header;
      procedure read_track (track: Int32);
      function  create_voice_message (status, c1, c2: Int32): TMIDI_Event;
      function  create_meta_event (mtype: Int32; mess: AnsiString): TMIDI_Event;
      function  get_string (len: Int32): AnsiString;
      function  get_var_integer: uInt32;
      function  get_cardinal: uInt32;
      function  get_integer (size: Int32): Int32;
      function  get_word: uInt16;
      function  get_byte: uInt8;
      procedure newline;
      procedure error (text: string);

   public
      constructor open (filename: string);
      destructor close;
      procedure load (Acontainer: TMIDI_Container; viewer: TStringList);

      property Raw_Copy: TStringList read FRaw_Copy write FRaw_Copy;
   end; // Class: TMIDI_In_File //

implementation

constructor TMIDI_In_File.open (filename: string);
begin
   inherited Create;

   Raw_Copy := nil;
   AssignFile (MIDI_In, filename);
   ReSet (MIDI_In);
   bytes_to_be_read := 0;
end; // open //

destructor TMIDI_In_File.close;
begin
   CloseFile (MIDI_In);

   inherited destroy;
end; // close //

procedure TMIDI_In_File.load (Acontainer: TMIDI_Container; viewer: TStringList);
var track: Int32;
begin
   Self.Container := Acontainer;
   Raw_Copy       := viewer;

// Read header and tempo track
   read_header;

// Read each separate track
   for track := 0 to n_tracks - 1 do
   begin
      read_track (track);
   end; // if
end; // load //

procedure TMIDI_In_File.read_header;
var time_format: Int32;
begin
// Read header chunk. First fout bytes should be 'MThd' and the length of the header
// chunk must be 6
   if (get_string (4) <> 'MThd') or (get_cardinal <> 6)
      then error ('This is not a legal MIDI file. MThd header lacks');
   line := line + ' ; header chunk';
   newline;
   MIDI_Format := get_integer (2); // MIDI format
   n_tracks    := get_integer (2); // n of tracks, one extra for tempo track
   time_format := get_integer (2); // number of delta times / quarter note
   if time_format < 0
      then line := line + Format (' ; Format = %d, tracks = %d, SMPTE = %d',
                                 [MIDI_Format, n_tracks, time_format])
      else line := line + Format (' ; Format = %d, tracks = %d, division = %d',
                                 [MIDI_Format, n_tracks, time_format]);
   Container.Division := time_format;
   newline;
end; // read_header //

procedure TMIDI_In_File.read_track_header;
begin
   if (get_string (4) <> 'MTrk')
      then error ('This is not a legal MIDI file. Track header lacks');
   bytes_to_be_read := get_cardinal;
   line := line + Format (' ; Track header, size = %d', [bytes_to_be_read]);
   newline;
end; // read_track_header //

procedure TMIDI_In_File.read_track (track: Int32);
const chan_type: array [0..15] of uInt8 = (0, 0, 0, 0, 0, 0, 0, 0,
                                           2, 2, 2, 2, 1, 1, 2, 0);
var ev: TMIDI_Event;
    time: uInt32;
    lookfor: uInt32;
    c, c1, c2, etype: Int32;
    running: boolean;
    status: Int32;
    needed: Int32;
    delta: uInt32;
    i: Int32;
    sysexcontinue: boolean;
    s: AnsiString;
// Read events from track for track # id
begin
   time := 0;
   status := 0;
   sysexcontinue := False;
   read_track_header;

   while bytes_to_be_read > 0 do
   begin
      delta := get_var_integer;
      line := Format ('%5d:', [delta]);
      time := time + delta;
      c := get_byte;
      if (sysexcontinue and (c <> $F7)) // Check for continuation of sysex
         then error ('Could noy find expected continuation of SysEx');
      if ((c and $80) = 0) then // Check for running status
      begin
         if status = 0
            then error ('Unexpected running status');
         running := True;
      end else
      begin
         status  := c;
         running := False;
      end; // if
      needed := chan_type [(status shr 4) and $F];

      if needed > 0 then // Handle Status messages
      begin
         if running then c1 := c
                    else c1 := get_byte;
         if needed = 2 then c2 := get_byte
                       else c2 := 0;
         ev := create_voice_message (status, c1, c2);

      end else  // Handle meta events and sysex messages
      begin
         case c of
         $FF:  begin // Meta event
                  etype   := get_byte;
                  lookfor := get_var_integer;
                  s       := get_string (lookfor);
                  ev      := create_meta_event (etype, s);
                  line    := line + ' ; ' + TMeta_Event (ev).Translated;
               end; // $FF
         $F0:  begin // Start of system exclusive, ignore
                  lookfor := get_var_integer;
                  // msginit ()
                  // msgadd ($F0);
                  s := get_string (lookfor);
                  ev := create_meta_event ($F0, s);
                  if (c = $F7) or (not nomerge)
                     then // sysex()
                     else sysexcontinue := true;
                  line := line + Format (' ; Sysex (F0): %d - %s', [lookfor, s]);
               end; // $F0
         $F7:  begin // Sysex continue or arbitrary stuff, ignore
                  lookfor := get_var_integer;
//                  if not sysexcontinue then ; // msginit
                  s := get_string (lookfor);
                  ev := create_meta_event ($F7, s);
                  line := line + Format (' ; End of Sysex (F7): %d - %s', [lookfor, s]);
               end; // $F7
         else  begin // bad uInt8, trying to ignore
                  line := line + '; Unrecognized uInt8';
                  ev := nil;
               end; // else
         end; // case
      end; // if

// If an event has been created, store it in the MIDI container
      if ev <> nil then
      begin
// correct the time by dividing it by the <division> or third parameter
// of the MIDI header block
         ev.Time := time div (uInt32 (Container.Division) div 8);
         ev.Port := 0;
         if container <> nil then i := container.store_event (ev)
                             else i := -1;
         if (time = 0) and (i > 1000) then line := line + Format ('?Anomaly? i = %d', [i]);
         if (ev.Event_Type = mc_MIDI_Meta_Event) and
            (ev.Data_Byte_1 = mc_Meta_End_Of_Track) and
            (bytes_to_be_read > 0) then
         begin
// An EOT was encountered while in the track header more bytes are left.
// The remainder is ignored and the EOT is used as the real end-of-track.
            line := line + '; Unexpected End Of Track';
            bytes_to_be_read := 0;
         end; // if
      end; // if
      newline;
   end; // while
end; // read_track //

function TMIDI_In_File.create_voice_message (status, c1, c2: Int32): TMIDI_Event;
var c: TMIDI_Range;
    e: uInt8;
begin
   c := (status and $F) + 1;
   e := status and $F0;

// First parameter is track, initially equal to channel;
   create_voice_message := TMIDI_Event.Create (c, c, e, c1, c2);
end; // create_voice_message //

function TMIDI_In_File.create_meta_event (mtype: Int32; mess: AnsiString): TMIDI_Event;
begin
   create_meta_event := TMeta_Event.Create (mtype, mess);
end; // create_meta_event //

function TMIDI_In_File.get_string (len: Int32): AnsiString;
var i: Int32;
    s: AnsiString;
begin
   s := '';
   for i := 1 to len do s := s + AnsiChar (get_byte);
   get_string := s;
end; // get_string //

function TMIDI_In_File.get_var_integer: uInt32;
var len, ret, value: uInt32;
begin
   ret := 0;
   len := 0;
   repeat
      len := len + 1;
      value := get_byte;
      ret   := (ret shl 7) + (value and $7F);
   until (value and $80) <= 0;

   line := line + Format ('[%d = %d] ', [len, ret]);
   get_var_integer := ret;
end; // get_var_integer //

function TMIDI_In_File.get_cardinal: uInt32;
var a, b, c, d: uInt8;
begin
   a := get_byte;
   b := get_byte;
   c := get_byte;
   d := get_byte;
   get_cardinal := convert_to_cardinal (a, b, c, d);
end; // get_cardinal //

function  TMIDI_In_File.get_integer (size: Int32): Int32;
var l: Int32;
begin
   if size = 0 then get_integer := 0 else
   begin
      l := (get_integer (size - 1) shl 8);
      l := l or (Int32 (get_byte) and $FF);
      get_integer := l;
   end;
end; // get_integer //

function TMIDI_In_File.get_word: uInt16;
var a, b: uInt8;
begin
   a := get_byte;
   b := get_byte;
   get_word := convert_to_word (a, b);
end; // get_word //

function TMIDI_In_File.get_byte: uInt8;
var b: uInt8;
begin
   if EOF (MIDI_In) then error ('Unexpected EOF in MIDI file.');
   Read (MIDI_In, b);
   get_byte := b;
   line := line + Format (' %2.2x', [b]);
   bytes_to_be_read := bytes_to_be_read - 1;
end; // get_byte //

procedure TMIDI_In_File.newline;
var i: Int32;
    adder: AnsiString;
begin
   if Raw_Copy <> nil then
   begin
      for i := 1 to Length (line) do
         if (Ord (line [i]) < 32) or (Ord (line [i]) > 127)
            then adder := adder + '\' + Format ('%2.2x', [Ord (line [i])])
            else adder := adder + line [i];
      Raw_Copy.Add (string (adder));
   end; // if
   line := '';
end; // newline //

procedure TMIDI_In_File.error (text: string);
begin
   line := line + AnsiString (text);
   newline;
   raise EMIDI_In_File_Error.Create (text);
end; // error //

end. // Unit: MIDI_In_File //
