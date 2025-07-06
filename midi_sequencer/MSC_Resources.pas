unit MSC_Resources;

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

uses Windows, SysUtils, Classes, Controls, MMSystem,
     MSC_Definitions, MSC_Component, MSC_Device, MSC_Out_Device, MSC_In_Device;

type
   EMSC_Resource_Error = class (EMSC_Error);

   TTrack_Array        = array [TMIDI_Range] of TPort_Capability;
   TOnMidiInput        = procedure (Sender: TObject) of object;

///////////////////////////////////////////////////////////////////////////////////////////
   TMIDI_Resources = class (TMSC_Component)
   protected
      FCount: Int32;               // Number of MIDI ports
      FMIDI_Thru: Int32;           // MIDI through port, port receiving sync signals
      FCap: array of TTRack_Array; // Containing the port capabilities
      MIDI_Ports: TStringList;     // List of MIDI ports
      synchronization: boolean;    // True if sync with external sequencer is necessary
      closed: array of boolean;    // Array that keeps track which ports were open during backup/restore

      procedure set_port_capability (port: Int32; channel: TMIDI_Range; pc: TPort_Capability);
      function  get_port_capability (port: Int32; channel: TMIDI_Range): TPort_Capability;
      function  get_port_name (port: Int32): string; virtual; abstract;
      function  get_port_names (track: Int32): string; virtual; abstract;
      function  get_version (port: Int32): Int32; virtual; abstract;
      function  get_technology (port: Int32): OutPortTech; virtual; abstract;
      function  get_driver_version (port: Int32): Int32; virtual; abstract;
      function  get_voices (port: Int32): Int32; virtual; abstract;
      function  get_notes (port: Int32): Int32; virtual; abstract;
      function  get_support (port: Int32): uInt32; virtual; abstract;
      function  allocated_port (port: Int32): boolean; virtual; abstract;
      function  get_status (port: Int32): Int32; virtual; abstract;
      procedure set_midi_thru (port: Int32);
      function  get_port_device (port: Int32): TMIDI_Device;

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Open (port: Int32); virtual; abstract;
      procedure Close (port: Int32); virtual; abstract;
      procedure open_all;
      procedure close_all;
      procedure backup_and_close;
      procedure restore_and_open;
      procedure port_disable (port: Int32);
      procedure port_play (port: Int32);
      procedure port_mute (port: Int32);
      function  port_exclusive_play (port: Int32): boolean;
      function  find_likely_default: Int32; virtual; abstract;

      property Count: Int32 read FCount; // Count of number of ports
      property Capability [port: Int32; channel: TMIDI_Range]: TPort_Capability
         read get_port_capability write set_port_capability; default;
      property ProductName [port: Int32]: string read get_port_name;
      property ProductNames [track: Int32]: string read get_port_names;
      property DriverVersion [port: Int32]: Int32 read get_version;
      property Technology [port: Int32]: OutPortTech read get_technology;
      property Voices [port: Int32]: Int32 read get_voices;
      property Notes [port: Int32]: Int32 read get_notes;
      property Support [port: Int32]: uInt32 read get_support;
      property Status [port: Int32]: Int32 read get_status;
      property MIDI_Thru: Int32 read FMIDI_Thru write set_midi_thru;
      property Port_Device [p: Int32]: TMIDI_Device read get_port_device;
   end; // Class: MIDI_Resources //

///////////////////////////////////////////////////////////////////////////////////////////
   TMIDI_Resources_Out = class (TMIDI_Resources)
   protected
      function  get_out_port (port: Int32): TMIDI_Device_Output;
      function  get_port_name (port: Int32): string; override;
      function  get_port_names (track: Int32): string; override;
      function  get_version (port: Int32): Int32; override;
      function  get_technology (port: Int32): OutPortTech; override;
      function  get_driver_version (port: Int32): Int32; override;
      function  get_voices (port: Int32): Int32; override;
      function  get_notes (port: Int32): Int32; override;
      function  get_support (port: Int32): uInt32; override;
      function  allocated_port (port: Int32): boolean; override;
      function  get_status (port: Int32): Int32; override;

      function  find_free_channel (var track: Int32): boolean;

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Open (port: Int32); override;
      procedure Close (port: Int32); override;

      function  get_MIDI_thru_port: TMIDI_Device_Output;
      function  MIDI_Thru_Open: boolean;
      function  find_likely_default: Int32; override;
      procedure notes_off;
      procedure synchronize (sync: boolean);
      procedure send_sync_signal;
      function  synced: boolean;
      procedure send_sync_message (MidiMessage: uInt8; Data1: uInt8; Data2: uInt8);
      procedure send_MIDI_message_to_port (port: Int32; MidiMessage: uInt8; Data1: uInt8; Data2: uInt8); overload;
      procedure send_MIDI_message_to_port (port: Int32; event: TMIDI_Event); overload;
      procedure send_MIDI_message (port: Int32; MidiMessage: uInt8; Data1: uInt8; Data2: uInt8); overload;
      procedure send_MIDI_message (event: TMIDI_Event); overload;

      property Port_Out [port: Int32]: TMIDI_Device_Output read get_out_port;
   end; // Class: TMIDI_Resources_Out //

///////////////////////////////////////////////////////////////////////////////////////////
   TMIDI_Resources_In = class (TMIDI_Resources)
   protected
      function  get_in_port (port: Int32): TMIDI_Device_Input;
      function  get_port_name (port: Int32): string; override;
      function  get_port_names (track: Int32): string; override;
      function  get_version (port: Int32): Int32; override;
      function  get_technology (port: Int32): OutPortTech; override;
      function  get_driver_version (port: Int32): Int32; override;
      function  get_voices (port: Int32): Int32; override;
      function  get_notes (port: Int32): Int32; override;
      function  get_support (port: Int32): uInt32; override;
      function  allocated_port (port: Int32): boolean; override;
      function  get_status (port: Int32): Int32; override;

      procedure set_onmidiinput (value: TOnMidiInput);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Open (port: Int32); override;
      procedure Close (port: Int32); override;

      procedure MIDI_Record (Sender: TObject);

      property Port_In [port: Int32]: TMIDI_Device_Input read get_in_port;
      property OnMidiInput: TOnMidiInput write set_onmidiinput;
   end; // Class: TMIDI_Resources_In //

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TMIDI_Resources_Out]);
   RegisterComponents (MSC_Package_Name, [TMIDI_Resources_In]);
end; // Register //

{*******************************************************************
*                                                                  *
* Class TMIDI_Resources                                            *
*                                                                  *
********************************************************************}

constructor TMIDI_Resources.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

   MIDI_Ports := TStringList.Create;
end; // Create //

destructor TMIDI_Resources.Destroy;
begin
   MIDI_Ports.Free;

   inherited Destroy;
end; // Destroy //

procedure TMIDI_Resources.set_port_capability (port: Int32; channel: TMIDI_Range;
                                               pc: TPort_Capability);
begin
   if (port >= 0) and (port < Count) then
   begin
      FCap [port][channel] := pc;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // set_port_capability //

function TMIDI_Resources.get_port_capability (port: Int32; channel: TMIDI_Range): TPort_Capability;
begin
   if (port >= 0) and (port < Count) then
   begin
      Result := FCap [port][channel];
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_port_capability //

procedure TMIDI_Resources.set_midi_thru (port: Int32);
begin
   if (port >= 0) and (port < Count) then
   begin
      FMIDI_Thru := port;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // set_midi_thru //

function TMIDI_Resources.get_port_device (port: Int32): TMIDI_Device;
begin
   if (port >= 0) and (port < Count) then
   begin
      Result := TMIDI_Device (MIDI_Ports.Objects [port]);
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_port_device //

procedure TMIDI_Resources.open_all;
var port: Int32;
begin
   for port := 0 to Count - 1 do
   begin
      Open (port);
   end; // for
end; // open_all //

procedure TMIDI_Resources.close_all;
var port: Int32;
begin
   for port := 0 to Count - 1 do
   begin
      Close (port);
   end; // for
end; // close_all //

procedure TMIDI_Resources.backup_and_close;
var p: Int32;
begin
   if Length (closed) <> Count then SetLength (closed, Count);
   for p := 0 to Count - 1 do
   begin
      closed [p] := True;
      if not Port_Device [p].is_closed then
      begin
         closed [p] := False;
         Close (p);
      end; // if
   end; // for
end; // backup_and_close //

procedure TMIDI_Resources.restore_and_open;
var p: Int32;
begin
   if Length (closed) <> Count
      then raise EMSC_Resource_Error.Create ('restore_and_open: Count of ports seems to ' +
                                             'have changed since backup');
   for p := 0 to Count - 1 do
   begin
      if ((closed [p]) and (Port_Device [p].is_opened)) or
         ((not closed [p]) and (Port_Device [p].is_closed))
         then EMSC_Resource_Error.Create ('restore_and_open: inconsistency between being ' +
                                          'open/close and expected open/close');
      if not closed [p] then Open (p);
   end; // for
end; // restore_and_open //

procedure TMIDI_Resources.port_disable (port: Int32);
var eChannel: Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      for eChannel := Low (TMIDI_Range) to High (TMIDI_Range)
         do Capability [port, eChannel] := pcDisabled;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // port_disable //

procedure TMIDI_Resources.port_play (port: Int32);
var eChannel: Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      for eChannel := Low (TMIDI_Range) to High (TMIDI_Range)
         do Capability [port, eChannel] := pcPlay;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // port_play //

procedure TMIDI_Resources.port_mute (port: Int32);
var eChannel: Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      for eChannel := Low (TMIDI_Range) to High (TMIDI_Range)
         do Capability [port, eChannel] := pcMute;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // port_mute //

function TMIDI_Resources.port_exclusive_play (port: Int32): boolean;
var device: Int32;
    eChannel: Int32;
begin
   port_exclusive_play := true;
   if not ((port >= 0) and (port < Count)) then
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if

   if Capability [port, 1] = pcDisabled then
   begin
      port_exclusive_play := false;
      Exit;
   end; // if
   for device := 0 to Count - 1 do
   begin
      for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
      begin
         if (Capability [device, eChannel] <> pcDisabled) then
         begin
            if (device = port)
               then Capability [device, eChannel] := pcPlay
               else Capability [device, eChannel] := pcMute;
         end; // if
      end; // for
   end; // for
end; // port_exclusive_play //

{*******************************************************************
*                                                                  *
* Class TMIDI_Resources_Out                                        *
*                                                                  *
********************************************************************}

constructor TMIDI_Resources_Out.Create (AOwner: TComponent);
var device: TMIDI_Device_Output;
    port: Int32;
begin
   inherited Create (AOwner);

   if not (csDesigning in ComponentState) then
   begin
      synchronization := false; // There is no sync with MIDI thru

// Count the number of MIDI output ports
      device := TMIDI_Device_Output.Create (nil);
      FCount := device.Numdevs;
      device.Free;

// Set array of capabilities to number of ports
      SetLength (FCap, Count);

      if Count <= 0
         then FMIDI_Thru := -1  // Means no MIDI thru available
         else FMIDI_Thru := 0;  // Choose a suitable default
// Create output ports
      for port := 0 to Count - 1 do
      begin
         device := TMIDI_Device_Output.Create (Self);
         device.DeviceID := port;
         MIDI_Ports.AddObject (device.ProductName, device);
         try // try to open the port
            device.open;
            port_play (port); // if succesful registrate as such
         except
            port_disable (port); // if not, mark ik as disabled
         end; // try

// Device is probably a MIDI output port. Assign to MIDI Thru
         if device.Technology = opt_MIDIPort then MIDI_Thru := port;
      end; // for
   end; // if
end; // Create //

destructor TMIDI_Resources_Out.Destroy;
var MIDI_Out: TMIDI_Device_Output;
    device: Int32;
begin
   if not (csDesigning in ComponentState) then
   begin
// Close and free all MIDI output devices
      for device := 0 to Count - 1 do
      begin
         MIDI_Out := Port_Out [device];
         MIDI_Out.Close;
         MIDI_Out.Free;
      end; // for
   end; // if

   inherited destroy;
end; // Destroy //

function TMIDI_Resources_Out.get_port_name (port: Int32): string;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_port_name := Port_Out [port].ProductName;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_port_name //

function TMIDI_Resources_Out.get_port_names (track: Int32): string;

// Returns the names of all available port names for a certain track,
// separated by slashes

var names: string;
    p: Int32;
begin
   names := '';
   for p := 0 to Count - 1 do
   begin
      if (Capability [p, track] = pcPlay) then
      begin
         if names = ''
            then names := get_port_name (p)
            else names := names + '/' + get_port_name (p);
      end; // if
   end; // for
   get_port_names := names;
end; // get_port_names //

function TMIDI_Resources_Out.get_version (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_version := Port_Out [port].DriverVersion;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_version //

function TMIDI_Resources_Out.get_technology (port: Int32): OutPortTech;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_technology := Port_Out [port].Technology;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_technology //

function TMIDI_Resources_Out.get_driver_version (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_driver_version := Port_Out [port].DriverVersion;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_driver_version //

function TMIDI_Resources_Out.get_voices (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_voices := Port_Out [port].Voices;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_voices //

function TMIDI_Resources_Out.get_notes (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_notes := Port_Out [port].Notes;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_notes //

function TMIDI_Resources_Out.get_support (port: Int32): uInt32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_support := Port_Out [port].Support;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_support //

function TMIDI_Resources_Out.allocated_port (port: Int32): boolean;
// Returns true if indexed port is open (allocated), else false
begin
   if (port >= 0) and (port < Count)
      then allocated_port := Port_Out [port].Status = mosOpen
      else allocated_port := false;
end; // allocated_port //

function  TMIDI_Resources_Out.get_status (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_status := Int32 (Port_Out [port].Status);
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_status //

function TMIDI_Resources_Out.get_out_port (port: Int32): TMIDI_Device_Output;
begin
   get_out_port := TMIDI_Device_Output (get_port_device (port));
end; // get_out_port //

procedure TMIDI_Resources_Out.Open (port: Int32);
begin
   Port_Out [port].Open;
end; // Open //

procedure TMIDI_Resources_Out.Close (port: Int32);
begin
   Port_Out [port].Close;
end; // Close //

function TMIDI_Resources_Out.find_free_channel (var track: Int32): boolean;

// Finds a free port and a channel that can be played to. This is useful if
// one wants to play music regardless where it comes from.

var eChannel: Int32;
    p: Int32;
begin
   find_free_channel := false;
   for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
   begin
      for p := 0 to Count - 1 do
      begin
         if Capability [p, eChannel] = pcPlay then
         begin
            find_free_channel := true;
            track := eChannel;
            Exit;
         end; // if
      end; // if
   end; // for
end; // find_free_channel //

function TMIDI_Resources_Out.find_likely_default: Int32;
var device: Int32;
    choice: Int32;
begin
   Result := no_ports_available;
   choice := -1;
   for device := 0 to Count - 1 do
   begin
      if get_port_capability (device, 1) <> pcDisabled then
      begin
         if (choice = -1) and (Technology [device] <> opt_MidiPort)
            then choice := device;
      end; // for
   end; // for
   if choice > -1 then Result := choice;
end; // find_likely_default //

function TMIDI_Resources_Out.MIDI_Thru_Open: boolean;
// Checks whether the MIDI out port is open, true if open
begin
   MIDI_Thru_Open := allocated_port (MIDI_Thru);
end; // MIDI_Thru_Open //

function TMIDI_Resources_Out.get_MIDI_thru_port: TMIDI_Device_Output;

// Returns a handle to the requested midi output port. Returns nil if
// index is outside range

begin
   get_MIDI_thru_port := nil;
   if (MIDI_Thru >= 0) and (MIDI_Thru < Count) then
   begin
      get_MIDI_thru_port := get_out_port (MIDI_Thru);
   end else
   begin
      if MIDI_Thru <> -1
         then EMSC_Resource_Error.CreateFmt ('TMIDI_Resources_Out.get_MIDI_thru_port: index out of bounds: ',
                                              [inttostr (MIDI_Thru)]);
   end; // if
end; // get_MIDI_thru_port //

procedure TMIDI_Resources_Out.send_MIDI_message_to_port
(
   port: Int32;       // Port to send message to
   MidiMessage: uInt8;   // MIDI message ID (MSb set)
   Data1: uInt8;         // Data uInt8 one
   Data2: uInt8          // and two
);

// Sends a standard MIDI message to a specific port

begin
   if allocated_port (port) then
   begin
      Port_Out [port].PutShort (MidiMessage, Data1, Data2);
   end else
   begin
      raise EMSC_Resource_Error.Create ('TMIDI_Resources_Out.send_MIDI_message_to_port: ' +
                           'device not allocated: ' + inttostr (port));
   end; // if
end; // send_MIDI_message_to_port //

procedure TMIDI_Resources_Out.send_MIDI_message_to_port (port: Int32; event: TMIDI_Event);
begin
   send_MIDI_message_to_port (port, event.MIDI_Message, event.Data_Byte_1, event.Data_Byte_2);
end; // send_MIDI_message_to_port //

// Sends a standard MIDI message to port for track
procedure TMIDI_Resources_Out.send_MIDI_message
(
   port: Int32;          // Port to send message to
   MidiMessage: uInt8;   // MIDI message ID (MSb set)
   Data1: uInt8;         // Data uInt8 one
   Data2: uInt8          // and two
);
var channel: TMIDI_Range;
begin
   channel := 1 + MidiMessage and $0F;
   if Capability [port, channel] = pcPlay
      then send_MIDI_message_to_port (Port, MidiMessage, Data1, Data2);
end; // send_MIDI_message //

procedure TMIDI_Resources_Out.send_MIDI_message (event: TMIDI_Event);
begin
   send_MIDI_message (event.Port, event.MIDI_Message, event.Data_Byte_1, event.Data_Byte_2);
end; // send_MIDI_message //

// Send an all notes off message to each MIDI port and resets all controllers
procedure TMIDI_Resources_Out.notes_off;
var i: uInt8;
    port: Int32;
begin
   port := 0;
   while port < Count do
   begin
      if Port_Out [port].Status = mosOpen then
      begin
         try
// Send an all notes off message to each MIDI channel. This is a control change message.
            for i := mc_MIDI_Control_Change to mc_MIDI_Control_Change + 15 do // was 176..191
            begin
               send_MIDI_message_to_port (port, i, mc_MIDI_All_Notes_Off, 0);
            end; // for

// Reset all controllers for each MIDI channel. This is a control change message.
            for i := mc_MIDI_Control_Change to mc_MIDI_Control_Change + 15 do
            begin
               send_MIDI_message_to_port (port, i, mc_MIDI_Reset_All_Controllers, 0);
            end; // for
         except
            on e:exception do // Nothing
            begin
// Sometimes exceptions are raised for unknown reasons.
// I suspect that that the note offs are being sent too fast
// Current workaround is to ignore them
            end; // on
         end; // try
      end; // if
      port := port + 1;
   end; // while
end; // all_notes_off //

procedure TMIDI_Resources_Out.synchronize (sync: boolean);
begin
   if sync then
   begin
      synchronization := true;
      if MIDI_Thru_Open then // Only when MIDI out port is open
      begin
         send_sync_message (mc_MIDI_Start, 0, 0); // Send it twice
         send_sync_message (mc_MIDI_Start, 0, 0); // for certainty
      end else
      begin
         synchronization := false;
         raise EMSC_Resource_Error.Create ('The MIDI output port: ' +
                      get_out_port (MIDI_Thru).ProductName +
                      ' is in use by another program. ' +
                      'Free it in the other program or select another ' +
                      'MIDI output port (and be sure it is deselected in ' +
                      'each other program).');
      end; // if
   end else
   begin
      synchronization := false;
      send_sync_message (mc_MIDI_Stop, 0, 0);
   end; // if
end; // synchronize //

function TMIDI_Resources_Out.synced: boolean;
begin
   synced := synchronization and MIDI_Thru_Open;
end; // synced //

procedure TMIDI_Resources_Out.send_sync_signal;
begin
   if synced then send_MIDI_message_to_port (MIDI_Thru, mc_MIDI_Continue, 0, 0);
end; // send_sync_signal //

procedure TMIDI_Resources_Out.send_sync_message (MidiMessage: uInt8; Data1: uInt8; Data2: uInt8);
begin
   if synced then send_MIDI_message_to_port (MIDI_Thru, MidiMessage, Data1, Data2);
end; // send_sync_message //

{*******************************************************************
*                                                                  *
* Class TMIDI_Resources_In                                         *
*                                                                  *
********************************************************************}

constructor TMIDI_Resources_In.Create (AOwner: TComponent);
var device: TMIDI_Device_Input;
    port: Int32;
begin
   inherited Create (AOwner);

   if not (csDesigning in ComponentState) then
   begin
// Count the number of MIDI input ports
//      device := TMIDI_Device_Input.Create (nil);
      FCount := midiInGetNumDevs;//device.Numdevs;
//      device.Free;

// Set array of capabilities to number of ports
      SetLength (FCap, Count);

// Create input ports
      for port := 0 to Count - 1 do
      begin
         device := TMIDI_Device_Input.Create (Self);
         device.DeviceID := port;
         device.OnMidiInput := MIDI_Record;
         MIDI_Ports.AddObject (device.ProductName, device);
         try // try to open the port
            device.open;
            port_play (port); // if succesful registrate as such
         except
            port_disable (port); // if not, mark it as disabled
         end; // try

// Start ??
      end; // for
   end; // if
end; // Create //

destructor TMIDI_Resources_In.Destroy;
var MIDI_In: TMIDI_Device_Input;
    device: Int32;
begin
	if not (csDesigning in ComponentState) then
   begin
// Close and free all MIDI input devices
      for device := 0 to Count - 1 do
      begin
         MIDI_In := Port_In [device];
         MIDI_In.Close;
         MIDI_In.Free;
      end; // for
   end; // if
   
   inherited destroy;
end; // Destroy //

procedure TMIDI_Resources_In.MIDI_Record (Sender: TObject);

// Example event handler that is assigned to the MIDI_Device_Input
// FOnMidiInput handler. Replace this one by your own event handler
// when you implement TMIDI_Resources_In by assigning a routine
// to FOnMidiInput
// This routine is a dummy procedure, it creates a MIDI_Event and
// frees it.

var thisEvent: TMIDI_Event;
begin
   with (Sender As TMIDI_Device_Input) do
   begin
      while (MessageCount > 0) do
      begin
         thisEvent := GetMidiEvent; // Convert sender to MIDI Event
                                    // Do something with the event
         thisEvent.Free;            // and free it (when it is no longer needed)
      end; // while
   end; // with
end; // MIDI_Record //

function TMIDI_Resources_In.get_port_name (port: Int32): string;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_port_name := Port_In [port].ProductName;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_port_name //

function TMIDI_Resources_In.get_port_names (track: Int32): string;

// Returns the names of all available port names for a certain track,
// separated by slashes

var names: string;
    p: Int32;
begin
   names := '';
   for p := 0 to Count - 1 do
   begin
      if (Capability [p, track] = pcPlay) then
      begin
         if names = ''
            then names := get_port_name (p)
            else names := names + '/' + get_port_name (p);
      end; // if
   end; // for
   get_port_names := names;
end; // get_port_names //

function TMIDI_Resources_In.get_version (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_version := Port_In [port].DriverVersion;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_version //

function TMIDI_Resources_In.get_technology (port: Int32): OutPortTech;
begin
   get_technology := OutPortTech (0); // N/A for input devices
end; // get_technology //

function TMIDI_Resources_In.get_driver_version (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_driver_version := Port_In [port].DriverVersion;
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_driver_version //

function TMIDI_Resources_In.get_voices (port: Int32): Int32;
begin
   get_voices := -1; // N/A for input devices
end; // get_voices //

function TMIDI_Resources_In.get_notes (port: Int32): Int32;
begin
   get_notes := -1; // N/A for input devices
end; // get_notes //

function TMIDI_Resources_In.get_support (port: Int32): uInt32;
begin
   get_support := 0; // N/A for input devices
end; // get_support //

function TMIDI_Resources_In.allocated_port (port: Int32): boolean;
// Returns true if indexed port is open (allocated), else false
begin
   if (port >= 0) and (port < Count)
      then allocated_port := Port_In [port].Status = misOpen
      else allocated_port := false;
end; // allocated_port //

function  TMIDI_Resources_In.get_status (port: Int32): Int32;
begin
   if (port >= 0) and (port < Count) then
   begin
      get_status := Int32 (Port_In [port].Status);
   end else
   begin
      raise EMSC_Resource_Error.CreateFmt ('port (%d) out of range [0..%d]', [port, Count - 1]);
   end; // if
end; // get_status //

function TMIDI_Resources_In.get_in_port (port: Int32): TMIDI_Device_Input;
begin
   get_in_port := TMIDI_Device_Input (get_port_device (port));
end; // get_port //

procedure TMIDI_Resources_In.set_onmidiinput (value: TOnMidiInput);
var port: Int32;
begin
   for port := 0 to Count - 1 do
   begin
      Port_In [port].OnMidiInput := value;
   end; // for
end; // set_onmidiinput //

procedure TMIDI_Resources_In.Open (port: Int32);
begin
   Port_In [port].Open;
end; // Open //

procedure TMIDI_Resources_In.Close (port: Int32);
begin
   Port_In [port].Close;
end; // Close //

end. // Unit: MIDI_Resources //
