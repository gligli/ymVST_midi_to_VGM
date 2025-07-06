unit Demo_Resources_View;

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
  Windows, SysUtils, Classes, StdCtrls, ComCtrls, Controls, ImgList, Forms,
  MSC_Definitions, MSC_Out_Device, MSC_Resources;

type
   TView_Columns = (rosNames, rosTech, rosDriver, rosVoices, rosNotes,
                    rosAvailable, rosEnabled, rosMisc);

const
   vcHeaders: array [TView_Columns] of string = ('Name', 'Technology', 'Driver', 'Voices',
                                                 'Notes', 'Available', 'Enabled', 'Misc.');
   vcWidths: array [TView_Columns] of integer = (200, 80, 40, 40, 40, 40, 40, 60);

type
  TDemo_Resources_View = class (TListView)
      procedure List_MIDI_OutMouseDown (Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);

   private
      FMIDI_Res: TMIDI_Resources; // Ref to MIDI resources
      MIDI_Out: integer; // MIDI output port

      function  get_MIDI_res: TMIDI_Resources;
      procedure set_MIDI_res (mr: TMIDI_Resources);
      function  n_devices: integer;
//      function  get_device_name (port: integer): string;
      procedure Check_MIDI_Out (X, Y: integer);
      procedure Check_Allocate (X, Y: integer);

   protected
      procedure Notification (AComponent: TComponent; Operation: TOperation); override;

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset;

      procedure Show;
      procedure port_disable (port: integer);
      procedure port_play (port: integer);
      function  port_exclusive_play (port: integer): boolean;
      procedure port_mute (port: integer);

   published
      property MIDI_Res: TMIDI_Resources read get_MIDI_res write set_MIDI_res;
   end; // Class: TDemo_Resources_View //

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TDemo_Resources_View]);
end; // Register //

constructor TDemo_Resources_View.Create (AOwner: TComponent);
var col: TListColumn;
    c: TView_Columns;
begin
   inherited Create (AOwner);

   ViewStyle     := vsReport;
   CheckBoxes    := True;
   ReadOnly      := True;
   GridLines     := True;
   RowSelect     := True;
   HideSelection := False;
   OnMouseDown   := List_MIDI_OutMouseDown;

   for c := Low (TView_Columns) to High (TView_Columns) do
   begin
      col := Columns.Add;
      col.Caption := vcHeaders [c];
      col.Width   := vcWidths  [c];
   end; // for
   MIDI_Res := nil;
end; // Create //

destructor TDemo_Resources_View.Destroy;
begin
   inherited destroy;
end; // Destroy //

procedure TDemo_Resources_View.Reset;
begin
end; // Reset //

procedure TDemo_Resources_View.Notification (AComponent: TComponent; Operation: TOperation);
begin
   if Operation = opRemove then
   begin
      if AComponent is TMIDI_Resources_Out then MIDI_Res := nil;
   end; // if
end; // Notification //

function TDemo_Resources_View.get_MIDI_res: TMIDI_Resources;
begin
   Result := FMIDI_Res;
end; // get_MIDI_res //

procedure TDemo_Resources_View.set_MIDI_res (mr: TMIDI_Resources);
begin
   FMIDI_Res := mr;
//   if FMIDI_Res <> nil then
//   begin
//      Show;
//   end; // if
end; // set_MIDI_res //

procedure TDemo_Resources_View.List_MIDI_OutMouseDown
(
   Sender: TObject;
   Button: TMouseButton;
   Shift: TShiftState;
   X, Y: Integer
);
var item: TListItem;
begin
   item := GetItemAt (X, Y);
   if item = nil then Exit;
   if Button = mbLeft then
   begin
      Selected := item;
      if GetHitTestInfoAt (X, Y) = [htOnStateIcon] then Check_Allocate (X, Y) else
      if GetHitTestInfoAt (X, Y) = [htOnIcon]      then Check_MIDI_Out (X, Y);
   end; // if
   Show;
end; // List_MIDI_OutMouseDown //

procedure TDemo_Resources_View.Show;
var item: TListItem;
    device: integer;
    caps: string;
begin
// Show all device in the List_MIDI_Out view
//   Items.BeginUpdate;
   Items.Clear;
   if MIDI_Res = nil then
   begin
      item := Items.Add;
      item.Caption := 'No resources available';
      Items.EndUpdate;
      Exit;
   end; // if
   device := 0;
   while device < n_devices do
   begin
      item := Items.Add;
      item.Caption := MIDI_Res.ProductName [device];
      case MIDI_Res.Technology [device] of
         opt_None:     Item.SubItems.Add ('None');
         opt_MidiPort: Item.SubItems.Add ('MIDI Port');
         opt_Synth:    Item.SubItems.Add ('Generic Synth');
         opt_SQSynth:  Item.SubItems.Add ('Square Wave Synth');
         opt_FMSynth:  Item.SubItems.Add ('FM Synth');
         opt_Mapper:   Item.SubItems.Add ('MIDI Mapper');
         else          Item.SubItems.Add (IntToStr (integer (MIDI_Res.Technology [device])));
      end; // case
      item.SubItems.Add (inttostr (MIDI_Res.DriverVersion [device]));
      Item.SubItems.Add (inttostr (MIDI_Res.Voices [device]));
      Item.SubItems.Add (inttostr (MIDI_Res.Notes [device]));
      item := Items [device];
      if (MIDI_Res.MIDI_Thru = device) or (MIDI_Res is TMIDI_Resources_In) then
      begin
         Item.SubItems.Add ('Yes');
         item.ImageIndex := 1;
      end else
      begin
         Item.SubItems.Add ('No');
         item.ImageIndex := 0;
      end; // if
      if midioutputState (MIDI_Res.Status [device]) = mosOpen then
      begin
         Item.SubItems.Add ('Yes');
         item.StateIndex := 2;
      end else
      begin
         Item.SubItems.Add ('No');
         item.StateIndex := 0;
      end; // if
      if MIDI_Res is TMIDI_Resources_Out then
      begin
         caps := '';
         if TMIDI_Resources_Out (MIDI_Res).Port_Out [device].Volume then caps := caps + 'V ';
         if TMIDI_Resources_Out (MIDI_Res).Port_Out [device].LRVolume then caps := caps + 'LR ';
         if TMIDI_Resources_Out (MIDI_Res).Port_Out [device].PatchCache then caps := caps + 'C ';
         Item.SubItems.Add (caps);
      end; // if
      case MIDI_Res.Capability [device, 1] of
         pcDisabled: item.ImageIndex := 0;
         pcMute:     item.ImageIndex := 1;
         pcPlay:     item.ImageIndex := 2;
      end; // case
      device := device + 1;
      Application.ProcessMessages;
   end; // while
//   Items.EndUpdate;
end; // Show //

function TDemo_Resources_View.n_devices: integer;
// Returns number of devices
begin
   if MIDI_Res = nil
      then Result := -1
      else Result := MIDI_Res.Count;
end; // n_devices //

procedure TDemo_Resources_View.port_disable (port: integer);
begin
   if MIDI_Res <> nil then MIDI_Res.port_disable (port);
end; // port_disable //

procedure TDemo_Resources_View.port_play (port: integer);
begin
   if MIDI_Res <> nil then MIDI_Res.port_play (port);
end; // port_play //

function TDemo_Resources_View.port_exclusive_play (port: integer): boolean;
begin
   Result := False;
   if MIDI_Res <> nil
      then Result := MIDI_Res.port_exclusive_play (port);
end; // port_exclusive_play //

procedure TDemo_Resources_View.port_mute (port: integer);
begin
   if MIDI_Res <> nil then MIDI_Res.port_mute (port);
end; // port_mute //
{
function TDemo_Resources_View.get_device_name (port: integer): string;
// Returns name of indexed MIDI port
begin
   Result := '';
   if (MIDI_Res <> nil) and (port >= 0) and (port < n_devices)
      then Result := MIDI_Res.ProductName [port];
end; // get_device_name //
}
procedure TDemo_Resources_View.Check_MIDI_Out (X, Y: integer);
var item: TListItem;
    device: integer;
begin
   item := GetItemAt (X, Y);
   if item <> nil then
   begin
      device := 0;
      while device < n_devices do
      begin
         Items [device].ImageIndex := 0;
         device := device + 1;
      end; // while
      SetFocus;
      Selected := item;
      if item.Index <> MIDI_Out then
      begin
         MIDI_Out := item.Index;
         Items [MIDI_Out].ImageIndex := 2;
      end else
      begin
         MIDI_Out := no_ports_available;
      end; // if
   end; // if
end; // Check_MIDI_Out //

procedure TDemo_Resources_View.Check_Allocate (X, Y: integer);
var port: integer;
    item: TListItem;
begin
   item := GetItemAt (X, Y);
   if (MIDI_Res <> nil) and (item <> nil) then
   begin
      SetFocus;
      port := Item.Index;
      Selected := item;
      if MIDI_Res.Capability [port, 1] <> pcDisabled then
      begin
         if midioutputState (MIDI_Res.Status [port]) <> mosOpen then
         begin
            try
               MIDI_Res.Open (port);
               port_play (port);
            except
               on e:exception
                  do port_disable (port);
            end; // try
         end else
         begin
            MIDI_Res.Close (port);
            port_mute (port);
            try
               MIDI_Res.Close (port);
               Item.ImageIndex := 2;
            except
               on e:exception do ; // Nothing
            end; // try
         end; // if
      end; // if
   end; // if
end; // Check_Allocate //

end. // Unit: Demo_Resources_View //
