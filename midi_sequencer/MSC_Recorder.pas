unit MSC_Recorder;

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

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ComCtrls, Buttons, ToolWin, ExtCtrls, MMSystem,
     MSC_Definitions, MSC_Component, MSC_Container, MSC_In_Device, MSC_Resources;

type
   EMSC_Recorder = class (Exception);

   TMIDI_Recorder = class (TMSC_Component)
   private
      FMIDI_Container: TMIDI_Container;
      FMIDI_Res_In: TMIDI_Resources_In;
      FRecording: boolean;

      procedure set_MIDI_container (value: TMIDI_Container);
      procedure set_MIDI_sound_cards (value: TMIDI_Resources_In);

   protected
      procedure Notification (AComponent: TComponent; Operation: TOperation); override;

      function  get_recording: boolean;
      procedure set_recording (value: boolean);
      procedure set_onmidiinput (value: TOnMidiInput);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;

   published
      property Recording: boolean read get_recording write set_recording;
      property MIDI_Container: TMIDI_Container
               read FMIDI_Container write set_MIDI_Container;
      property MIDI_Res_In: TMIDI_Resources_In
               read FMIDI_Res_In write set_MIDI_sound_cards;
      property OnMidiInput: TOnMidiInput write set_onmidiinput;
   end; // Class: TMIDI_Recorder //

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TMIDI_Recorder]);
end;

{*******************************************************************
*                                                                  *
* Class TMIDI_Recorder                                             *
*                                                                  *
********************************************************************}

constructor TMIDI_Recorder.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

// Set properties during design time
   MIDI_Container := nil;
   MIDI_Res_In    := nil;

// And the rest during creation time
   if not (csDesigning in ComponentState) then
   begin
   end; // if
end; // Create //

destructor TMIDI_Recorder.Destroy;
begin
   if not (csDesigning in ComponentState) then
   begin
   end; // if

   inherited destroy;
end; // Destroy //

function TMIDI_Recorder.get_recording: boolean;
begin
   get_recording := FRecording;
end; // get_recording //

procedure TMIDI_Recorder.set_recording (value: boolean);
var MIDI_In: TMIDI_Device_Input;
    port: Int32;
begin
   FRecording := value;

   for port := 0 to MIDI_Res_In.Count - 1 do
   begin
      MIDI_In := MIDI_Res_In.Port_In [port];
      if MIDI_In.Status = misOpen then
      begin
         if FRecording
            then MIDI_In.Start
            else MIDI_In.Stop;
      end; // if
   end; // for
end; // set_recording //

procedure TMIDI_Recorder.set_onmidiinput (value: TOnMidiInput);
begin
   if MIDI_Res_In <> nil then
   begin
      MIDI_Res_In.OnMidiInput := value;
   end; // if
end; // set_onmidiinput //

procedure TMIDI_Recorder.Notification (AComponent: TComponent; Operation: TOperation);
begin
   if Operation = opRemove then
   begin
      if AComponent is TMIDI_Resources_In then MIDI_Res_In    := nil;
   end; // if
end; // Notification //

procedure TMIDI_Recorder.set_MIDI_container (value: TMIDI_Container);
begin
   FMIDI_Container := value;
end; // set_MIDI_container //

procedure TMIDI_Recorder.set_MIDI_sound_cards (value: TMIDI_Resources_In);
begin
   FMIDI_Res_In := value;
end; // set_MIDI_sound_cards //

end. // Unit: MSC_Player //
