{ $Header: /MidiComp/MidiOut.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

{ Thanks very much to Fred Kohler for the Technology code. }

unit MSC_Out_Device;

{
  MIDI Output component.

  Properties:
   DeviceID: 	Windows numeric device ID for the MIDI output device.
   Between 0 and (midioutGetNumDevs-1), or MIDI_MAPPER (-1).
    Special value MIDI_MAPPER specifies output to the Windows MIDI mapper
   Read-only while device is open, exception if changed while open

   MIDIHandle:	The output handle to the MIDI device.
   0 when device is not open
   Read-only, runtime-only

   ProductName: Name of the output device product that corresponds to the
   DeviceID property (e.g. 'MPU 401 out').
   You can write to this while the device is closed to select a particular
   output device by name (the DeviceID property will change to match).
   Exception if this property is changed while the device is open.

   Numdevs: Number of MIDI output devices installed on the system. This
   is the value returned by midiOutGetNumDevs. It's included for
   completeness.

    Technology: Type of technology used by the MIDI device. You can set this
    property to one of the values listed for OutportTech (below) and the component
    will find an appropriate MIDI device. For example:
      MidiOutput.Technology := opt_FMSynth;
    will set MidiInput.DeviceID to the MIDI device ID of the FM synth, if one
    is installed. If no such device is available an exception is raised,
    see MidiOutput.SetTechnology.

   See the MIDIOUTCAPS entry in MMSYSTEM.HLP for descriptions of the
   following properties:
      DriverVersion
      Voices
      Notes
      ChannelMask
      Support

   Error: The error code for the last MMSYSTEM error. See the MMSYSERR_
   entries in MMSYSTEM.INT for possible values.

   Methods:
   Open: Open MIDI device specified by DeviceID property for output

   Close: Close device

   PutMidiEvent(Event:TMyMidiEvent): Output a note or sysex message to the
   device. This method takes a TMyMidiEvent object and transmits it.
   Notes:
     1. If the object contains a sysex event the OnMidiOutput event will
        be triggered when the sysex transmission is complete.
     2. You can queue up multiple blocks of system exclusive data for
        transmission by chucking them at this method; they will be
       transmitted as quickly as the device can manage.
     3. This method will not free the TMyMidiEvent object, the caller
        must do that. Any sysex data in the TMyMidiEvent is copied before
       transmission so you can free the TMyMidiEvent immediately after
       calling PutMidiEvent, even if output has not yet finished.

   PutShort(MidiMessage: uInt8; Data1: uInt8; Data2: uInt8): Output a short
   MIDI message. Handy when you can't be bothered to build a TMyMidiEvent.
   If the message you're sending doesn't use Data1 or Data2, set them to 0.

   PutLong(TheSysex: Pointer; msgLength: uInt16): Output sysex data.
      SysexPointer: Pointer to sysex data to send
      msgLength: Length of sysex data.
   This is handy when you don't have a TMyMidiEvent.

   SetVolume(Left: uInt16, Right: uInt16): Set the volume of the
   left and right channels on the output device (only on internal devices?).
   0xFFFF is maximum volume. If the device doesn't support separate
   left/right volume control, the value of the Left parameter will be used.
   Check the Support property to see whether the device supports volume
   control. See also other notes on volume control under midiOutSetVolume()
   in MMSYSTEM.HLP.

   Events:
   OnMidiOutput: Procedure called when output of a system exclusive block
   is completed.

   Notes:
   I haven't implemented any methods for midiOutCachePatches and
   midiOutCacheDrumpatches, mainly 'cause I don't have any way of testing
   them. Does anyone really use these?
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms, MMSystem,
  MSC_Definitions, MSC_Device, MSC_Circular_Buffer, MSC_Delphi_MIDI_Callback;

type
   midioutputState = (mosOpen, mosClosed);
   EMSC_Device_Out_Error = class (EMSC_Device_Error);

   { These are the equivalent of constants prefixed with mod_
   as defined in MMSystem. See SetTechnology }
   OutPortTech = (
   opt_None,         { none }
   opt_MidiPort,     { output port }
   opt_Synth,        { generic internal synth }
   opt_SQSynth,      { square wave internal synth }
   opt_FMSynth,      { FM internal synth }
   opt_Mapper);      { MIDI mapper }
   TechNameMap = array [OutPortTech] of string [18];


const
   TechName : TechNameMap = (
        'None', 'MIDI Port', 'Generic Synth', 'Square Wave Synth',
        'FM Synth', 'MIDI Mapper');

{-------------------------------------------------------------------}
type
   TMIDI_Device_Output = class (TMIDI_Device)
   protected
      FState: midioutputState;   // Current device state
      FMIDIHandle: Hmidiout;     // Handle to output device
      PCtlInfo: PMidiCtlInfo;    // Pointer to control info for DLL
      PBuffer: PCircularBuffer;  // Output queue for PutTimedEvent, set by Open

// Stuff from midioutCAPS
      FTechnology: OutPortTech;  // Type of MIDI output device
      FVoices: uInt16;           // Number of voices (internal synth)
      FNotes: uInt16;            // Number of notes (internal synth)
      FChannelMask: uInt16;      // Bit set for each MIDI channels that the
                                 // device responds to (internal synth)
      FSupport: uInt32;          // Technology supported (volume control,
                                 // patch caching etc.
      FVolume: boolean;          // True if volume control is supported
      FLRVolume: boolean;        // True if independent volume control is supported for each sereo channel
      FPatchCache: boolean;      // True if patch caching is supported
      FOnMIDIOutput: TNotifyEvent; // Sysex output finished

      procedure MidiOutput (var Message: TMessage);
      procedure SetDeviceID (DeviceID: Int32); override;
      procedure SetProductName (NewProductName: String); override;
      procedure SetTechnology (NewTechnology : OutPortTech);
      function  midioutErrorString (WError: uInt16): String;

  public
      constructor Create (AOwner:TComponent); override;
      destructor Destroy; override;
      function Open: Boolean; override;
      function Close: Boolean; override;
      function is_opened: boolean; override;
      function is_closed: boolean; override;
      function Status: midioutputState;
      procedure PutMidiEvent (theEvent: TMIDI_Event); virtual;
      procedure PutShort (MidiMessage: uInt8; Data1: uInt8; Data2: uInt8); virtual;
      procedure PutLong (TheSysex: Pointer; msgLength: uInt16); virtual;
      procedure SetVolume (Left: uInt16; Right: uInt16);

      property MIDIHandle: Hmidiout read FMIDIHandle;
      property Technology: OutPortTech   // Type of MIDI output device
          read FTechnology write SetTechnology default opt_Synth;
      property Voices: uInt16 read FVoices; // Number of voices (internal synth)
      property Notes: uInt16 read FNotes;   // Number of notes (internal synth)
      property ChannelMask: uInt16 read FChannelMask; // Bit set for each MIDI channels that the
                                                    // device responds to (internal synth)
      property Support: uInt32	read FSupport; // Technology supported (volume control, patch caching etc.
      property Volume: boolean read FVolume; // True if device supports volume, else False
      property LRVolume: boolean read FLRVolume; // True if device supports independent volume settings
                                                 // for right and left channel
      property PatchCache: boolean read FPatchCache; // True if device supports Patch caching

   published
      property Onmidioutput: TNotifyEvent read FOnmidioutput write FOnmidioutput;
end; // Class: mTMidiOutput //

procedure Register;

{-------------------------------------------------------------------}
implementation

{-------------------------------------------------------------------}
procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TMIDI_Device_Output]);
end; // Register

{-------------------------------------------------------------------}
constructor TMIDI_Device_Output.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);

   FState   := mosClosed;
   FNumdevs := midiOutGetNumDevs;

   { Create the window for callback notification }
   if not (csDesigning in ComponentState) then
   begin
      Handle := AllocateHwnd(MidiOutput);
   end; // if
end; // Create //

{-------------------------------------------------------------------}
destructor TMIDI_Device_Output.Destroy;
begin
   if FState = mosOpen then Close;
   if (PCtlInfo <> Nil) then GlobalSharedLockedFree( PCtlinfo^.hMem, PCtlInfo );
   DeallocateHwnd(Handle);

   inherited Destroy;
end; // Destroy //

{-------------------------------------------------------------------}
{ Convert the numeric return code from an MMSYSTEM function to a string
  using midioutGetErrorText. TODO: These errors aren't very helpful
  (e.g. "an invalid parameter was passed to a system function") so
  some proper error strings would be nice. }


function TMIDI_Device_Output.midioutErrorString (WError: uInt16): String;
var errorDesc: PChar;
begin
   errorDesc := Nil;
   try
      errorDesc := StrAlloc (MAXERRORLENGTH);
      if midioutGetErrorText( WError, errorDesc, MAXERRORLENGTH) = 0
         then result := StrPas (errorDesc)
         else result := 'Specified error number is out of range';
   finally
      If errorDesc <> Nil Then StrDispose(errorDesc);
   end; // try
end; // midioutErrorString //

{-------------------------------------------------------------------}
{ Set the output device ID and change the other properties to match }
procedure TMIDI_Device_Output.SetDeviceID (DeviceID: Int32);
var
   midioutCaps: TmidioutCaps;
begin
   if FState = mosOpen then
   begin
      raise EMSC_Device_Out_Error.Create('Change to DeviceID while device was open')
   end else
   begin
      if (DeviceID >= Int32 (midioutGetNumDevs)) And (DeviceID <> Int32 (MIDI_MAPPER)) then
      begin
         raise EMSC_Device_Out_Error.Create('Invalid device ID')
      end else
      begin
         FDeviceID := DeviceID;

// Set the name and other midioutCAPS properties to match the ID
         FError := midioutGetDevCaps (DeviceID, @midioutCaps, sizeof (TmidioutCaps));
         if Ferror > 0 then raise EMSC_Device_Out_Error.Create (midioutErrorString (FError));

         with midiOutCaps do
         begin
            FProductName   := string (StrPas(szPname));
            FDriverVersion := vDriverVersion;
            FTechnology    := OutPortTech (wTechnology);
            FVoices        := wVoices;
            FNotes         := wNotes;
            FChannelMask   := wChannelMask;
            FSupport       := dwSupport;
            FVolume        := boolean (dwSupport and MIDICAPS_VOLUME);
            FLRVolume      := boolean (dwSupport and MIDICAPS_LRVOLUME);
            FPatchCache    := boolean (dwSupport and MIDICAPS_CACHE);
         end; // with
      end; // if
   end; // if
end; // SetDeviceID //

{-------------------------------------------------------------------}
{ Set the product name property and put the matching output device number
  in FDeviceID.
  This is handy if you want to save a configured output/output device
  by device name instead of device number, because device numbers may
  change if users install or remove MIDI devices.
  Exception if output device with matching name not found,
  or if output device is open }
procedure TMIDI_Device_Output.SetProductName (NewProductName: String);
var
   midioutCaps: TmidioutCaps;
   testDeviceID: Int32;
   testProductName: String;
begin
   if FState = mosOpen
      then raise EMSC_Device_Out_Error.Create('Change to ProductName while device was open')
      else
      { Don't set the name if the component is reading properties because
      the saved Productname will be from the machine the application was compiled
      on, which may not be the same for the corresponding DeviceID on the user's
      machine. The FProductname property will still be set by SetDeviceID }
   if not (csLoading in ComponentState) then
   begin
      { Loop uses -1 to test for MIDI_MAPPER as well }
      for testDeviceID := -1 To (midioutGetNumDevs-1) do
      begin
         FError := midioutGetDevCaps(testDeviceID, @midioutCaps, sizeof(TmidioutCaps));
         if Ferror > 0
            then raise EMSC_Device_Out_Error.Create(midioutErrorString(FError));
         testProductName := string (StrPas(midioutCaps.szPname));
         if testProductName = NewProductName then
         begin
            FProductName := NewProductName;
            Break;
         end; // if
      end; // if
      if FProductName <> NewProductName
         then raise EMSC_Device_Out_Error.Create('MIDI output Device ' + NewProductName + ' not installed')
         else SetDeviceID(testDeviceID);
   end; // if
end; // SetProductName //

{-------------------------------------------------------------------}
{ Set the output technology property and put the matching output device
  number in FDeviceID.
  This is handy, for example, if you want to be able to switch between a
  sound card and a MIDI port }
procedure TMIDI_Device_Output.SetTechnology (NewTechnology: OutPortTech);
var
   midiOutCaps: TMidiOutCaps;
   testDeviceID: Int32;
   testTechnology: OutPortTech;
begin
   if FState = mosOpen
      then raise EMSC_Device_Out_Error.Create(
         'Change to Product Technology while device was open')
      else
   begin
       { Loop uses -1 to test for MIDI_MAPPER as well }
      for testDeviceID := -1 To (midiOutGetNumDevs-1) do
      begin
         FError :=
            midiOutGetDevCaps(testDeviceID,
               @midiOutCaps, sizeof(TMidiOutCaps));
         if Ferror > 0 then
            raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError));
         testTechnology := OutPortTech(midiOutCaps.wTechnology);
         if testTechnology = NewTechnology then
         begin
            FTechnology := NewTechnology;
            Break;
         end; // if
      end; // for
         if FTechnology <> NewTechnology
            then raise EMSC_Device_Out_Error.Create('MIDI output technology ' +
               string (TechName [NewTechnology]) + ' not installed')
            else SetDeviceID(testDeviceID);
   end; // if
end; // SetTechnology //

{-------------------------------------------------------------------}
{$HINTS OFF} // Avoid warning of result never being used
function TMIDI_Device_Output.Open: Boolean;
var hMem: THandle;
begin
   Result := False;
   try
{ Create the control info for the DLL }
      if (PCtlInfo = Nil) then
      begin
         PCtlInfo := GlobalSharedLockedAlloc( Sizeof(TMidiCtlInfo), hMem );
         PctlInfo^.hMem := hMem;
      end; // if

      Pctlinfo^.hWindow := Handle;	{ Control's window handle }

      FError := midioutOpen (@FMidiHandle, FDeviceId,
                              uInt32 (@midiHandler),
                              uInt32 (PCtlInfo),
                              CALLBACK_FUNCTION);
      If (FError <> 0) then
      begin
         raise EMSC_Device_Out_Error.Create(midioutErrorString(FError))
      end else
      begin
         Result := True;
         FState := mosOpen;
      end; // if

   except
      if PCtlInfo <> Nil then
      begin
         GlobalSharedLockedFree(PCtlInfo^.hMem, PCtlInfo);
         PCtlInfo := Nil;
      end; // if
      raise;
   end; // try
end; // Open //
{$HINTS ON}

{-------------------------------------------------------------------}
function TMIDI_Device_Output.Close: Boolean;
begin
   Result := False;
   if FState = mosOpen then
   begin

{ Note this sends a lot of fast control change messages which some synths can't handle.
 TODO: Make this optional. }
   {
      FError := midioutReset(FMidiHandle);
      if Ferror <> 0 then
         raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError));
   }
      FError := midioutClose(FMidiHandle);
      if Ferror <> 0
         then raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError))
         else Result := True;
   end; // if

   FMidiHandle := 0;
   FState := mosClosed;
end; // Close //

function TMIDI_Device_Output.is_opened: boolean;
begin
   Result := Status = mosOpen;
end; // is_opened //

function TMIDI_Device_Output.is_closed: boolean; 
begin
   Result := Status = mosClosed;
end; // is_closed //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Output.PutShort (MidiMessage: uInt8; Data1: uInt8; Data2: uInt8);
var thisMsg: uInt32;
begin
   thisMsg := uInt32(MidiMessage) Or
      (uInt32(Data1) shl 8) Or
      (uInt32(Data2) shl 16);

   FError := midiOutShortMsg(FMidiHandle, thisMsg);
   if Ferror > 0
      then raise EMSC_Device_Out_Error.Create(midioutErrorString(FError));
end; // PutShort//

{-------------------------------------------------------------------}
procedure TMIDI_Device_Output.PutLong (TheSysex: Pointer; msgLength: uInt16);
{ Notes: This works asynchronously; you send your sysex output by
calling this function, which returns immediately. When the MIDI device
driver has finished sending the data the MidiOutPut function in this
component is called, which will in turn call the OnMidiOutput method
if the component user has defined one. }

var MyMidiHdr: TMyMidiHdr;
begin
   { Initialize the header and allocate buffer memory }
   MyMidiHdr := TMyMidiHdr.Create(msgLength);

   { Copy the data over to the MidiHdr buffer
     We can't just use the caller's PChar because the buffer memory
     has to be global, shareable, and locked. }
   Move(MyMidiHdr.SysexPointer, TheSysex, msgLength);

   { Store the MyMidiHdr address in the header so we can find it again quickly
      (see the MidiOutput proc) }
   MyMidiHdr.hdrPointer^.dwUser := uInt32(MyMidiHdr);

   { Get MMSYSTEM's blessing for this header }
   FError := midiOutPrepareHeader(FMidiHandle,MyMidiHdr.hdrPointer, sizeof(TMIDIHDR));
   if Ferror > 0
      then raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError));

   { Send it }
   FError := midiOutLongMsg(FMidiHandle, MyMidiHdr.hdrPointer, sizeof(TMIDIHDR));
   if Ferror > 0
      then raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError));
end; // PutLong //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Output.PutMidiEvent (theEvent: TMIDI_Event);
begin
   if FState <> mosOpen then
      raise EMSC_Device_Out_Error.Create('MIDI Output device not open');

   with theEvent do
   begin
      if Sysex = Nil
         then PutShort (MIDI_Message, Data_Byte_1, Data_Byte_2)
         else PutLong (Sysex, SysexLength);
   end; // with
end; // PutMidiEvent //

function TMIDI_Device_Output.Status: midioutputState;
begin
   Result := FState;
end; // Status //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Output.SetVolume (Left: uInt16; Right: uInt16);
var
   dwVolume: uInt32;
begin
   dwVolume := (uInt32(Left) shl 16) Or Right;
   FError := midiOutSetVolume(DeviceID, dwVolume);
   if Ferror <> 0 then
      raise EMSC_Device_Out_Error.Create(MidiOutErrorString(FError));
end; // SetVolume //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Output.midioutput (var Message: TMessage);
{ Triggered when sysex output from PutLong is complete }
var MyMidiHdr: TMyMidiHdr;
    thisHdr: PMidiHdr;
begin
   if Message.Msg = Mom_Done then
      begin
      { Find the MIDIHDR we used for the output. Message.lParam is its address }
      thisHdr := PMidiHdr(Message.lParam);

      { Remove it from the output device }
      midiOutUnprepareHeader(FMidiHandle, thisHdr, sizeof(TMIDIHDR));

      { Get the address of the MyMidiHdr object containing this MIDIHDR structure.
         We stored this address in the PutLong procedure }
      MyMidiHdr := TMyMidiHdr(thisHdr^.dwUser);

      { Header and copy of sysex data no longer required since output is complete }
      MyMidiHdr.Free;

      { Call the user's event handler if any }
      if Assigned(FOnmidioutput) then
         FOnmidioutput(Self);
      end;
// there is no case for MOM_PLAYBACK_DONE
end; // midioutput //

end. // Unit: MIDI_IO_Output //
