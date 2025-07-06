{ $Header: /MidiComp/MSC_In_Device.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

unit MSC_In_Device;

{
  Properties:
   DeviceID: 	Windows numeric device ID for the MIDI input device.
   Between 0 and NumDevs-1.
   Read-only while device is open, exception when changed while open

   MIDIHandle:	The input handle to the MIDI device.
   0 when device is not open
   Read-only, runtime-only

   MessageCount:	Number of input messages waiting in input buffer

   Capacity:	Number of messages input buffer can hold
   Defaults to 1024
   Limited to (64K/event size)
   Read-only when device is open (exception when changed while open)

   SysexBufferSize:	Size in bytes of each sysex buffer
   Defaults to 10K
   Minimum 0K (no buffers), Maximum 64K-1

   SysexBufferCount:	Number of sysex buffers
   Defaults to 16
   Minimum 0 (no buffers), Maximum (avail mem/SysexBufferSize)
   Check where these buffers are allocated?

   SysexOnly: True to ignore all non-sysex input events. May be changed while
   device is open. Handy for patch editors where you have lots of short MIDI
   events on the wire which you are always going to ignore anyway.

   DriverVersion: Version number of MIDI device driver. High-order uInt16 is
   major version, low-order byte is minor version.

   ProductName: Name of product (e.g. 'MPU 401 In')

   MID and PID: Manufacturer ID and Product ID, see
   "Manufacturer and Product IDs" in MMSYSTEM.HLP for list of possible values.

   Methods:
   GetMidiEvent: Read Midi event at the head of the FIFO input buffer.
   Returns a TMyMidiEvent object containing MIDI message data, timestamp,
   and sysex data if applicable.
   This method automatically removes the event from the input buffer.
   It makes a copy of the received sysex buffer and puts the buffer back
   on the input device.
   The TMyMidiEvent object must be freed by calling MyMidiEvent.Free.

   Open: Opens device. Note no input will appear until you call the Start
   method.

   Close: Closes device. Any pending system exclusive output will be cancelled.

   Start: Starts receiving MIDI input.

   Stop: Stops receiving MIDI input.

   Events:
   OnMidiInput: Called when MIDI input data arrives. Use the GetMidiEvent to
   get the MIDI input data.

   OnOverflow: Called if the MIDI input buffer overflows. The caller must
   clear the buffer before any more MIDI input can be received.

   Notes:
   Buffering: Uses a circular buffer, separate pointers for next location
   to fill and next location to empty because a MIDI input interrupt may
   be adding data to the buffer while the buffer is being read. Buffer
   pointers wrap around from end to start of buffer automatically. If
   buffer overflows then the OnBufferOverflow event is triggered and no
   further input will be received until the buffer is emptied by calls
   to GetMidiEvent.

   Sysex buffers: There are (SysexBufferCount) buffers on the input device.
   When sysex events arrive these buffers are removed from the input device and
   added to the circular buffer by the interrupt handler in the DLL.  When the sysex events
   are removed from the circular buffer by the GetMidiEvent method the buffers are
   put back on the input. If all the buffers are used up there will be no
   more sysex input until at least one sysex event is removed from the input buffer.
   In other	words if you're expecting lots of sysex input you need to set the
   SysexBufferCount property high enough so that you won't run out of
   input buffers before you get a chance to read them with GetMidiEvent.

   If the synth sends a block of sysex that's longer than SysexBufferSize it
   will be received as separate events.
   TODO: Component derived from this one that handles >64K sysex blocks cleanly
   and can stream them to disk.

   Midi Time Code (MTC) and Active Sensing: The DLL is currently hardcoded
   to filter these short events out, so that we don't spend all our time
   processing them.
   TODO: implement a filter property to select the events that will be filtered
   out.
}

interface

uses Classes, SysUtils, WinTypes, Messages, WinProcs, MMSystem,
     MSC_Definitions, MSC_Device, MSC_Circular_Buffer, MSC_Delphi_MIDI_Callback;

type
   MidiInputState = (misOpen, misClosed, misCreating, misDestroying);
   EMSC_Device_In_Error = class (EMSC_Device_Error);

   {-------------------------------------------------------------------}
   TMIDI_Device_Input = class (TMIDI_Device)
   private
      FMIDIHandle: HMIDIIn;   // Handle to input device }
      FState: MidiInputState; // Current device state }
      FSysexOnly: Boolean;

// Queue
      FCapacity: uInt16; // Buffer capacity
      PBuffer: PCircularBuffer; // Low-level MIDI input buffer created by Open method

// Events
      FOnMIDIInput: TNotifyEvent;   // MIDI Input arrived
      FOnOverflow: TNotifyEvent;    // Input buffer overflow
// TODO: Some sort of error handling event for MIM_ERROR

// Sysex
      FSysexBufferSize:  uInt16;
      FSysexBufferCount: uInt16;
      MidiHdrs: Tlist;
      PCtlInfo: PMidiCtlInfo; // Pointer to control info for DLL

   protected
      procedure Prepareheaders;
      procedure UnprepareHeaders;
      procedure AddBuffers;
      procedure SetDeviceID (DeviceID: Int32); override;
      procedure SetProductName (NewProductName: String); override;
      function  GetEventCount: uInt16;
      procedure SetSysexBufferSize (BufferSize: uInt16);
      procedure SetSysexBufferCount (BufferCount: uInt16);
      procedure SetSysexOnly (bSysexOnly: Boolean);
      function  MidiInErrorString (WError: uInt16): String;

  public
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      function Open: boolean; override;
      function Close: boolean; override;
      function is_opened: boolean; override;
      function is_closed: boolean; override;

      property MIDIHandle: HMIDIIn read FMIDIHandle;
      property MessageCount: uInt16 read GetEventCount;

      procedure Start;
      procedure Stop;
      function Status: MidiInputState;
      function GetMidiEvent: TMIDI_Event; // Get first message in input queue
      procedure MidiInput (var Message: TMessage);

// Some functions to decode and classify incoming messages would be good

   published
      property Capacity: uInt16 read FCapacity write FCapacity default 1024;
      property SysexBufferSize: uInt16 read FSysexBufferSize write SetSysexBufferSize default 10000;
      property SysexBufferCount: uInt16 read FSysexBufferCount write SetSysexBufferCount default 16;
      property SysexOnly: Boolean read FSysexOnly write SetSysexOnly default False;

// Events
      property OnMidiInput: TNotifyEvent read FOnMidiInput write FOnMidiInput;
      property OnOverflow: TNotifyEvent read FOnOverflow write FOnOverflow;
end; // Class: TMIDI_Device_Input //

procedure Register;

{====================================================================}
implementation

Uses  Graphics, Controls, Forms, Dialogs;

{-------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents(MSC_Package_Name, [TMIDI_Device_Input]);
end;

{-------------------------------------------------------------------}
constructor TMIDI_Device_Input.Create (AOwner:TComponent);
begin
   inherited Create(AOwner);

   FState     := misCreating;
   FSysexOnly := False;
   FNumDevs   := midiInGetNumDevs;
   MidiHdrs   := Nil;

   { Set defaults }
   FCapacity := 1024;
   FSysexBufferSize := 4096;
   FSysexBufferCount := 16;

   { Create the window for callback notification }
   if not (csDesigning in ComponentState) then
   begin
      Handle := AllocateHwnd (MidiInput);
   end;

   if FNumDevs > 0 then SetDeviceID (0);
   FState := misClosed;
end; // Create //

{-------------------------------------------------------------------}
{ Close the device if it's open }
destructor TMIDI_Device_Input.Destroy;
begin
	if (FMidiHandle <> 0) then
	begin
		Close;
		FMidiHandle := 0;
	end;

	if (PCtlInfo <> Nil) then
		GlobalSharedLockedFree( PCtlinfo^.hMem, PCtlInfo );

	DeallocateHwnd(Handle);
	inherited Destroy;
end; // Destroy //

function TMIDI_Device_Input.Status: MidiInputState;
begin
   Result := FState;
end; // Status //

{-------------------------------------------------------------------}
{ Convert the numeric return code from an MMSYSTEM function to a string
  using midiInGetErrorText. TODO: These errors aren't very helpful
  (e.g. "an invalid parameter was passed to a system function") so
  sort out some proper error strings. }
function TMIDI_Device_Input.MidiInErrorString (WError: uInt16): String;
var errorDesc: PChar;
begin
   errorDesc := Nil;
   try
      errorDesc := StrAlloc (MAXERRORLENGTH);
      if midiInGetErrorText (WError, errorDesc, MAXERRORLENGTH) = 0
         then result := StrPas (errorDesc)
         else result := 'Specified error number is out of range';
   finally
      if errorDesc <> Nil then StrDispose(errorDesc);
   end;
end; // MidiInErrorString //

{-------------------------------------------------------------------}
{ Set the sysex buffer size, fail if device is already open }
procedure TMIDI_Device_Input.SetSysexBufferSize (BufferSize: uInt16);
begin
   if FState = misOpen
      then raise EMSC_Device_In_Error.Create ('Change to SysexBufferSize while device was open')
      else FSysexBufferSize := BufferSize;
end; // SetSysexBufferSize //

{-------------------------------------------------------------------}
{ Set the sysex buffer count, fail if device is already open }
procedure TMIDI_Device_Input.SetSysexBuffercount (Buffercount: uInt16);
begin
   if FState = misOpen
      then raise EMSC_Device_In_Error.Create('Change to SysexBuffercount while device was open')
      else FSysexBuffercount := Buffercount;{ TODO: Validate the sysex buffer count}
end; // SetSysexBuffercount //

{-------------------------------------------------------------------}
{ Set the Sysex Only flag to eliminate unwanted short MIDI input messages }
procedure TMIDI_Device_Input.SetSysexOnly (bSysexOnly: Boolean);
begin
   FSysexOnly := bSysexOnly;
   { Update the interrupt handler's copy of this property }
   if PCtlInfo <> nil
      then PCtlInfo^.SysexOnly := bSysexOnly;
end; // SetSysexOnly //

{-------------------------------------------------------------------}
{ Set the Device ID to select a new MIDI input device
  Note: If no MIDI devices are installed, throws an 'Invalid Device ID' exception }
procedure TMIDI_Device_Input.SetDeviceID (DeviceID: Int32);
var MidiInCaps: TMidiInCaps;
begin
   if FState = misOpen then
   begin
      raise EMSC_Device_In_Error.Create('Change to DeviceID while device was open')
   end else if (DeviceID < 0) or (DeviceID >= Int32 (midiInGetNumDevs)) then
   begin
      raise EMSC_Device_In_Error.Create ('Invalid device ID')
   end else
   begin
      FDeviceID := DeviceID;

      { Set the name and other MIDIINCAPS properties to match the ID }
      FError := midiInGetDevCaps (DeviceID, @MidiInCaps, sizeof (TMidiInCaps));
      if Ferror <> MMSYSERR_NOERROR
         then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));

      FProductName := string (StrPas(MidiInCaps.szPname));
      FDriverVersion := MidiInCaps.vDriverVersion;
      FMID := MidiInCaps.wMID;
      FPID := MidiInCaps.wPID;
   end; // if
end; // SetDeviceID //

{-------------------------------------------------------------------}
{ Set the product name and put the matching input device number in FDeviceID.
  This is handy if you want to save a configured input/output device
  by device name instead of device number, because device numbers may
  change if users add or remove MIDI devices.
  Exception if input device with matching name not found,
  or if input device is open }
procedure TMIDI_Device_Input.SetProductName (NewProductName: String);
var MidiInCaps: TMidiInCaps;
    testDeviceID: uInt16;
    testProductName: String;
begin
   if FState = misOpen
      then raise EMSC_Device_In_Error.Create('Change to ProductName while device was open')
      else
   { Don't set the name if the component is reading properties because
   the saved Productname will be from the machine the application was compiled
   on, which may not be the same for the corresponding DeviceID on the user's
   machine. The FProductname property will still be set by SetDeviceID }
   if not (csLoading in ComponentState) then
   begin
      for testDeviceID := 0 To (midiInGetNumDevs-1) do
      begin
         FError := midiInGetDevCaps(testDeviceID, @MidiInCaps, sizeof(TMidiInCaps));
         if Ferror <> MMSYSERR_NOERROR
            then raise EMSC_Device_In_Error.Create(MidiInErrorString(FError));
         testProductName := string (StrPas (MidiInCaps.szPname));
         if testProductName = NewProductName then
         begin
            FProductName := NewProductName;
            Break;
         end; // if
      end; // for
      if FProductName <> NewProductName
         then raise EMSC_Device_In_Error.Create('MIDI Input Device ' +
                              NewProductName + ' not installed ')
         else SetDeviceID(testDeviceID);
   end; // if
end; // SetProductName //


{-------------------------------------------------------------------}
{ Get the sysex buffers ready }
procedure TMIDI_Device_Input.PrepareHeaders;
var
   ctr: uInt16;
   MyMidiHdr: TMyMidiHdr;
begin
   if (FSysexBufferCount > 0) And (FSysexBufferSize > 0)
      And (FMidiHandle <> 0) then
      begin
      Midihdrs := TList.Create;
      for ctr := 1 to FSysexBufferCount do
         begin
         { Initialize the header and allocate buffer memory }
         MyMidiHdr := TMyMidiHdr.Create(FSysexBufferSize);

         { Store the address of the MyMidiHdr object in the contained MIDIHDR
              structure so we can get back to the object when a pointer to the
              MIDIHDR is received.
              E.g. see TMidiOutput.Output method }
         MyMidiHdr.hdrPointer^.dwUser := uInt32(MyMidiHdr);

         { Get MMSYSTEM's blessing for this header }
         FError := midiInPrepareHeader(FMidiHandle,MyMidiHdr.hdrPointer,
            sizeof(TMIDIHDR));
         if Ferror <> MMSYSERR_NOERROR then
            raise EMSC_Device_In_Error.Create(MidiInErrorString(FError));

         { Save it in our list }
         MidiHdrs.Add(MyMidiHdr);
         end;
      end;
end;

{-------------------------------------------------------------------}
{ Clean up from PrepareHeaders }
procedure TMIDI_Device_Input.UnprepareHeaders;
var ctr: uInt16;
begin
   if (MidiHdrs <> Nil) then	{ will be Nil if 0 sysex buffers }
   begin
      for ctr := 0 To MidiHdrs.Count-1 do
      begin
         FError := midiInUnprepareHeader (FMidiHandle,
            TMyMidiHdr(MidiHdrs.Items [ctr]).hdrPointer, sizeof (TMIDIHDR));
         if Ferror <> MMSYSERR_NOERROR
            then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));
         TMyMidiHdr(MidiHdrs.Items [ctr]).Free;
      end; // for
      MidiHdrs.Free;
      MidiHdrs := Nil;
   end; // if
end; // UnprepareHeaders //

{-------------------------------------------------------------------}
{ Add sysex buffers, if required, to input device }
procedure TMIDI_Device_Input.AddBuffers;
var ctr: uInt16;
begin
   if MidiHdrs <> Nil then { will be Nil if 0 sysex buffers }
   begin
      if MidiHdrs.Count > 0 Then
      begin
         for ctr := 0 To MidiHdrs.Count-1 do
         begin
            FError := midiInAddBuffer(FMidiHandle,
                      TMyMidiHdr(MidiHdrs.Items[ctr]).hdrPointer, sizeof(TMIDIHDR));
            If FError <> MMSYSERR_NOERROR then
               raise EMSC_Device_In_Error.Create(MidiInErrorString(FError));
         end; // for
      end; // if
   end; // if
end; // AddBuffers //

{-------------------------------------------------------------------}
function TMIDI_Device_Input.Open: boolean;
var hMem: THandle;
begin
   Result := True;
   try
      { Create the buffer for the MIDI input messages }
      If (PBuffer = Nil)
         then PBuffer := CircBufAlloc (FCapacity);

      { Create the control info for the DLL }
      if (PCtlInfo = Nil) then
      begin
         PCtlInfo := GlobalSharedLockedAlloc( Sizeof(TMidiCtlInfo), hMem);
         PctlInfo^.hMem := hMem;
      end;
      PctlInfo^.pBuffer := PBuffer;
      Pctlinfo^.hWindow := Handle; //  Control's window handle
      PCtlInfo^.SysexOnly := FSysexOnly;
      FError := midiInOpen(@FMidiHandle, FDeviceId, uInt32 (@midiHandler),
                                         uInt32 (PCtlInfo), CALLBACK_FUNCTION);

      If (FError <> MMSYSERR_NOERROR)
         then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));

      PrepareHeaders;      // Get sysex buffers ready
      AddBuffers;          // Add them to the input
      FState := misOpen;   // device is opened

   except
      Result := False;
      if PBuffer <> Nil then
      begin
         CircBufFree(PBuffer);
         PBuffer := Nil;
      end;

      if PCtlInfo <> Nil then
      begin
         GlobalSharedLockedFree(PCtlInfo^.hMem, PCtlInfo);
         PCtlInfo := Nil;
      end; // if
   end; // try
end; // Open //

{-------------------------------------------------------------------}
function TMIDI_Device_Input.Close: boolean;
begin
   Result := True;
   if FState = misOpen then
   begin
      FState := misClosed;

      { MidiInReset cancels any pending output.
      Note that midiInReset causes an MIM_LONGDATA callback for each sysex
      buffer on the input, so the callback function and Midi input buffer
      should still be viable at this stage.
      All the resulting MIM_LONGDATA callbacks will be completed by the time
      MidiInReset returns, though. }

      FError := MidiInReset(FMidiHandle);
      if Ferror <> MMSYSERR_NOERROR then
      begin
         raise EMSC_Device_In_Error.Create(MidiInErrorString(FError));
      end; // if

      { Remove sysex buffers from input device and free them }
      UnPrepareHeaders;

      { Close the device (finally!) }
      FError := MidiInClose(FMidiHandle);
      if Ferror <> MMSYSERR_NOERROR then
      begin
         raise EMSC_Device_In_Error.Create(MidiInErrorString(FError));
      end; // if

      FMidiHandle := 0;

      If (PBuffer <> Nil) then
      begin
         CircBufFree( PBuffer );
         PBuffer := Nil;
      end; // if
   end; // if
end; // Close //

function TMIDI_Device_Input.is_opened: boolean;
begin
   Result := Status = misOpen;
end; // is_opened //

function TMIDI_Device_Input.is_closed: boolean;
begin
   Result := Status = misClosed;
end; // is_closed //

{-------------------------------------------------------------------}
function TMIDI_Device_Input.GetMidiEvent: TMIDI_Event;
var thisItem: TMidiBufferItem;
begin
   if (FState = misOpen) and CircBufReadEvent (PBuffer, @thisItem) then
   begin
      Result := TMIDI_Event.Create;
      with thisItem Do
      begin
         Result.Time := Timestamp;
         if (Sysex = Nil) then // Short Message
         begin
            Result.MIDI_Message := LoByte(LoWord(Data));
            Result.Data_Byte_1 := HiByte(LoWord(Data));
            Result.Data_Byte_2 := LoByte(HiWord(Data));
            Result.Sysex := Nil;
            Result.SysexLength := 0;
         end else // Long Sysex message
         begin
            Result.MIDI_Message := mc_MIDI_Begin_SysEx;
            Result.Data_Byte_1 := 0;
            Result.Data_Byte_2 := 0;
            Result.SysexLength := Sysex^.dwBytesRecorded;
            if Sysex^.dwBytesRecorded <> 0 then
            begin
               { Put a copy of the sysex buffer in the object }
               GetMem (Result.Sysex, Sysex^.dwBytesRecorded);
               Move (Result.Sysex, Sysex^.lpData, Sysex^.dwBytesRecorded);
            end; // if

            { Put the header back on the input buffer }
            FError := midiInPrepareHeader (FMidiHandle,Sysex, sizeof (TMIDIHDR));
            If Ferror = 0
               then FError := midiInAddBuffer (FMidiHandle, Sysex, sizeof (TMIDIHDR));
            if Ferror <> MMSYSERR_NOERROR
               then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));
         end; // if
      end; // with
      CircbufRemoveEvent(PBuffer);
   end else Result := nil; // Device isn't open, return a nil event
end; // GetMidiEvent //

{-------------------------------------------------------------------}
function TMIDI_Device_Input.GetEventCount: uInt16;
begin
   if FState = misOpen
      then Result := PBuffer^.EventCount
      else Result := 0;
end; // GetEventCount //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Input.Start;
begin
   if FState = misOpen then
   begin
      FError := MidiInStart (FMidiHandle);
      if Ferror <> MMSYSERR_NOERROR
         then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));
   end; // if
end; // Start //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Input.Stop;
begin
   if FState = misOpen then
   begin
      FError := MidiInStop (FMidiHandle);
      if Ferror <> MMSYSERR_NOERROR
         then raise EMSC_Device_In_Error.Create (MidiInErrorString (FError));
   end; // if
end; // Stop //

{-------------------------------------------------------------------}
procedure TMIDI_Device_Input.MidiInput (var Message: TMessage);
{ Triggered by incoming message from DLL.
  Note DLL has already put the message in the queue }
begin
   case Message.Msg of
   mim_data:
      { Trigger the user's MIDI input event, if they've specified one and
      we're not in the process of closing the device. The check for
      GetEventCount > 0 prevents unnecessary event calls where the user has
      already cleared all the events from the input buffer using a GetMidiEvent
      loop in the OnMidiInput event handler }
      if Assigned(FOnMIDIInput) and (FState = misOpen)
         and (GetEventCount > 0) then
         FOnMIDIInput(Self);

   mim_Overflow: { input circular buffer overflow }
      if Assigned (FOnOverflow) and (FState = misOpen) then
         FOnOverflow (Self);
   end;
end; // MidiInput //

end. // Unit: MIDI_Device_In //
