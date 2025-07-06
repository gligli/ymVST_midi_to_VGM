unit MSC_Port;

interface

type
   TPort_Capability     = (pcDisabled, pcMute, pcPlay);
   TTrack_Array         = packed array [TTRack_Range] of TPort_Capability;

   TMIDI_Port = class (TObject)
// This class provides a wrapper for the MIDI devices for input and output.
   private
      FMIDI_Port: TMIDI_Device_Output;
      FPort_Capability: TTrack_Array;

      function  get_port_capability  (track: TTrack_Range): TPort_Capability;
      procedure set_port_capability (track: TTrack_Range; cap: TPort_Capability);

   public
      constructor Create (index: integer);
      destructor Destroy; override;

	property DriverVersion: Version    // Driver version from midioutGetDevCaps
				read FDriverVersion;
	property Technology: OutPortTech   // Type of MIDI output device
		read FTechnology
                write SetTechnology
                default opt_Synth;
	property Voices: Word			     // Number of voices (internal synth)
				read FVoices;
	property Notes: Word			        // Number of notes (internal synth)
				read FNotes;
	property ChannelMask: Word			  //Bit set for each MIDI channels that the
				read FChannelMask;        // device responds to (internal synth)
	property Support: DWORD			     // Technology supported (volume control,
				read FSupport;            // patch caching etc.)
	property Error: Word read FError;
	property Numdevs: Word read FNumdevs;

      property MIDI_Out: TMIDI_Device_Output read FMIDI_Port;
      property Port_Capability [index: TTrack_Range]: TPort_Capability
         read  get_port_capability
         write set_port_capability;
   end; // TMIDI_Port //

implementation

{*******************************************************************
*                                                                  *
* Class TMIDI_Port                                                 *
*                                                                  *
********************************************************************}

constructor TMIDI_Port.Create (index: integer);
var track: TTRack_Range;
begin
   inherited Create;

   try
// Test whether the MIDI port can be created
      FMIDI_Port := TMIDI_Device_Output.Create (nil);
      FMIDI_Port.DeviceID := index;
      try
// Try to open the MIDI port.
         FMIDI_Port.Open;
         for track := Low (TTrack_Range) to High (TTrack_Range) do
            Port_Capability[track] := pcPlay;
      except
// MIDI port could not be opened. It is flagged as pcDisabled.
         for track := Low (TTrack_Range) to High (TTrack_Range) do
            Port_Capability[track] := pcDisabled;
      end; // try
   except
// Something went wrong when opening the MIDI port.
// Set it at nil. This means that it is pcDisabled.
      FMIDI_Port := nil;
   end; // try
end; // Create //

destructor TMIDI_Port.Destroy;
begin
   if MIDI_Out <> nil then
   begin
      if MIDI_Out.Status = mosOpen then MIDI_Out.Close;
      FMIDI_Port.Free;
   end; // if

   inherited Destroy;
end; // Destroy //

function TMIDI_Port.get_port_capability
(
   track: TTrack_Range // Track of port which capability has to be returned
): TPort_Capability;
begin
   if MIDI_Out = nil
      then get_port_capability := pcDisabled
      else if MIDI_Out.Status = mosOpen
         then get_port_capability := FPort_Capability [track]
         else get_port_capability := pcDisabled;
end; // get_port_capability //

procedure TMIDI_Port.set_port_capability
(
   track: TTrack_Range;   // Track of port to be set
   cap: TPort_Capability  // to this capability
);
// Set the desired port capability. It can only be set if a MIDI output device
// has been assigned and opened. Else the port capability will be set to disabled.
begin
   if (MIDI_Out <> nil) and (MIDI_Out.Status = mosOpen)
      then FPort_Capability [track] := cap
      else FPort_Capability [track] := pcDisabled;
end; // set_port_capability //

end. // Unit: MSC_Port //
