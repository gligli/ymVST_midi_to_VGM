unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  MSC_Definitions, MSC_Out_Device;

type
  TExample = class(TForm)
      SoundCards: TComboBox;
      Label1: TLabel;
      Label2: TLabel;
      Label3: TLabel;
      Label4: TLabel;
      Label_Productname: TLabel;
      Label_Technology: TLabel;
      Label_Voices: TLabel;
      Label_Notes: TLabel;
      Timer1: TTimer;
      Play_Button: TButton;
      Label5: TLabel;
      MIDI_Channels: TComboBox;
      Label6: TLabel;
      MIDI_Patches: TComboBox;
      Label7: TLabel;
      Track_Volume: TTrackBar;
      MIDI_Notes: TComboBox;
      Label8: TLabel;
      Label9: TLabel;

      procedure SoundCardsChange(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure Timer1Timer(Sender: TObject);
      procedure Play_ButtonClick(Sender: TObject);
      procedure MIDI_ChannelsChange(Sender: TObject);
      procedure MIDI_PatchesChange(Sender: TObject);
      procedure Track_VolumeChange(Sender: TObject);
      procedure MIDI_NotesChange(Sender: TObject);

   private
      FMIDI_Output: TMIDI_Device_Output;
      FPlaying: boolean; // Playing or not?
      FChannel: byte;    // Current MIDI channel
      FPatch: byte;      // Current patch
      FNote: byte;       // Selected note
      FSpeed: byte;      // speed of note

   public
      procedure find_soundcard;
   end; // Class: TExample //

var
  Example: TExample;

implementation

{$R *.DFM}

// Form event handlers

procedure TExample.FormCreate(Sender: TObject);
var i: byte;
begin
   find_soundcard;          // Chack soundcards present
   SoundCardsChange (Self); // Show parameters of current one
   FPlaying := false;
   Timer1.Enabled := false;

// Fill selection boxes with their respective values
   for i := 1 to 16  do MIDI_Channels.Items.Add (Format ('%d', [i]));
   for i := 0 to 127 do MIDI_Patches .Items.Add (Format ('%d', [i]));
   for i := 0 to 127 do MIDI_Notes   .Items.Add (Format ('%d', [i]));

// And initialize them
   MIDI_Channels.ItemIndex := 0;
   MIDI_Patches .ItemIndex := 0;
   MIDI_Notes   .ItemIndex := 60;
end; // FormCreate //

procedure TExample.find_soundcard;

// Routine to check each soundcard present and put their name in the
// SoundCards combobox. The number of soundcards present can be derived
// from the NumDevs property

var device: smallint;
begin
   FMIDI_Output := TMIDI_Device_Output.Create (Self);
   Soundcards.Items.Clear;
   for device := 0 To FMIDI_Output.NumDevs - 1 do
   begin
      FMIDI_Output.DeviceID := device;
      Soundcards.Items.Add (FMIDI_Output.ProductName);
   end; // for
   Soundcards.ItemIndex := 0;
end; // update_soundcard //
{
   Routine to start playing when not and to stop playing when playing.
   Start playing: select parameters, open MIDI device and send patch.
   Then enable timer.
   Stop playing: send all MIDI notes off (emergency case), and close MIDI
   device.
   The exception handles each exception and stops playing.
}
procedure TExample.Play_ButtonClick(Sender: TObject);
var i: byte;
begin
   try
      if not FPlaying then // If not playing start play
      begin
         FPlaying := true;
         FMIDI_Output.DeviceID := SoundCards.ItemIndex; // Select MIDI device
         FChannel := MIDI_Channels.ItemIndex;           // Select channel
         FPatch   := MIDI_Patches .ItemIndex;           // Select patch (program)
         FNote    := MIDI_Notes   .ItemIndex;           // Select note to be played
         FSpeed   := Track_volume .Position;            // Select volume
         FMIDI_Output.Open;                             // Open MIDI device
         FMIDI_Output.PutShort (mc_MIDI_Program_Change + FChannel, FPatch, 0); // Send patch
         Play_Button.Caption := 'Sto&p';
         Timer1.Enabled := true;                        // Start timer
      end else // Stop playing
      begin
         Timer1.Enabled := false; // Stop timer immediately
         FPlaying := false;

// Send an all notes off for each channel
         for i := 0 to 15
             do FMIDI_Output.PutShort (175 + i, mc_Ctrl_all_notes_off, 0);

         FMIDI_Output.Close; // Close MIDI device
         Play_Button.Caption := '&Play';
      end; // if
   except
      on e:exception do
      begin
         Timer1.Enabled := false;

// All notes off for each channel
         for i := 0 to 15
             do FMIDI_Output.PutShort (175 + i, mc_Ctrl_all_notes_off, 0);
         FPlaying := false;
         showmessage ('Error ' + e.classname + ' encountered. ' +
                      'Sorry, the music cannot be played. The error was: ' +
                      e.message);
      end; // on
   end; // try
end; // Play_ButtonClick //

{
   Sends a putshort at each interval. I have used the normal timer for clarity.
   This one is very sensitive for other windows events (try starting another
   program when playing), so use the multimedia timer. The one of Dion Kurczek
   is a good one. When errors are encountered the timer is disabled.
}
procedure TExample.Timer1Timer(Sender: TObject);
begin
   try
      FMIDI_Output.PutShort (mc_MIDI_Note_On + FChannel, FNote, FSpeed);
   except
      on e:exception do
      begin
         Timer1.Enabled := False;
         showmessage ('Error ' + e.classname + ' encountered. ' +
                      'Sorry, the music cannot be played. The error was: ' +
                      e.message);
      end; // on
   end; // try
end; // Timer1Timer //

// Combobox change handlers.

// Select soundcard and show some of its capabilities.
// When playing, close device, select device and then open it again.
procedure TExample.SoundCardsChange (Sender: TObject);
var pos: smallint;
begin
   if FPlaying then
   begin
      FMIDI_Output.Close;
   end; // if
   pos := Soundcards.ItemIndex;
   FMIDI_Output.DeviceID := pos;
   if FPlaying then
   begin
      FMIDI_Output.Open;
   end; // if
   Label_Productname.Caption := FMIDI_Output.Productname;
   case FMIDI_Output.Technology of
      opt_None:     Label_Technology.Caption := 'None';
      opt_MidiPort: Label_Technology.Caption := 'MIDI Output port';
      opt_Synth:    Label_Technology.Caption := 'Internal Synthesizer';
      opt_SQSynth:  Label_Technology.Caption := 'SQ Synthesizer';
      opt_FMSynth:  Label_Technology.Caption := 'FM Synthesizer';
      opt_Mapper:   Label_Technology.Caption := 'Windows MIDI Mapper';
   end; // case
   Label_Voices.Caption := Format ('%d', [FMIDI_Output.Voices]);
   Label_Notes .Caption := Format ('%d', [FMIDI_Output.Notes]);
end; // SoundCardsChange //

// Selects MIDI channel.
procedure TExample.MIDI_ChannelsChange(Sender: TObject);
begin
   FChannel := MIDI_Channels.ItemIndex;
end; // MIDI_ChannelsChange //

// Change patch. If playing send it immediately to the MIDI output device.
procedure TExample.MIDI_PatchesChange(Sender: TObject);
begin
   FPatch := MIDI_Patches.ItemIndex;
   if FPlaying then FMIDI_Output.PutShort (mc_MIDI_Program_Change + FChannel, FPatch, 0);
end; // MIDI_PatchesChange //

// Change volume.
procedure TExample.Track_VolumeChange(Sender: TObject);
begin
   FSpeed := Track_Volume.Position;
end; // Track_VolumeChange //

// Change the note to be played.
procedure TExample.MIDI_NotesChange(Sender: TObject);
begin
   FNote := MIDI_Notes.ItemIndex;
end; // MIDI_NotesChange //

end. // Unit: Main //
