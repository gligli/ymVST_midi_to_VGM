unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst, Spin,
  MSC_Definitions, MSC_Container, ymexport, ymsynth;

type

  { TTrack }

  TTrack = class
    Index: Integer;
    NoteCount: Integer;
    Name: String;
    PatchFileName: String;
    Export: Boolean;
  end;


 { TFormMain }

 TFormMain = class(TForm)
   btConvert: TButton;
   btDn: TButton;
   btInputBrowse: TButton;
   btOutputBrowse: TButton;
   btLoad: TButton;
   btUp: TButton;
   cbExport: TComboBox;
   chkAutomations: TCheckBox;
   edAuthor: TEdit;
   edSongName: TEdit;
   lbTracks: TCheckListBox;
   edInputMid: TEdit;
   edOutput: TEdit;
   llAuthor: TLabel;
   llBPM: TLabel;
   llFormat: TLabel;
   llHint: TLabel;
   llSongName: TLabel;
   llTime: TLabel;
   seBPM: TSpinEdit;
   seLength: TFloatSpinEdit;
   procedure btConvertClick(Sender: TObject);
   procedure btDnClick(Sender: TObject);
   procedure btInputBrowseClick(Sender: TObject);
   procedure btLoadClick(Sender: TObject);
   procedure btOutputBrowseClick(Sender: TObject);
   procedure btUpClick(Sender: TObject);
   procedure cbExportChange(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure FormDestroy(Sender: TObject);
   procedure lbTracksClickCheck(Sender: TObject);
   procedure lbTracksDblClick(Sender: TObject);
   procedure lbTracksSelectionChange(Sender: TObject; User: boolean);
 private
   FMIDIContainer: TMIDI_Container;

   procedure UpdateGUI;
 public

 end;

var
 FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FMIDIContainer := TMIDI_Container.Create;

{$IFNDEF DEBUG}
  edInputMid.Text := '';
  edAuthor.Text := '';
  edSongName.Text := '';
  edOutput.Text := '';
{$ENDIF}

  UpdateGUI;
end;

procedure TFormMain.btLoadClick(Sender: TObject);
var
  i, iTrack, iEvent: Integer;
  tmpTrackName: String;
  trackNameDone: Boolean;
  e: TMIDI_Event;
  t: TTrack;
begin
  for i := 0 to lbTracks.Count - 1 do
    lbTracks.Items.Objects[i].Free;
  lbTracks.Clear;

  FMIDIContainer.LoadFromFile(edInputMid.Text, nil);
  seBPM.Value := FMIDIContainer.BPM;
  seLength.Value := FMIDIContainer.time_to_seconds(FMIDIContainer.set_max_time_to_container);

  for iTrack := low(TMIDI_Range) to high(TMIDI_Range) do
  begin
    if FMIDIContainer.Voice_Count[iTrack] > 0 then
    begin
      tmpTrackName := '';
      trackNameDone := False;

      for iEvent := 0 to FMIDIContainer.Count - 1 do
      begin
        e := FMIDIContainer.Event[iEvent];

        if not trackNameDone and (e.Data_Byte_1 = mc_Meta_Track_Name) then
        begin
          tmpTrackName := TMeta_Event(e).Text;
        end
        else if (e.Channel = iTrack) and (tmpTrackName <> '') then
        begin
          FMIDIContainer.Track_Name[iTrack] := tmpTrackName;
          tmpTrackName := '';
          trackNameDone := True;
        end;
      end;

      t := TTrack.Create;
      t.Index := iTrack;
      t.NoteCount := FMIDIContainer.Voice_Count[iTrack];
      t.Name := FMIDIContainer.Track_Name[iTrack];
      t.PatchFileName := ChangeFileExt(t.Name,'.fxp');

      t.Export := True;

      lbTracks.AddItem('dummy', t);
    end;
  end;

  UpdateGUI;
end;

procedure TFormMain.btOutputBrowseClick(Sender: TObject);
var
  fn: String;
begin
  fn := edOutput.Text;
  if PromptForFileName(fn, 'VGM / SNDH|*.vgm;*.snd;*.sndh', '', '', '', True) then
  begin
    edOutput.Text := fn;
    UpdateGUI;
  end;
end;

procedure TFormMain.btUpClick(Sender: TObject);
begin
  lbTracks.Exchange(lbTracks.ItemIndex, lbTracks.ItemIndex - 1);
  lbTracks.ItemIndex := lbTracks.ItemIndex - 1;
  UpdateGUI;
end;

procedure TFormMain.cbExportChange(Sender: TObject);
begin
  if edOutput.Text <> '' then
    edOutput.Text := ChangeFileExt(edOutput.Text, '.' + LowerCase(cbExport.Text));
  UpdateGUI;
end;

procedure TFormMain.btInputBrowseClick(Sender: TObject);
var
  fn: String;
begin
  fn := edInputMid.Text;
  if PromptForFileName(fn, 'MIDI|*.mid;*.midi') then
  begin
    edInputMid.Text := fn;
    edOutput.Text := ChangeFileExt(fn, '.' + LowerCase(cbExport.Text));
    UpdateGUI;
  end;
end;

procedure TFormMain.btConvertClick(Sender: TObject);
var
  iTrack, iNote, iEvent: Integer;
  YMData: TYMData;
  trk: TTrack;
  p: TYMPatch;
  vv: TYMVirtualVoice;
  yms: TYMSynth;
  yme: TYMBaseExporter;
  e: TMIDI_Event;
  n, d, s, t: TReal_Array;
begin
  yms := TYMSynth.Create(seLength.Value);
  try
    yms.EnableAutomations := chkAutomations.Checked;

    for iTrack := 0 to lbTracks.Count - 1 do
    begin
      trk := TTrack(lbTracks.Items.Objects[iTrack]);

      if trk.Export and FileExists(trk.PatchFileName) then
      begin
        p := TYMPatch.Create;
        yms.Patches.Add(p);

        p.LoadFromCubaseFXP(trk.PatchFileName);

        vv := TYMVirtualVoice.Create(yms, p);
        yms.VirtualVoices.Add(vv);

        FMIDIContainer.get_vectors(trk.Index, n, d, s, t);

        for iNote := 0 to High(n) do
        begin
          d[iNote] := FMIDIContainer.time_to_seconds(Round(d[iNote]));
          t[iNote] := FMIDIContainer.time_to_seconds(Round(t[iNote]));
        end;

        vv.LoadFromMSCVectors(n, d, s, t);

        for iEvent := 0 to FMIDIContainer.Count - 1 do
        begin
          e := FMIDIContainer.Event[iEvent];

          if (e.Channel = trk.Index) and (e.Event_Type = mc_MIDI_Control_Change) then
            vv.AddController(e.Data_Byte_1, e.Data_Byte_2, FMIDIContainer.time_to_seconds(e.Time));
        end;
      end;
    end;

    YMData := yms.Render;

    YMData.SongName := UnicodeString(edSongName.Text);
    YMData.Author := UnicodeString(edAuthor.Text);
  finally
    yms.Free;
  end;

  if SameText(ExtractFileExt(edOutput.Text), '.vgm') then
    yme := TYMVGMExporter.Create(edOutput.Text)
  else
    yme := TYMSNDHExporter.Create(edOutput.Text);
  try
    yme.Export(YMData);
  finally
    yme.Free;
  end;
end;

procedure TFormMain.btDnClick(Sender: TObject);
begin
  lbTracks.Exchange(lbTracks.ItemIndex, lbTracks.ItemIndex + 1);
  lbTracks.ItemIndex := lbTracks.ItemIndex + 1;
  UpdateGUI;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lbTracks.Count - 1 do
    lbTracks.Items.Objects[i].Free;

  FMIDIContainer.Free;
end;

procedure TFormMain.lbTracksClickCheck(Sender: TObject);
var
  iTrack: Integer;
  t: TTrack;
begin
  iTrack := lbTracks.ItemAtPos(lbTracks.ScreenToClient(Mouse.CursorPos), False);

  if iTrack >= 0 then
  begin
    t := TTrack(lbTracks.Items.Objects[iTrack]);
    t.Export := not t.Export;
    UpdateGUI;
  end;
end;

procedure TFormMain.lbTracksDblClick(Sender: TObject);
var
  iTrack: Integer;
  t: TTrack;
begin
  iTrack := lbTracks.ItemAtPos(lbTracks.ScreenToClient(Mouse.CursorPos), False);

  if iTrack >= 0 then
  begin
    t := TTrack(lbTracks.Items.Objects[iTrack]);
    if PromptForFileName(t.PatchFileName,'Cubase FXP|*.fxp') then
    begin
      t.Export := True;
      UpdateGUI;
    end;
  end;
end;

procedure TFormMain.lbTracksSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateGUI;
end;

procedure TFormMain.UpdateGUI;
var
  iTrack: Integer;
  t: TTrack;
  ok: Boolean;
begin
  ok := False;
  for iTrack := 0 to lbTracks.Count - 1 do
  begin
    t := TTrack(lbTracks.Items.Objects[iTrack]);

    lbTracks.Items[iTrack] := Format('Track: %2d, Name: %16s, NoteCount: %5d, PatchFile: %s', [t.Index, t.Name, t.NoteCount, t.PatchFileName]);
    lbTracks.ItemEnabled[iTrack] := FileExists(t.PatchFileName);
    lbTracks.Checked[iTrack] := t.Export and lbTracks.ItemEnabled[iTrack];

    ok := ok or lbTracks.Checked[iTrack];
  end;

  btUp.Enabled := lbTracks.ItemIndex > 0;
  btDn.Enabled := InRange(lbTracks.ItemIndex, 0, lbTracks.Count - 2);
  btLoad.Enabled := FileExists(edInputMid.Text);
  btConvert.Enabled := ok and (edOutput.Text <> '');
end;

end.

