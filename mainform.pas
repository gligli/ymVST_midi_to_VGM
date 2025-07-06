unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst, Spin,
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
   btInputBrowse: TButton;
   btLoad: TButton;
   edAuthor: TEdit;
   edSongName: TEdit;
   lbTracks: TCheckListBox;
   edInputMid: TEdit;
   edOutputYM: TEdit;
   llAuthor: TLabel;
   llBPM: TLabel;
   llHint: TLabel;
   llSongName: TLabel;
   llTime: TLabel;
   seBPM: TSpinEdit;
   seLength: TFloatSpinEdit;
   procedure btConvertClick(Sender: TObject);
   procedure btInputBrowseClick(Sender: TObject);
   procedure btLoadClick(Sender: TObject);
   procedure FormCreate(Sender: TObject);
   procedure FormDestroy(Sender: TObject);
   procedure lbTracksClickCheck(Sender: TObject);
   procedure lbTracksDblClick(Sender: TObject);
 private
   FMIDIContainer: TMIDI_Container;

   procedure NameTracks;
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

  NameTracks;
end;

procedure TFormMain.btInputBrowseClick(Sender: TObject);
var
  fn: String;
begin
  fn := edInputMid.Text;
  if PromptForFileName(fn, '*.mid') then
  begin
    edInputMid.Text := fn;
    edOutputYM.Text := ChangeFileExt(fn, '.ym');
  end;
end;

procedure TFormMain.btConvertClick(Sender: TObject);
var
  iTrack, iNote: Integer;
  YMData: TYMData;
  trk: TTrack;
  p: TYMPatch;
  vv: TYMVirtualVoice;
  yms: TYMSynth;
  yme: TYMExporter;
  n, d, s, t: TReal_Array;
begin
  yms := TYMSynth.Create(seLength.Value);
  try
    for iTrack := 0 to lbTracks.Count - 1 do
    begin
      trk := TTrack(lbTracks.Items.Objects[iTrack]);

      if trk.Export then
      begin
        p := TYMPatch.Create;
        yms.Patches.Add(p);

        // TODO: TMP
        p.HasSquare := True;
        p.TicksPerVBL := 2;

        vv := TYMVirtualVoice.Create(yms, p);
        yms.VirtualVoices.Add(vv);

        FMIDIContainer.get_vectors(trk.Index, n, d, s, t);

        for iNote := 0 to High(n) do
        begin
          d[iNote] := FMIDIContainer.time_to_seconds(Round(d[iNote]));
          t[iNote] := FMIDIContainer.time_to_seconds(Round(t[iNote]));
        end;

        vv.LoadFromMSCVectors(n, d, s, t);
      end;
    end;

    YMData := yms.Render;

    YMData.SongName := edSongName.Text;
    YMData.Author := edAuthor.Text;
  finally
    yms.Free;
  end;

  yme := TYMExporter.Create(edOutputYM.Text);
  try
    yme.Export(YMData);
  finally
    yme.Free;
  end;
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
    NameTracks;
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
    if PromptForFileName(t.PatchFileName) then
      NameTracks;
  end;
end;

procedure TFormMain.NameTracks;
var
  iTrack: Integer;
  t: TTrack;
begin
  for iTrack := 0 to lbTracks.Count - 1 do
  begin
    t := TTrack(lbTracks.Items.Objects[iTrack]);

    lbTracks.Items[iTrack] := Format('Track: %2d, Name: %16s, NoteCount: %5d, PatchFile: %s', [t.Index, t.Name, t.NoteCount, t.PatchFileName]);
    lbTracks.Checked[iTrack] := t.Export;
  end;
end;

end.

