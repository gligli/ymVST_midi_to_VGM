unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst, Spin,
  MSC_Definitions, MSC_Container, ymexport;

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
   lbChannels: TCheckListBox;
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
   procedure lbChannelsDblClick(Sender: TObject);
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
  for i := 0 to lbChannels.Count - 1 do
    lbChannels.Items.Objects[i].Free;
  lbChannels.Clear;

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

      lbChannels.AddItem('dummy', t);
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
  YMData: TYMData;
  yme: TYMExporter;
begin
  YMData.FrameRate := 200;
  YMData.SongName := edSongName.Text;
  YMData.Author := edAuthor.Text;

  SetLength(YMData.Frames, 200);

  yme := TYMExporter.Create(edOutputYM.Text);
  try
    yme.Export(YMData);
  finally
    yme.Free;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FMIDIContainer.Free;
end;

procedure TFormMain.lbChannelsDblClick(Sender: TObject);
var
  i: Integer;
  t: TTrack;
begin
  i := lbChannels.ItemAtPos(lbChannels.ScreenToClient(Mouse.CursorPos), False);

  if i >= 0 then
  begin
    t := TTrack(lbChannels.Items.Objects[i]);
    if PromptForFileName(t.PatchFileName) then
      NameTracks;
  end;
end;

procedure TFormMain.NameTracks;
var
  i: Integer;
  t: TTrack;
begin
  for i := 0 to lbChannels.Count - 1 do
  begin
    t := TTrack(lbChannels.Items.Objects[i]);

    lbChannels.Items[i] := Format('Track: %2d, Name: %16s, NoteCount: %5d, PatchFile: %s', [t.Index, t.Name, t.NoteCount, t.PatchFileName]);
    lbChannels.Checked[i] := t.Export;
  end;
end;

end.

