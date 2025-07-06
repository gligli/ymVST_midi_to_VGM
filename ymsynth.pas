unit ymsynth;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Types, fgl, ymexport;

const
  CVBLPerSecond = 50;

type
  TYMSynth = class;
  TYMVirtualVoice = class;

  { TYMPatch }

  TYMPatch = class
    HasNoise: Boolean;
    HasSquare: Boolean;
    TicksPerVBL: Byte;
  end;

  TYMPatchList = specialize TFPGObjectList<TYMPatch>;

  { TYMNote }

  TYMNote = class
  public
    VirtualVoice: TYMVirtualVoice;
    Note, Velocity: Byte;
    StartTime, EndTime: Cardinal;

    constructor Create(AVV: TYMVirtualVoice);

    function GetNoteHertz: Double;
    function GetYMNote: Word;
  end;

  TYMNoteList = specialize TFPGObjectList<TYMNote>;

  { TYMVirtualVoice }

  TYMVirtualVoice = class
  private
    FSynth: TYMSynth;
    FPatchRef: TYMPatch;
    FNotes: TYMNoteList;
  public
    constructor Create(ASynth: TYMSynth; APatchRef: TYMPatch);
    destructor Destroy; override;

    procedure LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);
    function GetNoteAtTime(ATime: Cardinal): TYMNote;

    property Synth: TYMSynth read FSynth;
    property PatchRef: TYMPatch read FPatchRef;
    property Notes: TYMNoteList read FNotes;
  end;

  TYMVirtualVoiceList = specialize TFPGObjectList<TYMVirtualVoice>;

  { TYMSynth }

  TYMSynth = class
  private
    FSongLength: Double;
    FPatches: TYMPatchList;
    FVirtualVoices: TYMVirtualVoiceList;
  public
    constructor Create(ASongLength: Double);
    destructor Destroy; override;

    function NotesToRegSet(ANotes: array of TYMNote; ANoteCnt: Integer): TYMRegSet;
    function Render: TYMData;

    property Patches: TYMPatchList read FPatches;
    property VirtualVoices: TYMVirtualVoiceList read FVirtualVoices;
  end;

implementation

{ TYMNote }

constructor TYMNote.Create(AVV: TYMVirtualVoice);
begin
  VirtualVoice := AVV;
end;

function TYMNote.GetNoteHertz: Double;
begin
  Result := 440.0 * Power(2.0, (Integer(Note) - 69) / 12.0);
end;

function TYMNote.GetYMNote: Word;
begin
  Result := round(125000.0 / GetNoteHertz);
end;

{ TYMVirtualVoice }

constructor TYMVirtualVoice.Create(ASynth: TYMSynth; APatchRef: TYMPatch);
begin
  FSynth := ASynth;
  FPatchRef := APatchRef;
  FNotes := TYMNoteList.Create;
end;

destructor TYMVirtualVoice.Destroy;
begin
  FNotes.Free;
  inherited Destroy;
end;

procedure TYMVirtualVoice.LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);
var
  iNote: Integer;
  note: TYMNote;
begin
  for iNote := 0 to High(n) do
  begin
    note := TYMNote.Create(Self);

    note.Note := Round(n[iNote]);
    note.Velocity := Round(s[iNote]);
    note.StartTime := Round(t[iNote] * PatchRef.TicksPerVBL * CVBLPerSecond);
    note.EndTime := Round((t[iNote] + d[iNote]) * PatchRef.TicksPerVBL * CVBLPerSecond);

    Notes.Add(note);
  end;
end;

function TYMVirtualVoice.GetNoteAtTime(ATime: Cardinal): TYMNote;
var
  iNote: Integer;
  note: TYMNote;
begin
  Result := nil;
  for iNote := 0 to Notes.Count - 1 do
  begin
    note := Notes[iNote];
    if (note.StartTime <= ATime) and (note.EndTime > ATime) then
    begin
      Result := note;
      Break;
    end;
  end;
end;

{ TYMSynth }

constructor TYMSynth.Create(ASongLength: Double);
begin
  FSongLength := ASongLength;
  FPatches := TYMPatchList.Create;
  FVirtualVoices := TYMVirtualVoiceList.Create;
end;

destructor TYMSynth.Destroy;
begin
  FVirtualVoices.Free;
  FPatches.Free;
  inherited Destroy;
end;

function TYMSynth.NotesToRegSet(ANotes: array of TYMNote; ANoteCnt: Integer): TYMRegSet;
var
  iNote, iVoice: Integer;
  n: TYMNote;
  Assigned: array[0 .. 2] of Boolean;
  Pitch: array[0 .. 2] of Word;
  Level: array[0 .. 2] of Byte;
  NoiseFreq: Byte;
  AssignedPitchCount: Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
  FillChar(Assigned, SizeOf(Assigned), 0);
  FillChar(Pitch, SizeOf(Pitch), 0);
  FillChar(Level, SizeOf(Level), 0);

  // assign notes

  AssignedPitchCount := 0;
  for iNote := 0 to ANoteCnt - 1 do
  begin
    n := ANotes[iNote];

    if n.VirtualVoice.PatchRef.HasSquare and (AssignedPitchCount < Length(Pitch)) then
    begin
      Assigned[AssignedPitchCount] := True;
      Pitch[AssignedPitchCount] := n.GetYMNote;
      Level[AssignedPitchCount] := n.Velocity shr 3;
      Inc(AssignedPitchCount);
    end;
  end;

  // generate regset

  for iVoice := 0 to High(Assigned) do
  begin
    Result[iVoice * 2 + 0] := Pitch[iVoice] and $ff;
    Result[iVoice * 2 + 1] := Pitch[iVoice] shr 8;
    Result[7] := Result[7] or (Ord(not Assigned[iVoice]) shl iVoice);
    Result[iVoice + 8] := Level[iVoice];
  end;
end;

function TYMSynth.Render: TYMData;
var
  iVV, iFrame, frmCnt, noteCnt: Integer;
  TicksPerVBL: Byte;
  VoiceTicksDiv: TByteDynArray;
  RegSet: TYMRegSet;
  Notes: array of TYMNote;
begin

  // find highest TicksPerVBL

  TicksPerVBL := 1;
  for iVV := 0 to VirtualVoices.Count - 1 do
    TicksPerVBL := Max(TicksPerVBL, VirtualVoices[ivv].PatchRef.TicksPerVBL);

  // devise VoiceTicksDiv

  SetLength(VoiceTicksDiv, VirtualVoices.Count);
  for iVV := 0 to VirtualVoices.Count - 1 do
    VoiceTicksDiv[iVV] := TicksPerVBL div VirtualVoices[ivv].PatchRef.TicksPerVBL;

  // properties

  Result.FrameRate := CVBLPerSecond * TicksPerVBL;
  frmCnt := Ceil(FSongLength * TicksPerVBL * CVBLPerSecond);
  SetLength(Result.Frames, frmCnt);

  // actual rendering

  SetLength(Notes, VirtualVoices.Count);

  for iFrame := 0 to High(Result.Frames) do
  begin
    FillChar(Notes[0], Length(Notes) * SizeOf(Pointer), 0);

    noteCnt := 0;
    for iVV := 0 to VirtualVoices.Count - 1 do
    begin
      Notes[noteCnt] := VirtualVoices[iVV].GetNoteAtTime(iFrame div VoiceTicksDiv[iVV]);
      if Assigned(Notes[noteCnt]) then
        Inc(noteCnt);
    end;

    RegSet := NotesToRegSet(Notes, noteCnt);

    Move(RegSet, Result.Frames[iFrame, 0], SizeOf(RegSet));
  end;
end;

end.

