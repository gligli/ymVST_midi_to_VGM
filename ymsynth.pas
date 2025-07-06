unit ymsynth;

{$mode ObjFPC}{$H+}

interface

uses
  windows, Classes, SysUtils, StrUtils, Math, Types, fgl, ymexport;

const
  CYMBaseFreq = 125000;
  CVBLPerSecond = 50;

  CVelocityToVolume: array[0 .. High(ShortInt)] of Byte = (
    0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15
  );

  CVolumeToVelocity: array[0 .. 15] of Byte = (
    0, 1, 2, 3, 4, 5, 7, 10, 14, 19, 27, 36, 51, 68, 96, 127
  );


type
  TymVSTParameter = (
    yvpInternalpatchName,
    yvpArpLen, yvpArpOnOff, yvpArpSpeed, yvpArpSync,
    yvpE0, yvpE1, yvpE2, yvpE3, yvpE4, yvpE5, yvpE6, yvpE7, yvpE8, yvpE9,
    yvpEnvSpeed, yvpHardDetune, yvpHardLength, yvpHardOnOff, yvpHardSync, yvpHardwareMainTune, yvpHardwareForm,
    yvpMasterVol,
    yvpNoiseBendDepth, yvpNoiseBendRate, yvpNoiseFreq, yvpNoiseLength, yvpNoiseOnOff,
    yvpPitchBendDepth, yvpPitchBendDir, yvpPitchBendRate,
    yvpPortamento,
    yvpSIDDetune, yvpSIDOnOff,
    yvpScope,
    yvpSquareLength, yvpSquareOnOff, yvpSquareSync,
    yvpStep0, yvpStep1, yvpStep2,
    yvpTimer,
    yvpTremDepth, yvpTremFreq
  );

  TYMSynth = class;
  TYMVirtualVoice = class;

  { TYMPatch }

  TYMPatch = class
    Parameters: array[TymVSTParameter] of Integer;
    ParametersFloat: array[TymVSTParameter] of Double;

    procedure LoadFromCubaseFXP(AFileName: String);

    function GetTicksPerVBL: Byte;

    function GetVolumeAt(AVelocity: Byte; ARelativeFrame: Integer): Byte;
    function GetPitchAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Word;
  end;

  TYMPatchList = specialize TFPGObjectList<TYMPatch>;

  { TYMNote }

  TYMNote = class
  public
    VirtualVoice: TYMVirtualVoice;
    Note, Velocity: Byte;
    StartTime, EndTime: Cardinal;

    constructor Create(AVV: TYMVirtualVoice);
  end;

  TYMNoteList = specialize TFPGObjectList<TYMNote>;

  { TYMVirtualVoice }

  TYMVirtualVoice = class
  private
    FSynth: TYMSynth;
    FPatchRef: TYMPatch;
    FNotes: TYMNoteList;
    FTicksDiv: Integer;
  public
    constructor Create(ASynth: TYMSynth; APatchRef: TYMPatch);
    destructor Destroy; override;

    procedure LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);
    function GetNoteAtTime(ATime: Cardinal): TYMNote;

    property Synth: TYMSynth read FSynth;
    property PatchRef: TYMPatch read FPatchRef;
    property Notes: TYMNoteList read FNotes;
    property TicksDiv: Integer read FTicksDiv write FTicksDiv;
  end;

  TYMVirtualVoiceList = specialize TFPGObjectList<TYMVirtualVoice>;

  { TYMSynth }

  TYMSynth = class
  private
    FSongLength: Double;
    FPatches: TYMPatchList;
    FVirtualVoices: TYMVirtualVoiceList;
  public
    class function GetNoteHertz(ANote: Double): Double;
    class function GetYMNote(AHertz: Double): Word;

    constructor Create(ASongLength: Double);
    destructor Destroy; override;

    function NotesToRegSet(ANotes: array of TYMNote; ANoteCnt, AFrameIdx: Integer): TYMRegSet;
    function Render: TYMData;

    property Patches: TYMPatchList read FPatches;
    property VirtualVoices: TYMVirtualVoiceList read FVirtualVoices;
  end;

implementation

function lerp(x, y, alpha: Double): Double; inline;
begin
  Result := x + (y - x) * alpha;
end;

function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
begin
  Result := x + ((y - x) * alpha) div maxAlpha;
end;

{ TYMNote }

constructor TYMNote.Create(AVV: TYMVirtualVoice);
begin
  VirtualVoice := AVV;
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
    note.StartTime := Round(t[iNote] * PatchRef.GetTicksPerVBL * CVBLPerSecond);
    note.EndTime := Round((t[iNote] + d[iNote]) * PatchRef.GetTicksPerVBL * CVBLPerSecond);

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

{ TYMPatch }

procedure TYMPatch.LoadFromCubaseFXP(AFileName: String);
var
  yvp: TymVSTParameter;
  fs: TFileStream;
  s: String;
  v: Cardinal;
  InvariantFormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_INVARIANT, InvariantFormatSettings);

  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    v := fs.ReadDWord;
    Assert(v = $4B6E6343, AFileName + ' not a Cubase file!');
    fs.ReadDWord;
    v := fs.ReadDWord;
    Assert(v = $68435046, AFileName + ' not a Cubase FXP file!');

    fs.Seek($f4, soFromBeginning);

    for yvp := Low(TymVSTParameter) to High(TymVSTParameter) do
    begin
      v := fs.ReadByte;
      SetLength(s, v);
      fs.Read(s[1], v);
      ParametersFloat[yvp] := StrToFloatDef(s, 0, InvariantFormatSettings);
      Parameters[yvp] := Round(ParametersFloat[yvp]);
    end;

  finally
    fs.Free;
  end;
end;

function TYMPatch.GetTicksPerVBL: Byte;
begin
  Result := 1 shl (2 - Parameters[yvpTimer]);
end;

function TYMPatch.GetVolumeAt(AVelocity: Byte; ARelativeFrame: Integer): Byte;
begin
  Assert(InRange(AVelocity, 0, High(ShortInt)));

  Result := Round(ParametersFloat[TymVSTParameter(Ord(yvpE0) + Min(9, ARelativeFrame shr Parameters[yvpEnvSpeed]))] * High(ShortInt) / 15);

  Assert(InRange(Result, 0, High(ShortInt)));

  Result := CVelocityToVolume[(AVelocity * Result) div High(ShortInt)];
end;

function TYMPatch.GetPitchAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Word;
var
  rate, depth: Integer;
  note: Double;
begin
  Result := 0;
  rate := Parameters[yvpPitchBendRate] * IfThen(Parameters[yvpPitchBendDir] = 0, 1, -1);
  depth := Parameters[yvpPitchBendDepth] - 48;

  if rate = 0 then
    note := ANote
  else if rate > 0 then
    note := lerp(ANote, ANote + depth, Max(0, rate - ARelativeFrame) / rate)
  else if rate < 0 then
    note := lerp(ANote + depth, ANote, Max(0, (-rate) - ARelativeFrame) / -rate);

  rate := Parameters[yvpTremFreq];
  depth := Parameters[yvpTremDepth];

  note += 0.25 * Sin(AFrame / IntPower(2.0, rate) * 2.0 * Pi);

  Result := TYMSynth.GetYMNote(TYMSynth.GetNoteHertz(note));
end;

{ TYMSynth }

class function TYMSynth.GetNoteHertz(ANote: Double): Double;
begin
  Result := 440.0 * Power(2.0, (ANote - 69.0) / 12.0);
end;

class function TYMSynth.GetYMNote(AHertz: Double): Word;
begin
  Result := EnsureRange(round(CYMBaseFreq / AHertz), 0, 4095);
end;

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

function TYMSynth.NotesToRegSet(ANotes: array of TYMNote; ANoteCnt, AFrameIdx: Integer): TYMRegSet;
var
  iNote, iVoice, RelativeFrame: Integer;
  n: TYMNote;
  Assigned: array[0 .. 2] of Boolean;
  Pitch: array[0 .. 2] of Word;
  Level: array[0 .. 2] of Byte;
  NoiseFreq: Byte;
  AssignedCount: Byte;
begin
  FillChar(Result, SizeOf(Result), 0);
  FillChar(Assigned, SizeOf(Assigned), 0);
  FillChar(Pitch, SizeOf(Pitch), 0);
  FillChar(Level, SizeOf(Level), 0);

  // assign notes

  AssignedCount := 0;
  for iNote := 0 to ANoteCnt - 1 do
  begin
    n := ANotes[iNote];

    RelativeFrame := AFrameIdx div n.VirtualVoice.TicksDiv - n.StartTime;

    if AssignedCount < Length(Assigned) then
    begin
      Assigned[AssignedCount] := True;
      Pitch[AssignedCount] := n.VirtualVoice.PatchRef.GetPitchAt(n.Note, RelativeFrame, AFrameIdx);
      Level[AssignedCount] := n.VirtualVoice.PatchRef.GetVolumeAt(n.Velocity, RelativeFrame);
      Inc(AssignedCount);
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
  vv: TYMVirtualVoice;
  RegSet: TYMRegSet;
  Notes: array of TYMNote;
begin

  // find highest TicksPerVBL

  TicksPerVBL := 1;
  for iVV := 0 to VirtualVoices.Count - 1 do
    TicksPerVBL := Max(TicksPerVBL, VirtualVoices[ivv].PatchRef.GetTicksPerVBL);

  // devise VoiceTicksDiv

  for iVV := 0 to VirtualVoices.Count - 1 do
  begin
    vv := VirtualVoices[iVV];

    vv.TicksDiv := TicksPerVBL div vv.PatchRef.GetTicksPerVBL;
  end;

  // properties

  Result.FrameRate := CVBLPerSecond * TicksPerVBL;
  frmCnt := Ceil(FSongLength * Result.FrameRate);
  SetLength(Result.Frames, frmCnt);

  // actual rendering

  SetLength(Notes, VirtualVoices.Count);

  for iFrame := 0 to High(Result.Frames) do
  begin
    FillChar(Notes[0], Length(Notes) * SizeOf(Pointer), 0);

    noteCnt := 0;
    for iVV := 0 to VirtualVoices.Count - 1 do
    begin
      vv := VirtualVoices[iVV];

      Notes[noteCnt] := vv.GetNoteAtTime(iFrame div vv.TicksDiv);
      if Assigned(Notes[noteCnt]) then
        Inc(noteCnt);
    end;

    RegSet := NotesToRegSet(Notes, noteCnt, iFrame);

    Move(RegSet, Result.Frames[iFrame, 0], SizeOf(RegSet));
  end;
end;

end.

