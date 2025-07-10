unit ymsynth;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, StrUtils, Math, Types, fgl, ymexport;

type
  TymVSTParameter = (
    yvpNone = -1,
    yvpInternalpatchName = 0,
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

const
  CYMBaseFreq = 2000000;
  CYMSquareNoiseFreq = CYMBaseFreq / 16;
  CYMBuzzFreq = CYMBaseFreq / 256;

  CVelocityToLevel: array[0 .. High(ShortInt)] of Byte = (
    0, 1, 2, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9,
    10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15,
    15, 15
  );

  CLevelToVelocity: array[0 .. 31] of Byte = (
    0, 1, 2, 3, 4, 5, 7, 10, 14, 18, 26, 34, 48, 64, 91, 120,
    127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127
  );


  CymVSTCCToParameter: array[0 .. High(ShortInt)] of TymVSTParameter = (
    yvpNone, yvpTremDepth, yvpNone, yvpHardwareMainTune, yvpNone, yvpPortamento, yvpNone, yvpNone, // 0
    yvpNone, yvpNoiseFreq, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 8
    yvpNoiseLength, yvpSquareLength, yvpHardLength, yvpArpLen, yvpE0, yvpE1, yvpE2, yvpE3, // 16
    yvpE4, yvpE5, yvpE6, yvpE7, yvpE8, yvpE9, yvpNoiseBendDepth, yvpNoiseBendRate, // 24
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 32
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 40
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 48
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 56
    yvpEnvSpeed, yvpNone, yvpNone, yvpNone, yvpPitchBendDir, yvpNone, yvpHardwareForm, yvpPitchBendRate, // 64
    yvpPitchBendDepth, yvpNone, yvpNone, yvpSquareSync, yvpHardSync, yvpArpSync, yvpSIDOnOff, yvpTimer, // 72
    yvpNoiseOnOff, yvpSquareOnOff, yvpHardOnOff, yvpArpOnOff, yvpNone, yvpNone, yvpNone, yvpArpSpeed, // 80
    yvpStep0, yvpStep1, yvpStep2, yvpNone, yvpTremFreq, yvpNone, yvpHardDetune, yvpSIDDetune, // 88
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 96
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 104
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, // 112
    yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone, yvpNone  // 120
  );

  CymVSTParameterValueCount: array[TymVSTParameter] of Integer = (
    1,
    1,
    11,2,9,2,
    16,16,16,16,16,16,16,16,16,16,
    9,16,11,2,2,73,4,
    1,
    63,16,32,11,2,
    97,2,16,
    16,
    16,2,
    1,
    11,2,2,
    37,37,37,
    3,
    16,9
  );


type

  TYMSynth = class;
  TYMVirtualVoice = class;

  { TYMController }

  TYMController = class
    Parameter: TymVSTParameter;
    Value: Byte;
    Time: Cardinal;
  end;

  TYMControllerList = specialize TFPGObjectList<TYMController>;

  { TYMPatch }

  TYMPatch = class
  private
    FParameters: array[TymVSTParameter] of Double;
  public
    procedure LoadFromCubaseFXP(AFileName: String);
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
    FControllers: TYMControllerList;
    FTicksDiv: Integer;
    FPrevSquareNote: Byte;
    FPortamentoNote: Byte;
  public
    constructor Create(ASynth: TYMSynth; APatchRef: TYMPatch);
    destructor Destroy; override;

    procedure AddController(CC, Value: Byte; Time: Double);
    procedure LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);
    function GetNoteAtTime(AFrame: Integer): TYMNote;
    function GetNoteCountAtTime(AFrame: Integer): Integer;

    function GetIntParameter(AYVP: TymVSTParameter; AFrame: Integer): Integer;
    function GetFloatParameter(AYVP: TymVSTParameter; AFrame: Integer): Double;

    function GetTicksPerVBL: Byte;
    function GetVolumeAt(AVelocity: Byte; ARelativeFrame, AFrame: Integer; AIsNoise: Boolean): Byte;
    function GetPitchAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Word;
    function GetNoiseAt(ARelativeFrame, AFrame: Integer): Byte;
    function GetBuzzAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Integer;

    property Synth: TYMSynth read FSynth;
    property PatchRef: TYMPatch read FPatchRef;
    property Notes: TYMNoteList read FNotes;
    property Controllers: TYMControllerList read FControllers;
    property TicksDiv: Integer read FTicksDiv write FTicksDiv;
  end;

  TYMVirtualVoiceList = specialize TFPGObjectList<TYMVirtualVoice>;

  { TYMSynth }

  TYMSynth = class
  private
    FEnableAutomations: Boolean;
    FSongLength: Double;
    FPatches: TYMPatchList;
    FVirtualVoices: TYMVirtualVoiceList;
    FPrevBuzz: Integer;
  public
    class function GetNoteHertz(ANote: Double): Double;
    class function GetHertzNote(AHertz: Double): Double;
    class function GetYMSquareNote(AHertz: Double): Word;
    class function GetYMBuzzNote(AHertz: Double): Cardinal;
    class function GetYMNoiseValue(AHertz: Double): Byte;
    class function GetYMNoiseHertz(AValue: Byte): Double;

    constructor Create(ASongLength: Double);
    destructor Destroy; override;

    function NotesToRegSet(ANotes: array of TYMNote; ANoteCnt, AFrame: Integer): TYMRegSet;
    function Render: TYMData;

    property EnableAutomations: Boolean read FEnableAutomations write FEnableAutomations;
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
  FControllers := TYMControllerList.Create;
end;

destructor TYMVirtualVoice.Destroy;
begin
  FControllers.Free;
  FNotes.Free;
  inherited Destroy;
end;

procedure TYMVirtualVoice.AddController(CC, Value: Byte; Time: Double);
var
  c: TYMController;
begin
  if CymVSTCCToParameter[CC] <> yvpNone then
  begin
    c := TYMController.Create;

    c.Parameter := CymVSTCCToParameter[CC];
    c.Value := Value;

    // take extents into account
    c.Value := Round(c.Value * (CymVSTParameterValueCount[c.Parameter] - 1) / High(ShortInt));

    c.Time := Round(Time * GetTicksPerVBL * CVBLPerSecond);

    if (Controllers.Count = 0) or (Controllers.Last.Parameter <> c.Parameter) or
        (Controllers.Last.Time <> c.Time) or
        (Controllers.Last.Value <> c.Value) then
      Controllers.Add(c)
    else
      c.Free;
  end;
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
    note.StartTime := Round(t[iNote] * GetTicksPerVBL * CVBLPerSecond);
    note.EndTime := Round((t[iNote] + d[iNote]) * GetTicksPerVBL * CVBLPerSecond);

    Notes.Add(note);
  end;
end;

function TYMVirtualVoice.GetNoteAtTime(AFrame: Integer): TYMNote;
var
  iNote: Integer;
  note: TYMNote;
begin
  AFrame := AFrame div TicksDiv;

  Result := nil;
  for iNote := 0 to Notes.Count - 1 do
  begin
    note := Notes[iNote];
    if (note.StartTime <= AFrame) and (note.EndTime > AFrame) then
      Result := note;
  end;
end;

function TYMVirtualVoice.GetNoteCountAtTime(AFrame: Integer): Integer;
var
  iNote: Integer;
  note: TYMNote;
begin
  AFrame := AFrame div TicksDiv;

  Result := 0;
  for iNote := 0 to Notes.Count - 1 do
  begin
    note := Notes[iNote];
    if (note.StartTime <= AFrame) and (note.EndTime >= AFrame) then
      Inc(Result);
  end;
end;

function TYMVirtualVoice.GetIntParameter(AYVP: TymVSTParameter; AFrame: Integer): Integer;
begin
  Result := Round(GetFloatParameter(AYVP, AFrame));
end;

function TYMVirtualVoice.GetFloatParameter(AYVP: TymVSTParameter; AFrame: Integer): Double;
var
  iCtrlr: Integer;
  c: TYMController;
begin
  Result := PatchRef.FParameters[AYVP];

  if Synth.EnableAutomations and (AFrame >= 0) then
    for iCtrlr := 0 to FControllers.Count - 1 do
    begin
      c := FControllers[iCtrlr];
      if (c.Parameter = AYVP) and (c.Time <= AFrame) then
      begin
        Result := c.Value;
      end;
    end;
end;

function TYMVirtualVoice.GetTicksPerVBL: Byte;
begin
  Result := 1 shl (2 - GetIntParameter(yvpTimer, -1)); //TODO: automating this sounds crazy...
end;

function TYMVirtualVoice.GetVolumeAt(AVelocity: Byte; ARelativeFrame, AFrame: Integer; AIsNoise: Boolean): Byte;
var
  yvpLen: TymVSTParameter;
begin
  if GetIntParameter(yvpHardOnOff, AFrame) <> 0 then
  begin
    Assert(InRange(AVelocity, 0, High(ShortInt)));

    Result := Round(GetFloatParameter(TymVSTParameter(Ord(yvpE0) + Min(9, ARelativeFrame shr GetIntParameter(yvpEnvSpeed, AFrame))), AFrame) - 0.25) * High(ShortInt) div 15;

    Assert(InRange(Result, 0, High(ShortInt)));

    Result := CVelocityToLevel[(AVelocity * Result) div High(ShortInt)];

    if AIsNoise then
      yvpLen := yvpNoiseLength
    else
      yvpLen := yvpSquareLength;
  end
  else
  begin
    Result := 16;

    yvpLen := yvpHardLength;
  end;

  if (GetIntParameter(yvpLen, AFrame) <> 0) and (ARelativeFrame >= GetIntParameter(yvpLen, AFrame)) then
    Result := 0;
end;

function TYMVirtualVoice.GetPitchAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Word;
var
  depth, frm, speed: Integer;
  porta, rate, note: Double;
begin
  Result := 0;

  note := ANote;

  // portamento

  porta := GetIntParameter(yvpPortamento, AFrame) * 0.25;

  if (ARelativeFrame = 0) and (porta > 0) and (GetNoteCountAtTime(AFrame * TicksDiv) > 1) then
    FPortamentoNote := FPrevSquareNote;

  if FPortamentoNote <> 0 then
  begin
    note := lerp(note, FPortamentoNote, Max(0, porta - ARelativeFrame / GetTicksPerVBL) / porta);
    if ARelativeFrame >= porta then
      FPortamentoNote := 0;
  end;

  // pitch envelope

  rate := (GetIntParameter(yvpPitchBendRate, AFrame) + 1) * IfThen(GetIntParameter(yvpPitchBendDir, AFrame) = 0, 0.5, -0.5);
  depth := GetIntParameter(yvpPitchBendDepth, AFrame) - 48;

  if rate > 0 then
    note := TYMSynth.GetHertzNote(lerp(TYMSynth.GetNoteHertz(note), TYMSynth.GetNoteHertz(note + depth), Max(0, rate - ARelativeFrame / GetTicksPerVBL) / rate))
  else if rate < 0 then
    note := TYMSynth.GetHertzNote(lerp(TYMSynth.GetNoteHertz(note + depth), TYMSynth.GetNoteHertz(note), Max(0, (-rate) - ARelativeFrame / GetTicksPerVBL) / -rate));

  // tremolo

  speed := GetIntParameter(yvpTremFreq, AFrame);
  depth := GetIntParameter(yvpTremDepth, AFrame);

  note += depth * 0.125 * Sin(AFrame / (1 shl speed) * 2.0 * Pi);

  // arpeggiator

  if GetIntParameter(yvpArpOnOff, AFrame) = 0 then
  begin
    speed := GetIntParameter(yvpArpSpeed, AFrame);
    if (GetIntParameter(yvpArpLen, AFrame) = 0) or (ARelativeFrame shr speed < GetIntParameter(yvpArpLen, AFrame)) then
    begin
      frm := IfThen(GetIntParameter(yvpArpSync, AFrame) = 0, ARelativeFrame, AFrame);
      note += GetIntParameter(TymVSTParameter(Ord(yvpStep0) + ((frm shr speed) mod 3)), AFrame);
    end;
  end;

  Result := TYMSynth.GetYMSquareNote(TYMSynth.GetNoteHertz(note));

  if (ARelativeFrame = 0) and (GetIntParameter(yvpSquareSync, AFrame) = 0) then
    Result := Result or $1000;
end;

function TYMVirtualVoice.GetNoiseAt(ARelativeFrame, AFrame: Integer): Byte;
var
  depth: Integer;
  freq, rate: Double;
begin
  freq := TYMSynth.GetYMNoiseHertz(31 - GetIntParameter(yvpNoiseFreq, AFrame));

  // noise pitch envelope

  rate := (GetIntParameter(yvpNoiseBendRate, AFrame) + 1) * 0.5;
  depth := GetIntParameter(yvpNoiseBendDepth, AFrame) - 31;

  freq := lerp(freq, TYMSynth.GetNoteHertz(TYMSynth.GetHertzNote(freq) + depth), Max(0, rate - ARelativeFrame / GetTicksPerVBL) / rate);

  Result := TYMSynth.GetYMNoiseValue(freq);
end;

function TYMVirtualVoice.GetBuzzAt(ANote: Integer; ARelativeFrame, AFrame: Integer): Integer;
var
  note: Double;
begin
  if GetIntParameter(yvpHardOnOff, AFrame) = 0 then
  begin
    note := ANote;
    note += GetIntParameter(yvpHardwareMainTune, AFrame) - 36;
    note += GetIntParameter(yvpHardDetune, AFrame) * 0.02 * 4.0 / 3.0;

    if GetIntParameter(yvpHardwareForm, AFrame) >= 2 then // tri is half speed
      note += 12;

    Result := TYMSynth.GetYMBuzzNote(TYMSynth.GetNoteHertz(note));

    case GetIntParameter(yvpHardwareForm, AFrame) of
      0: Result := Result or $c0000;
      1: Result := Result or $80000;
      2: Result := Result or $e0000;
      3: Result := Result or $a0000;
    end;

    if (ARelativeFrame = 0) and (GetIntParameter(yvpHardSync, AFrame) = 0) then
      Result := Result or $1000000;
  end
  else
  begin
    Result := -1;
  end;
end;

{ TYMPatch }

procedure TYMPatch.LoadFromCubaseFXP(AFileName: String);
var
  yvp: TymVSTParameter;
  fs: TFileStream;
  s: String;
  v: Cardinal;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    v := fs.ReadDWord;
    Assert(v = $4B6E6343, AFileName + ' not a Cubase file!');
    fs.ReadDWord;
    v := fs.ReadDWord;
    Assert(v = $68435046, AFileName + ' not a Cubase FXP file!');

    fs.Seek($f4, soFromBeginning);

    for yvp := Succ(Low(TymVSTParameter)) to High(TymVSTParameter) do
    begin
      v := fs.ReadByte;
      SetLength(s, v);
      fs.Read(s[1], v);
      FParameters[yvp] := StrToFloatDef(s, 0, InvariantFormatSettings);
    end;

  finally
    fs.Free;
  end;
end;

{ TYMSynth }

class function TYMSynth.GetNoteHertz(ANote: Double): Double;
begin
  Result := 440.0 * Power(2.0, (ANote - 69.0) / 12.0);
end;

class function TYMSynth.GetHertzNote(AHertz: Double): Double;
begin
  Result := 12.0 * Log2(AHertz / 440.0) + 69.0;
end;

class function TYMSynth.GetYMSquareNote(AHertz: Double): Word;
begin
  Result := EnsureRange(round(CYMSquareNoiseFreq / AHertz), 0, 4095);
end;

class function TYMSynth.GetYMBuzzNote(AHertz: Double): Cardinal;
begin
  Result := EnsureRange(round(CYMBuzzFreq / AHertz), 0, High(Word));
end;

class function TYMSynth.GetYMNoiseValue(AHertz: Double): Byte;
begin
  Result := EnsureRange(round(CYMSquareNoiseFreq / AHertz), 0, 31);
end;

class function TYMSynth.GetYMNoiseHertz(AValue: Byte): Double;
begin
 Result := CYMSquareNoiseFreq / AValue;
end;

constructor TYMSynth.Create(ASongLength: Double);
begin
  FEnableAutomations := True;
  FSongLength := ASongLength;
  FPatches := TYMPatchList.Create;
  FVirtualVoices := TYMVirtualVoiceList.Create;
  FPrevBuzz := -1;
end;

destructor TYMSynth.Destroy;
begin
  FVirtualVoices.Free;
  FPatches.Free;
  inherited Destroy;
end;

function TYMSynth.NotesToRegSet(ANotes: array of TYMNote; ANoteCnt, AFrame: Integer): TYMRegSet;
var
  iNote, iVoice, frame, relativeFrame, lvlDiff, bestLvlDiff, bestVoice: Integer;
  n: TYMNote;
  AssignedSquare: array[0 .. 2] of Boolean;
  AssignedNoise: array[0 .. 2] of Boolean;
  AssignedBuzz: array[0 .. 2] of Boolean;
  Pitch: array[0 .. 2] of Word;
  Level: array[0 .. 2] of Byte;
  noiseFreq, noiseLvl: ShortInt;
  buzz: Integer;
  assignedCount: Byte;
begin
  // init

  FillChar(Result, SizeOf(Result), 0);
  FillChar(AssignedSquare, SizeOf(AssignedSquare), 0);
  FillChar(AssignedNoise, SizeOf(AssignedNoise), 0);
  FillChar(AssignedBuzz, SizeOf(AssignedBuzz), 0);
  FillChar(Pitch, SizeOf(Pitch), 0);
  FillChar(Level, SizeOf(Level), 0);
  noiseFreq := -1;
  buzz := -1;

  // assign notes

  assignedCount := 0;

    // buzz

  for iNote := 0 to ANoteCnt - 1 do
  begin
    n := ANotes[iNote];

    frame := AFrame div n.VirtualVoice.TicksDiv;
    relativeFrame := frame - n.StartTime;

    if (n.VirtualVoice.GetIntParameter(yvpHardOnOff, frame) = 0) and (buzz < 0) and (assignedCount < Length(AssignedBuzz)) then
    begin
      buzz := n.VirtualVoice.GetBuzzAt(n.Note, relativeFrame, frame);
      Level[assignedCount] := n.VirtualVoice.GetVolumeAt(n.Velocity, relativeFrame, frame, False);

      if Level[assignedCount] > 0 then
      begin
        AssignedBuzz[assignedCount] := True;
        Inc(assignedCount);
      end;
    end;
  end;

    // square

  for iNote := 0 to ANoteCnt - 1 do
  begin
    n := ANotes[iNote];

    frame := AFrame div n.VirtualVoice.TicksDiv;
    relativeFrame := frame - n.StartTime;

    if (n.VirtualVoice.GetIntParameter(yvpSquareOnOff, frame) = 0) and (assignedCount < Length(AssignedSquare)) then
    begin
      Pitch[assignedCount] := n.VirtualVoice.GetPitchAt(n.Note, relativeFrame, frame);
      Level[assignedCount] := n.VirtualVoice.GetVolumeAt(n.Velocity, relativeFrame, frame, False);

      if Level[assignedCount] > 0 then
      begin
        AssignedSquare[assignedCount] := True;
        Inc(assignedCount);

        // for portamento
        n.VirtualVoice.FPrevSquareNote := n.Note;
      end;
    end;
  end;

    // noise

  for iNote := 0 to ANoteCnt - 1 do
  begin
    n := ANotes[iNote];

    frame := AFrame div n.VirtualVoice.TicksDiv;
    relativeFrame := frame - n.StartTime;

    if (n.VirtualVoice.GetIntParameter(yvpNoiseOnOff, frame) = 0) and (noiseFreq < 0) then
    begin
      noiseLvl := n.VirtualVoice.GetVolumeAt(n.Velocity, relativeFrame, frame, True);

      if noiseLvl > 0 then
      begin
        noiseFreq := n.VirtualVoice.GetNoiseAt(relativeFrame, frame);

        if assignedCount < Length(AssignedNoise) then
        begin
          AssignedNoise[assignedCount] := True;
          Level[assignedCount] := noiseLvl;
          Inc(assignedCount);
        end
        else
        begin
          // pair with the voice closest in volume

          bestVoice := -1;
          bestLvlDiff := MaxInt;
          for iVoice := 0 to High(AssignedSquare) do
          begin
            lvlDiff := Abs(CLevelToVelocity[Level[iVoice]] - CLevelToVelocity[noiseLvl]);

            if lvlDiff <= bestLvlDiff then
            begin
              bestVoice := iVoice;
              bestLvlDiff := lvlDiff;
            end;
          end;

          Assert(bestVoice >= 0);

          AssignedNoise[bestVoice] := True;
          Level[bestVoice] := noiseLvl;
        end;
      end;
    end;
  end;

  // generate regset

  for iVoice := 0 to High(AssignedSquare) do
  begin
    Result[iVoice * 2 + 0] := Pitch[iVoice] and $ff;
    Result[iVoice * 2 + 1] := (Pitch[iVoice] shr 8) and $1f;
    Result[7] := Result[7] or (Ord(not AssignedNoise[iVoice]) shl (iVoice + 3)) or (Ord(not AssignedSquare[iVoice]) shl iVoice);
    Result[iVoice + 8] := Level[iVoice];
  end;

  if noiseFreq >= 0 then
  begin
    Result[6] := noiseFreq;
  end;

  Result[13] := $ff; // do not reset the envelope by default
  if buzz >= 0 then
  begin
    Result[11] := buzz and $ff;
    Result[12] := (buzz shr 8) and $ff;

    if ((buzz shr 16) and $0f <> (FPrevBuzz shr 16) and $0f) or ((buzz shr 24) and $01 <> 0) then
      Result[13] := (buzz shr 16) and $0f;
  end;

  FPrevBuzz := buzz;
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
    TicksPerVBL := Max(TicksPerVBL, VirtualVoices[ivv].GetTicksPerVBL);

  // devise VoiceTicksDiv

  for iVV := 0 to VirtualVoices.Count - 1 do
  begin
    vv := VirtualVoices[iVV];
    vv.TicksDiv := TicksPerVBL div vv.GetTicksPerVBL;
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

      Notes[noteCnt] := vv.GetNoteAtTime(iFrame);
      if Assigned(Notes[noteCnt]) then
        Inc(noteCnt);
    end;

    RegSet := NotesToRegSet(Notes, noteCnt, iFrame);

    Move(RegSet, Result.Frames[iFrame, 0], SizeOf(RegSet));
  end;
end;

end.

