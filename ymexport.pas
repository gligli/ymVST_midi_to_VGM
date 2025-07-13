unit ymexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Types;

const
  CSNDHReplayerFileName = 'Atari_SNDH_replayer\sndh.bin';
  CVGMTag = 'Vgm ';
  CGD3Tag = 'Gd3 ';
  CVGMVersion = $151;
  CVGMSampleRate = 44100;
  CYMFrequency: Cardinal = 2000000;
  CVBLPerSecond = 50;
  CSTSystemName = 'Atari ST';


  CYMRegsMasks: array[0 .. 15] of Byte = (
    $ff, $0f, $ff, $0f, $ff, $0f,
    $1f,
    $3f,
    $1f, $1f, $1f,
    $ff, $ff, $0f,
    $00, $00
  );

type

  TYMRegSet = array[0 .. 15] of Byte;

  TYMRegSetArray = array of TYMRegSet;

  TYMData = record
    Frames: TYMRegSetArray;
    FrameRate: Word;
    SongName, Author: UnicodeString;
  end;

  { TYMBaseExporter }

  TYMBaseExporter = class
  private
    FFileName: String;
  protected
    procedure ParseFrameData(const AFrameRegData: TYMRegSet; var ymState: TYMRegSet; var ymRegChanged: array of Boolean;
     var ymAnyChanged: Boolean);
  public
    constructor Create(AFileName: String); virtual;

    procedure Export(const AYMData: TYMData); virtual; abstract;
  end;

  { TYMVGMExporter }

  TYMVGMExporter = class(TYMBaseExporter)
  public
    procedure Export(const AYMData: TYMData); override;
  end;

  { TYMSNDHExporter }

  TYMSNDHExporter = class(TYMBaseExporter)
  public
    procedure Export(const AYMData: TYMData); override;
  end;

  function InvariantFormatSettings: TFormatSettings;

implementation
uses windows, Forms;

function InvariantFormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_INVARIANT, Result);
end;

{ TYMBaseExporter }

procedure TYMBaseExporter.ParseFrameData(const AFrameRegData: TYMRegSet; var ymState: TYMRegSet; var ymRegChanged: array of Boolean; var ymAnyChanged: Boolean);
var
  iReg: Integer;
  reg, maskedReg: Byte;
begin
  for iReg := 0 to High(TYMRegSet) - 2 do
  begin
    reg := AFrameRegData[iReg];
    maskedReg := reg and CYMRegsMasks[iReg];

    if (maskedReg <> ymState[iReg]) or (iReg = 13) then
    begin
      ymState[iReg] := maskedReg;

      if (iReg <> 13) or (reg <> $ff) then // account for not resetting the SSG envelope
      begin
        ymRegChanged[iReg] := True;
        ymAnyChanged := True;
      end;
    end;
  end;
end;

constructor TYMBaseExporter.Create(AFileName: String);
begin
  FFileName := AFileName;
end;

{ TYMVGMExporter }

procedure TYMVGMExporter.Export(const AYMData: TYMData);
var
  fs: TFileStream;
  ymSamplePos, ymPrevSamplePos: Double;
  ymAnyChanged: Boolean;

  procedure WriteString(AString: UnicodeString);
  var
    p: PUnicodeChar;
  begin
    p := GetMem((Length(AString) + 1) * SizeOf(UnicodeChar));
    try
      if Length(AString) > 0 then
        Move(AString[1], p^, Length(AString) * SizeOf(UnicodeChar));
      p[Length(AString)] := #0;
      fs.Write(p^, (Length(AString) + 1) * SizeOf(UnicodeChar));
    finally
      Freemem(p);
    end;
  end;

  procedure HandleVGMWaits(AForce: Boolean);
  var
    actualWait: Integer;
  begin
    if (ymAnyChanged or AForce) and not SameValue(ymSamplePos, ymPrevSamplePos) then
    begin
      actualWait := Round(ymSamplePos - ymPrevSamplePos);
      ymPrevSamplePos += actualWait;
      ymAnyChanged := False;

      while actualWait >= High(Word) do
      begin
        fs.WriteByte($61);
        fs.WriteWord(High(Word));
        actualWait -= High(Word);
      end;

      if actualWait = 882 then
      begin
        fs.WriteByte($63);
      end
      else if actualWait > 0 then
      begin
        fs.WriteByte($61);
        fs.WriteWord(actualWait);
      end;
    end;
  end;

var
  iFrame, iReg, iVoice, gd3SizePos, ymSquareSyncCnt: Integer;
  ymRegChanged: array[0 .. High(TYMRegSet)] of Boolean;
  ymState: TYMRegSet;
begin
  fs := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  try
    // write header

    fs.Write(CVGMTag[1], Length(CVGMTag));
    fs.WriteDWord(0); // EoF offset (dummy, will be written later)
    fs.WriteDWord(CVGMVersion);
    fs.WriteDWord(0); // SN76489 clock (none)
    fs.WriteDWord(0); // YM2413 clock (none)
    fs.WriteDWord(0); // GD3 offset (dummy, will be written later)
    fs.WriteDWord(Round(Length(AYMData.Frames) / AYMData.FrameRate * CVGMSampleRate)); // Total # samples
    fs.WriteDWord(0); // Loop offset (none)
    fs.WriteDWord(0); // Loop # samples (none)
    fs.WriteDWord(CVBLPerSecond); // Rate
    fs.Seek($74, soBeginning);
    fs.WriteDWord(CYMFrequency); // AY8910 clock
    fs.WriteByte($10); // AY8910 Chip Type (YM2149)
    fs.WriteByte($02); // AY8910 Flags (Single)
    fs.WriteByte($00); // YM2203/AY8910 Flags (none)
    fs.WriteByte($00); // YM2608/AY8910 Flags (none)
    fs.WriteByte($00); // Volume Modifier (100%)
    fs.WriteByte($00); // reserved (0)
    fs.WriteByte($00); // Loop Base (none)
    fs.WriteByte($00); // Loop Modifier (none)

    // update VGM data offset

    fs.Seek($34, soBeginning);
    fs.WriteDWord(fs.Size - $34);
    fs.Seek(0, soEnd);

    // write VGM data

    FillChar(ymState, SizeOf(ymState), $ff);
    FillChar(ymRegChanged, SizeOf(ymState), 0);
    ymAnyChanged := False;
    ymSamplePos := 0.0;
    ymPrevSamplePos := 0.0;

    for iFrame := 0 to High(AYMData.Frames) do
    begin
      // parse frame data

      ParseFrameData(AYMData.Frames[iFrame], ymState, ymRegChanged, ymAnyChanged);

      // handle VGM waits

      HandleVGMWaits(False);

      // handle square sync

      ymSquareSyncCnt := 0;
      for iVoice := 0 to 2 do
        if AYMData.Frames[iFrame, iVoice * 2 + 1] and $10 <> 0 then
        begin
          fs.WriteByte($a0);
          fs.WriteByte(iVoice * 2 + 0);
          fs.WriteByte($ff);

          fs.WriteByte($a0);
          fs.WriteByte(iVoice * 2 + 1);
          fs.WriteByte($0f);

          fs.WriteByte($70); // wait 1 sample

          Inc(ymSquareSyncCnt, 1);
        end;

      // write frame data

      for iReg := 0 to High(ymState) do
        if ymRegChanged[iReg] then
        begin
          fs.WriteByte($a0);
          fs.WriteByte(iReg);
          fs.WriteByte(ymState[iReg]);

          ymRegChanged[iReg] := False;
        end;

      // update VGM position

      ymSamplePos += CVGMSampleRate / AYMData.FrameRate - ymSquareSyncCnt;
    end;

    HandleVGMWaits(True);

    fs.WriteByte($66);

    // update GD3 offset

    fs.Seek($14, soBeginning);
    fs.WriteDWord(fs.Size - $14);
    fs.Seek(0, soEnd);

    // write GD3 data

    fs.Write(CGD3Tag[1], Length(CGD3Tag));
    fs.WriteDWord(BEtoN($00010000)); // Gd3 version number
    gd3SizePos := fs.Size;
    fs.WriteDWord(0); // Gd3 size (dummy, will be written later)

    WriteString(AYMData.SongName); // Track name (in English characters)
    WriteString(AYMData.SongName); // Track name (in original (non-English) game language characters)
    WriteString(''); // Game name (in English characters)
    WriteString(''); // Game name (in original (non-English) game characters)
    WriteString(CSTSystemName); // System name (in English characters)
    WriteString(CSTSystemName); // System name (in original (non-English) game characters)
    WriteString(AYMData.Author); // Name of Original Track Author (in English characters)
    WriteString(AYMData.Author); // Name of Original Track Author (in original (non-English) game characters)
    WriteString(UnicodeString(FormatDateTime('yyyy/mm/dd', Date))); // Date of game's release written in the form yyyy/mm/dd, or just yyyy/mm or yyyy if month and day is not known
    WriteString(UnicodeString(Application.Title)); // Name of person who converted it to a VGM file.
    WriteString(''); // Notes

    fs.Seek(gd3SizePos, soBeginning);
    fs.WriteDWord(fs.Size - gd3SizePos);
    fs.Seek(0, soEnd);

    // update EoF offset in header

    fs.Seek($04, soBeginning);
    fs.WriteDWord(fs.Size - $04);
  finally
    fs.Free;
  end;
end;

{ TYMSNDHExporter }

procedure TYMSNDHExporter.Export(const AYMData: TYMData);
var
  fs, rfs: TFileStream;
  AnyChanged: Boolean;
  Regs: TYMRegSet;
  RegChanged: array[0 .. High(TYMRegSet)] of Boolean;

  procedure WriteString(AString: AnsiString);
  var
    p: PAnsiChar;
  begin
    p := GetMem((Length(AString) + 1) * SizeOf(AnsiChar));
    try
      if Length(AString) > 0 then
        Move(AString[1], p^, Length(AString) * SizeOf(AnsiChar));
      p[Length(AString)] := #0;
      fs.Write(p^, (Length(AString) + 1) * SizeOf(AnsiChar));
    finally
      Freemem(p);
    end;
  end;

  procedure WriteHeaderString(ATag, AString: AnsiString);
  begin
    fs.Write(ATag[1], Length(ATag));
    WriteString(AString);
  end;

  procedure WriteHeaderWord(ATag: AnsiString; AWord: Word);
  begin
    fs.Write(ATag[1], Length(ATag));
    fs.WriteWord(NtoBE(AWord));
  end;

  function HandleFrameWaits(AFrame: Integer; AForce: Boolean): Boolean;
  var
    iFutureFrame, actualWait: Integer;
    futureAnyChanged: Boolean;
    futureRegs: TYMRegSet;
    futureRegChanged: array[0 .. High(TYMRegSet)] of Boolean;
  begin
    Result:= False;

    if (AnyChanged or AForce) then
    begin
      actualWait := 0;
      Move(Regs[0], futureRegs[0], Length(futureRegs));
      FillChar(futureRegChanged[0], Length(futureRegChanged), 0);
      futureAnyChanged := False;
      for iFutureFrame := AFrame + 1 to High(AYMData.Frames) do
      begin
        Inc(actualWait);
        ParseFrameData(AYMData.Frames[iFutureFrame], futureRegs, futureRegChanged, futureAnyChanged);
        if futureAnyChanged then
          Break;
      end;

      while actualWait >= High(Byte) do
      begin
        Regs[15] := High(Byte);
        RegChanged[15] := True;
        AnyChanged := True;

        fs.WriteWord(NtoBE($0001));
        fs.WriteByte(High(Byte));

        actualWait -= High(Byte);
      end;

      if Regs[15] <> actualWait then
      begin
        Regs[15] := actualWait;
        RegChanged[15] := True;
        AnyChanged := True;
      end;

      Result := True;
    end;
  end;

  procedure WriteRegSet;
  var
    iReg: Integer;
    regInd: Word;
  begin
    if AnyChanged then
    begin
      regInd := 0;
      for iReg := 0 to High(TYMRegSet) do
        if RegChanged[iReg] then
          regInd := regInd or ($8000 shr iReg);
      fs.WriteWord(NtoBE(regInd));

      for iReg := 0 to High(TYMRegSet) do
        if RegChanged[iReg] then
        begin
          fs.WriteByte(Regs[iReg]);
          RegChanged[iReg] := False;
        end;
      AnyChanged := False;
    end;
  end;

var
  iFrame: Integer;
begin
  fs := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  rfs := TFileStream.Create(ExtractFilePath(Application.ExeName) + CSNDHReplayerFileName, fmOpenRead or fmShareDenyNone);
  try
    // write replayer

    fs.CopyFrom(rfs, rfs.Size);

    // write sndh header

    fs.Seek($10, soBeginning);

    if AYMData.SongName <> '' then
      WriteHeaderString('TITL', AnsiString(AYMData.SongName));
    if AYMData.Author <> '' then
      WriteHeaderString('COMM', AnsiString(AYMData.Author));
    WriteHeaderString('CONV', Application.Title);
    WriteHeaderString('TC', IntToStr(AYMData.FrameRate));
    WriteHeaderString('YEAR', FormatDateTime('yyyy', Date));
    WriteHeaderWord('TIME', Ceil(Length(AYMData.Frames) / AYMData.FrameRate));

    WriteHeaderString('HDNS', '');

    // write song data

    fs.Seek(rfs.Size, soBeginning);

    FillChar(Regs, SizeOf(Regs), $ff);
    FillChar(RegChanged, SizeOf(RegChanged), 0);

    for iFrame := 0 to High(AYMData.Frames) do
    begin
      ParseFrameData(AYMData.Frames[iFrame], Regs, RegChanged, AnyChanged);
      HandleFrameWaits(iFrame, False);
      WriteRegSet;
    end;

    HandleFrameWaits(Length(AYMData.Frames), True);

    // terminator
    fs.WriteWord(NtoBE($0001));
    fs.WriteByte($00);

  finally
    rfs.Free;
    fs.Free;
  end;
end;

end.

