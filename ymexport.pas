unit ymexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Types;

const
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


  { TYMVGMExporter }

  TYMVGMExporter = class
  private
    FFileName: String;
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;

    procedure Export(const AYMData: TYMData);
  end;

  function InvariantFormatSettings: TFormatSettings;

implementation
uses windows, Forms;

function InvariantFormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_INVARIANT, Result);
end;

{ TYMVGMExporter }

constructor TYMVGMExporter.Create(AFileName: String);
begin
  FFileName := AFileName;
end;

destructor TYMVGMExporter.Destroy;
begin
 inherited Destroy;
end;

procedure TYMVGMExporter.Export(const AYMData: TYMData);
var
  fs: TFileStream;

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

var
  iFrame, iReg, iVoice, gd3SizePos, actualWait: Integer;
  reg, maskedReg: Byte;
  ymSamplePos, ymPrevSamplePos: Double;
  ymAnyChanged: Boolean;
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
    fs.WriteByte($03); // AY8910 Flags (Legacy + Single)
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
      // handle square sync

      for iVoice := 0 to 2 do
        if AYMData.Frames[iFrame, iVoice * 2 + 1] and $10 <> 0 then
        begin
          ymState[iVoice * 2 + 0] := 0;

          fs.WriteByte($a0);
          fs.WriteByte(iVoice * 2 + 0);
          fs.WriteByte(0);

          ymState[iVoice * 2 + 1] := 0;

          fs.WriteByte($a0);
          fs.WriteByte(iVoice * 2 + 1);
          fs.WriteByte(0);
        end;

      // parse frames

      for iReg := 0 to High(ymState) do
      begin
        reg := AYMData.Frames[iFrame, iReg];
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

      // handle VGM waits

      if ymAnyChanged and not SameValue(ymSamplePos, ymPrevSamplePos) then
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

        if actualWait > 0 then
        begin
          fs.WriteByte($61);
          fs.WriteWord(actualWait);
        end;
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

      ymSamplePos += CVGMSampleRate / AYMData.FrameRate;
    end;

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

end.

