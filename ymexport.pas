unit ymexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Types;

const
  CYMTag = 'YM5!LeOnArD!';
  CYMEnd = 'End!';
  CYMFrequency: Cardinal = 2000000;

type

  TYMRegSet = array[0..15] of Byte;

  TYMRegSetArray = array of TYMRegSet;

  TYMData = record
    Frames: TYMRegSetArray;
    FrameRate: Word;
    SongName, Author: AnsiString;
  end;


  { TYMExporter }

  TYMExporter = class
  private
    FFileName: String;
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;

    procedure Export(const AYMData: TYMData);
  end;

implementation
uses Forms;

{ TYMExporter }

constructor TYMExporter.Create(AFileName: String);
begin
  FFileName := AFileName;
end;

destructor TYMExporter.Destroy;
begin
 inherited Destroy;
end;

procedure TYMExporter.Export(const AYMData: TYMData);
var
  iFrame: Integer;
  fs: TFileStream;
begin
  fs := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  try
    fs.Write(CYMTag[1], Length(CYMTag));

    fs.WriteDWord(NtoBE(Cardinal(Length(AYMData.Frames)))); // Nb of valid VBL of the file
    fs.WriteDWord(NtoBE(0)); // Song attributes  (TODO: not interleaved for now)
    fs.WriteWord(NtoBE(0)); // Nb of digi-drum sample
    fs.WriteDWord(NtoBE(CYMFrequency)); // YM2149 External frequency in Hz
    fs.WriteWord(NtoBE(AYMData.FrameRate)); // Player frequency in Hz
    fs.WriteDWord(NtoBE(0)); // VBL number to loop the song
    fs.WriteWord(NtoBE(0)); // Size (in bytes) of future additinal data

    fs.Write(AYMData.SongName[1], Length(AYMData.SongName));
    fs.WriteByte(0);

    fs.Write(AYMData.Author[1], Length(AYMData.Author));
    fs.WriteByte(0);

    fs.Write(Application.Title[1], Length(Application.Title));
    fs.WriteByte(0);

    for iFrame := 0 to High(AYMData.Frames) do
      fs.Write(AYMData.Frames[iFrame, 0], SizeOf(TYMRegSet));

    fs.Write(CYMEnd[1], Length(CYMEnd));
  finally
    fs.Free;
  end;
end;

end.

