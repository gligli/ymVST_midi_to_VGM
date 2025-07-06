unit ymsynth;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, Types, fgl, ymexport;

const
  CVBLPerSecond = 50;

type

  { TYMPatch }

  TYMPatch = class
    HasNoise: Boolean;
    HasSquare: Boolean;
    TicksPerVBL: Byte;
  end;

  TYMPatchList = specialize TFPGObjectList<TYMPatch>;

  { TYMVirtualVoice }

  TYMVirtualVoice = class
  private
    FPatchRef: TYMPatch;
  public
    constructor Create(APatchRef: TYMPatch);
    destructor Destroy; override;

    procedure LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);

    property PatchRef: TYMPatch read FPatchRef;
  end;

  TYMVirtualVoiceList = specialize TFPGObjectList<TYMVirtualVoice>;

  { TYMSynth }

  TYMSynth = class
  private
    FPatches: TYMPatchList;
    FVirtualVoices: TYMVirtualVoiceList;
  public
    constructor Create;
    destructor Destroy; override;

    function Render: TYMData;

    property Patches: TYMPatchList read FPatches;
    property VirtualVoices: TYMVirtualVoiceList read FVirtualVoices;
  end;

implementation

{ TYMVirtualVoice }

constructor TYMVirtualVoice.Create(APatchRef: TYMPatch);
begin
 FPatchRef := APatchRef;
end;

destructor TYMVirtualVoice.Destroy;
begin
 inherited Destroy;
end;

procedure TYMVirtualVoice.LoadFromMSCVectors(const n, d, s, t: TDoubleDynArray);
begin

end;

{ TYMSynth }

constructor TYMSynth.Create;
begin
 FPatches := TYMPatchList.Create;
 FVirtualVoices := TYMVirtualVoiceList.Create;
end;

destructor TYMSynth.Destroy;
begin
 FVirtualVoices.Free;
 FPatches.Free;
 inherited Destroy;
end;

function TYMSynth.Render: TYMData;
var
  iVV: Integer;
  TicksPerVBL: Byte;
begin

  // find highest TicksPerVBL

  TicksPerVBL := 1;
  for iVV := 0 to VirtualVoices.Count - 1 do
    TicksPerVBL := Max(TicksPerVBL, VirtualVoices[ivv].PatchRef.TicksPerVBL);

  Result.FrameRate := CVBLPerSecond * TicksPerVBL;


end;

end.

