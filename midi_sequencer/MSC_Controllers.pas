unit MSC_Controllers;

{
   Copyright 1998-2011 Arnold Reinders

   This file is part of the MIDI Sequencer Components (MSC).

   MIDI Sequencer Components (MSC) is free software: you can redistribute it
   and/or modify it under the terms of the Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the License,
   or (at your option) any later version.

   MIDI Sequencer Components (MSC) is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License and the
   Lesser General Public License along with MIDI Sequencer Components (MSC).
   If not, see <http://www.gnu.org/licenses/>.

   Any changes in the MIDI Sequencer Components (MSC) should be reported
   to Arnold Reinders at musoft@musoft.com
}

interface

uses Classes, SysUtils, AnsiStrings,
     MSC_Definitions;

type
   TController_Type  = (ct14bit, ct7bit, ctBinary{, ctNone});

   EMSC_Controller_Error = class (EMSC_Error);
{
   TMIDI_Controller is a wrapper around MIDI controllers.
   A MIDI controller may be 7 or 14 bit. A 14 bit controller has an
   additional controller associated with it. The controller ID is kept
   in Fcnt_ID, its associated controller in Faux_ID.
   Fcnt_ID > Faux_ID: Fcnt_ID = MSB, FAux_ID = LSB
   Both controllers contain the 14bit value which is identical
}

   TMIDI_Controller = class (TObject)
   private
      Fcnt_ID: T7bit;
      Faux_ID: uInt8;
      Fcnt_name: AnsiString;
      Fcnt_type: TController_Type;
      FValue: T14Bit;

      function  get_max: Int32;
      function  get_value: T14Bit;
      procedure set_value (value: T14Bit);

   public
      constructor Create (ctype: TController_Type; id1: T7bit; id2: uInt8 = 0;
                          cname: AnsiString = '');
      destructor Destroy; override;
      function create_event (channel: TMIDI_Range; value: T7bit): TMIDI_Event;
      procedure create_events (channel: TMIDI_Range; var ev1, ev2: TMIDI_Event);

      property cnt_ID: T7bit read Fcnt_ID;
      property Aux_ID: uInt8 read Faux_ID;
      property cnt_Name: AnsiString read Fcnt_Name;
      property cnt_Type: TController_Type read Fcnt_Type;
      property Max: Int32 read get_max;
      property value: T14Bit read get_value write set_value;
   end; // Class: TMIDI_Controller //

   TController_Holder = class (TObject)
   protected
      FControllers: array [T7Bit] of TMIDI_Controller;

      function  get_controller (index: T7bit): TMIDI_Controller;
      function  get_name (index: T7bit): AnsiString;
      function  get_ctrl_by_name (index: AnsiString): Int32;
      function  get_value (index: T7Bit): T14Bit;
      procedure set_value (index: T7Bit; value: T14Bit);
      function  get_byte_val (index: T7Bit): uInt8;

   public
      constructor Create;
      destructor Destroy; override;

      property Controller [index: T7bit]: TMIDI_Controller read get_controller; default;
      property Ctrl_Name  [index: T7bit]: AnsiString read get_name;
      property Ctrl_Idx   [index: AnsiString]: Int32 read get_ctrl_by_name;
      property Value      [index: T7bit]: T14Bit read get_value write set_value;
      property Byte_Val   [index: T7Bit]: uInt8 read get_byte_val;
   end; // Class: TController_Holder //

implementation

//{$R Controllers.res}

{*******************************************************************
*                                                                  *
* Class TMIDI_Controller                                           *
*                                                                  *
********************************************************************}

constructor TMIDI_Controller.Create
(
   ctype: TController_Type;
   id1: T7bit;
   id2: uInt8;
   cname: AnsiString
);
begin
   inherited Create;

   Fcnt_ID    := id1;
   FAux_ID    := id2;
   Fcnt_Type  := ctype;
   Fcnt_Name  := cname;
end; // set_ctype //

destructor TMIDI_Controller.Destroy;
begin
   inherited Destroy;
end; // Destroy //

function TMIDI_Controller.get_max: Int32;
begin
   if cnt_Type = ct14Bit then get_max := High (T14Bit)
                         else get_max := High (T7bit);
end; // get_max //

function TMIDI_Controller.create_event
(
   channel: TMIDI_Range; // Channel number to send message to
   value: T7bit          // Value of the controller
): TMIDI_Event;
// Sends a controller message to the MIDI channel of the MIDI port
var event: TMIDI_Event;
begin
   if (value < 0) or (value >= Max)
      then raise EMSC_Controller_Error.CreateFmt ('Controller (%d) value (%d) out ' +
                                                  'of range [0..%d]', [Fcnt_ID, value, Max]);
   event := TMIDI_Event.Create;
   event.Channel    := channel;
   event.Event_Type := mc_MIDI_Control_Change;
   event.Data_Byte_1 := cnt_ID;
   case cnt_Type of
      ct14bit: event.Data_Byte_2 := value;
      ct7bit:  event.Data_Byte_2 := value;
      ctBinary: if value <= C7bit div 2
                   then event.Data_Byte_2 := 0
                   else event.Data_Byte_2 := 127;
   end; // case
   create_event := event;
end; // send_message //

procedure TMIDI_Controller.create_events (channel: TMIDI_Range; var ev1, ev2: TMIDI_Event);
begin
   if cnt_Type <> ct14bit then
   begin
      ev1 := create_event (channel, Self.Value);
      ev2 := nil;
   end else
   begin
      if FCnt_ID < FAux_ID then
      begin
         ev1 := create_event (channel, Self.value div C7bit);
         ev2 := create_event (channel, Self.value mod C7bit);
      end else
      begin
         ev1 := create_event (channel, Self.value mod C7bit);
         ev2 := create_event (channel, Self.value div C7bit);
      end;
   end;
end; // create_events //

function TMIDI_Controller.get_value: T14Bit;
begin
   get_value := FValue;
end; // get_value //

procedure TMIDI_Controller.set_value (value: T14Bit);
begin
   if (value > High (T7Bit)) and (cnt_Type <> ct14Bit)
      then raise EMSC_Controller_Error.CreateFmt
            ('Controller %d was set to value %d while not ' +
             'a 14 bit controller. Maximum allowed is 127', [Fcnt_ID, value]);
   FValue := value;
end; // set_value //

{*******************************************************************
*                                                                  *
* Class TController_Holder                                         *
*                                                                  *
********************************************************************}

constructor TController_Holder.Create;
begin
   inherited Create;

   FControllers [  0] := TMIDI_Controller.Create (ct14Bit,    0,  32, 'Continuous Controller #0');
   FControllers [  1] := TMIDI_Controller.Create (ct14Bit,    1,  33, 'Modulation');
   FControllers [  2] := TMIDI_Controller.Create (ct14Bit,    2,  34, 'Breath Control');
   FControllers [  3] := TMIDI_Controller.Create (ct14Bit,    3,  35, 'Continuous Controller #3');
   FControllers [  4] := TMIDI_Controller.Create (ct14Bit,    4,  36, 'Foot Controller');
   FControllers [  5] := TMIDI_Controller.Create (ct14Bit,    5,  37, 'Portamento Time');
   FControllers [  6] := TMIDI_Controller.Create (ct14Bit,    6,  38, 'Data Entry');
   FControllers [  7] := TMIDI_Controller.Create (ct14Bit,    7,  39, 'Volume');
   FControllers [  8] := TMIDI_Controller.Create (ct14Bit,    8,  40, 'Continuous Controller #08');
   FControllers [  9] := TMIDI_Controller.Create (ct14Bit,    9,  41, 'Continuous Controller #09');
   FControllers [ 10] := TMIDI_Controller.Create (ct14Bit,   10,  42, 'Panpot');
   FControllers [ 11] := TMIDI_Controller.Create (ct14Bit,   11,  43, 'Expression');
   FControllers [ 12] := TMIDI_Controller.Create (ct14Bit,   12,  44, 'Continuous Controller #12');
   FControllers [ 13] := TMIDI_Controller.Create (ct14Bit,   13,  45, 'Continuous Controller #13');
   FControllers [ 14] := TMIDI_Controller.Create (ct14Bit,   14,  46, 'continuous Controller #14');
   FControllers [ 15] := TMIDI_Controller.Create (ct14Bit,   15,  47, 'Continuous Controller #15');
   FControllers [ 16] := TMIDI_Controller.Create (ct14Bit,   16,  48, 'Continuous Controller #16');
   FControllers [ 17] := TMIDI_Controller.Create (ct14Bit,   17,  49, 'Continuous Controller #17');
   FControllers [ 18] := TMIDI_Controller.Create (ct14Bit,   18,  50, 'Continuous Controller #18');
   FControllers [ 19] := TMIDI_Controller.Create (ct14Bit,   19,  51, 'Continuous Controller #19');
   FControllers [ 20] := TMIDI_Controller.Create (ct14Bit,   20,  52, 'Continuous Controller #20');
   FControllers [ 21] := TMIDI_Controller.Create (ct14Bit,   21,  53, 'Continuous Controller #21');
   FControllers [ 22] := TMIDI_Controller.Create (ct14Bit,   22,  54, 'Continuous Controller #22');
   FControllers [ 23] := TMIDI_Controller.Create (ct14Bit,   23,  55, 'Continuous Controller #23');
   FControllers [ 24] := TMIDI_Controller.Create (ct14Bit,   24,  56, 'Continuous Controller #24');
   FControllers [ 25] := TMIDI_Controller.Create (ct14Bit,   25,  57, 'Continuous Controller #25');
   FControllers [ 26] := TMIDI_Controller.Create (ct14Bit,   26,  58, 'Continuous Controller #26');
   FControllers [ 27] := TMIDI_Controller.Create (ct14Bit,   27,  59, 'Continuous Controller #27');
   FControllers [ 28] := TMIDI_Controller.Create (ct14Bit,   28,  60, 'Continuous Controller #28');
   FControllers [ 29] := TMIDI_Controller.Create (ct14Bit,   29,  61, 'Continuous Controller #29');
   FControllers [ 30] := TMIDI_Controller.Create (ct14Bit,   30,  62, 'Continuous Controller #30');
   FControllers [ 31] := TMIDI_Controller.Create (ct14Bit,   31,  63, 'Continuous Controller #31');
   FControllers [ 32] := TMIDI_Controller.Create (ct14Bit,   32,   0, 'Continuous Controller #0 (lsb)');
   FControllers [ 33] := TMIDI_Controller.Create (ct14Bit,   33,   1, 'Modulation (lsb)');
   FControllers [ 34] := TMIDI_Controller.Create (ct14Bit,   34,   2, 'Breath Control (lsb)');
   FControllers [ 35] := TMIDI_Controller.Create (ct14Bit,   35,   3, 'Continuous Controller #3 (lsb)');
   FControllers [ 36] := TMIDI_Controller.Create (ct14Bit,   36,   4, 'Foot Controller (lsb)');
   FControllers [ 37] := TMIDI_Controller.Create (ct14Bit,   37,   5, 'Portamento Time (lsb)');
   FControllers [ 38] := TMIDI_Controller.Create (ct14Bit,   38,   6, 'Data Entry (lsb)');
   FControllers [ 39] := TMIDI_Controller.Create (ct14Bit,   39,   7, 'Volume (lsb)');
   FControllers [ 40] := TMIDI_Controller.Create (ct14Bit,   40,   8, 'Continuous Controller #08 (lsb)');
   FControllers [ 41] := TMIDI_Controller.Create (ct14Bit,   41,   9, 'Continuous Controller #09 (lsb)');
   FControllers [ 42] := TMIDI_Controller.Create (ct14Bit,   42,  10, 'Panpot (lsb)');
   FControllers [ 43] := TMIDI_Controller.Create (ct14Bit,   43,  11, 'Expression (lsb)');
   FControllers [ 44] := TMIDI_Controller.Create (ct14Bit,   44,  12, 'Continuous Controller #12 (lsb)');
   FControllers [ 45] := TMIDI_Controller.Create (ct14Bit,   45,  13, 'Continuous Controller #13 (lsb)');
   FControllers [ 46] := TMIDI_Controller.Create (ct14Bit,   46,  14, 'continuous Controller #14 (lsb)');
   FControllers [ 47] := TMIDI_Controller.Create (ct14Bit,   47,  15, 'Continuous Controller #15 (lsb)');
   FControllers [ 48] := TMIDI_Controller.Create (ct14Bit,   48,  16, 'Continuous Controller #16 (lsb)');
   FControllers [ 49] := TMIDI_Controller.Create (ct14Bit,   49,  17, 'Continuous Controller #17 (lsb)');
   FControllers [ 50] := TMIDI_Controller.Create (ct14Bit,   50,  18, 'Continuous Controller #18 (lsb)');
   FControllers [ 51] := TMIDI_Controller.Create (ct14Bit,   51,  19, 'Continuous Controller #19 (lsb)');
   FControllers [ 52] := TMIDI_Controller.Create (ct14Bit,   52,  20, 'Continuous Controller #20 (lsb)');
   FControllers [ 53] := TMIDI_Controller.Create (ct14Bit,   53,  21, 'Continuous Controller #21 (lsb)');
   FControllers [ 54] := TMIDI_Controller.Create (ct14Bit,   54,  22, 'Continuous Controller #22 (lsb)');
   FControllers [ 55] := TMIDI_Controller.Create (ct14Bit,   55,  23, 'Continuous Controller #23 (lsb)');
   FControllers [ 56] := TMIDI_Controller.Create (ct14Bit,   56,  24, 'Continuous Controller #24 (lsb)');
   FControllers [ 57] := TMIDI_Controller.Create (ct14Bit,   57,  25, 'Continuous Controller #25 (lsb)');
   FControllers [ 58] := TMIDI_Controller.Create (ct14Bit,   58,  26, 'Continuous Controller #26 (lsb)');
   FControllers [ 59] := TMIDI_Controller.Create (ct14Bit,   59,  27, 'Continuous Controller #27 (lsb)');
   FControllers [ 60] := TMIDI_Controller.Create (ct14Bit,   60,  28, 'Continuous Controller #28 (lsb)');
   FControllers [ 61] := TMIDI_Controller.Create (ct14Bit,   61,  29, 'Continuous Controller #29 (lsb)');
   FControllers [ 62] := TMIDI_Controller.Create (ct14Bit,   62,  30, 'Continuous Controller #30 (lsb)');
   FControllers [ 63] := TMIDI_Controller.Create (ct14Bit,   63,  31, 'Continuous Controller #31 (lsb)');
   FControllers [ 64] := TMIDI_Controller.Create (ctBinary,  64,   0, 'Damper Pedal');
   FControllers [ 65] := TMIDI_Controller.Create (ctBinary,  65,   0, 'Portamento');
   FControllers [ 66] := TMIDI_Controller.Create (ctBinary,  66,   0, 'Sostenuto');
   FControllers [ 67] := TMIDI_Controller.Create (ctBinary,  67,   0, 'Soft Pedal');
   FControllers [ 68] := TMIDI_Controller.Create (ctBinary,  68,   0, 'General 4');
   FControllers [ 69] := TMIDI_Controller.Create (ctBinary,  69,   0, 'Hold 2');
   FControllers [ 70] := TMIDI_Controller.Create (ctBinary,  70,   0, 'Undefined on/off');
   FControllers [ 71] := TMIDI_Controller.Create (ctBinary,  71,   0, 'Undefined on/off');
   FControllers [ 72] := TMIDI_Controller.Create (ctBinary,  72,   0, 'Undefined on/off');
   FControllers [ 73] := TMIDI_Controller.Create (ctBinary,  73,   0, 'Undefined on/off');
   FControllers [ 74] := TMIDI_Controller.Create (ctBinary,  74,   0, 'Undefined on/off');
   FControllers [ 75] := TMIDI_Controller.Create (ctBinary,  75,   0, 'Undefined on/off');
   FControllers [ 76] := TMIDI_Controller.Create (ctBinary,  76,   0, 'Undefined on/off');
   FControllers [ 77] := TMIDI_Controller.Create (ctBinary,  77,   0, 'Undefined on/off');
   FControllers [ 78] := TMIDI_Controller.Create (ctBinary,  78,   0, 'Undefined on/off');
   FControllers [ 79] := TMIDI_Controller.Create (ctBinary,  79,   0, 'Undefined on/off');
   FControllers [ 80] := TMIDI_Controller.Create (ct14Bit,   80,  86, 'General Purpose Controller #5');
   FControllers [ 81] := TMIDI_Controller.Create (ct14Bit,   81,  87, 'General Purpose Controller #6');
   FControllers [ 82] := TMIDI_Controller.Create (ct14Bit,   82,  88, 'General Purpose Controller #7');
   FControllers [ 83] := TMIDI_Controller.Create (ct14Bit,   83,  89, 'General Purpose Controller #8');
   FControllers [ 84] := TMIDI_Controller.Create (ctBinary,  84,   0, 'Undefined on/off');
   FControllers [ 85] := TMIDI_Controller.Create (ctBinary,  85,   0, 'Undefined on/off');
   FControllers [ 86] := TMIDI_Controller.Create (ct14Bit,   86,  80, 'General Purpose Controller #5 (lsb)');
   FControllers [ 87] := TMIDI_Controller.Create (ct14Bit,   87,  81, 'General Purpose Controller #6 (lsb)');
   FControllers [ 88] := TMIDI_Controller.Create (ct14Bit,   88,  82, 'General Purpose Controller #7 (lsb)');
   FControllers [ 89] := TMIDI_Controller.Create (ct14Bit,   89,  83, 'General Purpose Controller #8 (lsb)');
   FControllers [ 90] := TMIDI_Controller.Create (ctBinary,  90,   0, 'Undefined on/off');
   FControllers [ 91] := TMIDI_Controller.Create (ct7Bit,    91,   0, 'Reverb Depth');
   FControllers [ 92] := TMIDI_Controller.Create (ct7Bit,    92,   0, 'Tremolo Depth');
   FControllers [ 93] := TMIDI_Controller.Create (ct7Bit,    93,   0, 'Chorus Depth');
   FControllers [ 94] := TMIDI_Controller.Create (ct7Bit,    94,   0, 'Detune');
   FControllers [ 95] := TMIDI_Controller.Create (ct7Bit,    95,   0, 'Phaser Depth');
   FControllers [ 96] := TMIDI_Controller.Create (ct7Bit,    96,   0, 'Data Inc');
   FControllers [ 97] := TMIDI_Controller.Create (ct7Bit,    97,   0, 'Data Dec');
   FControllers [ 98] := TMIDI_Controller.Create (ct14Bit,   98,  99, 'Non Registered Parameter');
   FControllers [ 99] := TMIDI_Controller.Create (ct14Bit,   99,  98, 'Non Registered Parameter (lsb)');
   FControllers [100] := TMIDI_Controller.Create (ct14Bit,  100, 101, 'Registered Parameter');
   FControllers [101] := TMIDI_Controller.Create (ct14Bit,  101, 100, 'Registered Parameter (lsb)');
   FControllers [102] := TMIDI_Controller.Create (ct7Bit,   102,   0, 'Undefined');
   FControllers [103] := TMIDI_Controller.Create (ct7Bit,   103,   0, 'Undefined');
   FControllers [104] := TMIDI_Controller.Create (ct7Bit,   104,   0, 'Undefined');
   FControllers [105] := TMIDI_Controller.Create (ct7Bit,   105,   0, 'Undefined');
   FControllers [106] := TMIDI_Controller.Create (ct7Bit,   106,   0, 'Undefined');
   FControllers [107] := TMIDI_Controller.Create (ct7Bit,   107,   0, 'Undefined');
   FControllers [108] := TMIDI_Controller.Create (ct7Bit,   108,   0, 'Undefined');
   FControllers [109] := TMIDI_Controller.Create (ct7Bit,   109,   0, 'Undefined');
   FControllers [110] := TMIDI_Controller.Create (ct7Bit,   110,   0, 'Undefined');
   FControllers [111] := TMIDI_Controller.Create (ct7Bit,   111,   0, 'Undefined');
   FControllers [112] := TMIDI_Controller.Create (ct7Bit,   112,   0, 'Undefined');
   FControllers [113] := TMIDI_Controller.Create (ct7Bit,   113,   0, 'Undefined');
   FControllers [114] := TMIDI_Controller.Create (ct7Bit,   114,   0, 'Undefined');
   FControllers [115] := TMIDI_Controller.Create (ct7Bit,   115,   0, 'Undefined');
   FControllers [116] := TMIDI_Controller.Create (ct7Bit,   116,   0, 'Undefined');
   FControllers [117] := TMIDI_Controller.Create (ct7Bit,   117,   0, 'Undefined');
   FControllers [118] := TMIDI_Controller.Create (ct7Bit,   118,   0, 'Undefined');
   FControllers [119] := TMIDI_Controller.Create (ct7Bit,   119,   0, 'Undefined');
   FControllers [120] := TMIDI_Controller.Create (ct7Bit,   120,   0, 'Undefined');
   FControllers [121] := TMIDI_Controller.Create (ct7Bit,   121,   0, 'Reset All Controllers');
   FControllers [122] := TMIDI_Controller.Create (ct7Bit,   122,   0, 'Local Control on/off');
   FControllers [123] := TMIDI_Controller.Create (ct7Bit,   123,   0, 'All Notes Off');
   FControllers [124] := TMIDI_Controller.Create (ct7Bit,   124,   0, 'Omni Off');
   FControllers [125] := TMIDI_Controller.Create (ct7Bit,   125,   0, 'Omni On');
   FControllers [126] := TMIDI_Controller.Create (ct7Bit,   126,   0, 'Mono');
   FControllers [127] := TMIDI_Controller.Create (ct7Bit,   127,   0, 'Poly');
end; // Create //

destructor TController_Holder.Destroy;
var i: T7bit;
begin
   for i := Low (T7bit) to High (T7bit) do FControllers [i].Free;

   inherited Destroy;
end; // Destroy //

function TController_Holder.get_controller (index: T7bit): TMIDI_Controller;
begin
   get_controller := FControllers [index];
end; // get_controller //

function TController_Holder.get_name (index: T7bit): AnsiString;
begin
   get_name := FControllers [index].cnt_Name;
end; // get_name //

function TController_Holder.get_ctrl_by_name (index: AnsiString): Int32;
var idx: Int32;
begin
   Result := -1;
   idx := 0;
   while (Result = -1) and (idx < c7Bit) do
   begin
      if FControllers [idx].cnt_Name = index
         then Result := idx;
      idx := idx + 1;
   end; // if
end; // get_ctrl_by_name //

function TController_Holder.get_value (index: T7Bit): T14Bit;
var cnt: TMIDI_Controller;
begin
   cnt := Controller [index];
   get_value := cnt.value;
   if (cnt.cnt_Type = ct14Bit) and (cnt.cnt_ID > cnt.aux_ID)
         then get_value := Controller [cnt.Aux_ID].get_value;
end; // get_value //

procedure TController_Holder.set_value (index: T7Bit; value: T14Bit);
var cnt: TMIDI_Controller;
begin
   cnt := Controller [index];
   cnt.value := value;
   if cnt.cnt_Type = ct14Bit
      then Controller [cnt.Aux_ID].Value := value;
end; // set_value //

function  TController_Holder.get_byte_val (index: T7Bit): uInt8;
var cnt: TMIDI_Controller;
begin
   cnt := Controller [index];
   if cnt.cnt_Type = ct14Bit then
   begin
      if cnt.Cnt_ID < cnt.Aux_ID
         then get_byte_val := Controller [index].Value div C7Bit
         else get_byte_val := Controller [index].Value mod C7Bit;
   end else
   begin
      get_byte_val := cnt.Value;
   end; // if
end; // get_byte_val //

end. // Unit: MSC_Controllers //
