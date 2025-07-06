unit MSC_Out_Resources_View;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, ComCtrls,
  MSC_Definitions, MSC_Out_Device, MSC_Out_Resources;

type
   TMIDI_Resources_Out_View = class (TForm)
      Button_Close: TButton;
      List_MIDI_Out: TListView;
      Image_List: TImageList;
      Sound_Box: TGroupBox;
      Label1: TLabel;
      Label2: TLabel;
      Label3: TLabel;
      Label4: TLabel;
      Label_Version: TLabel;
      Label_Technology: TLabel;
      Label_Voices: TLabel;
      Label_Notes: TLabel;
      Label5: TLabel;
      Label_Allocated: TLabel;
      Label7: TLabel;
      Label_MIDI_Out: TLabel;
      View_Ports: TListView;
      Label_List_Ports_Tracks: TLabel;

      procedure List_MIDI_OutMouseDown (Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);

   private
      FMIDI_Res_Out: TMIDI_Resources_Out; // Ref to MIDI resources
      MIDI_Out: integer;     // MIDI output port
      enabled : boolean;     // True if some sound can be played, else false

      function  get_MIDI_res_out: TMIDI_Resources_Out;
      procedure set_MIDI_res_out (mr: TMIDI_Resources_Out);
      function  n_devices: integer;
      procedure list_device (device: smallint);
      procedure show_ports;
      function  is_enabled: boolean;
      procedure port_disable (port: integer);
      procedure port_play (port: integer);
      function  port_exclusive_play (port: integer): boolean;
      procedure port_mute (port: integer);
      function  get_device_name (device: integer): string;
      procedure Check_MIDI_Out (X, Y: integer);
      procedure Check_Allocate (X, Y: integer);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;

      property MIDI_Res_Out: TMIDI_Resources_Out read  get_MIDI_res_out
                                                 write set_MIDI_res_out;
   end; // Class: TMIDI_Resources_Out_View //

implementation

{$R *.DFM}

constructor TMIDI_Resources_Out_View.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

   FMIDI_Res_Out := nil;
   Label_Version   .Caption := '';
   Label_Technology.Caption := '';
   Label_Voices    .Caption := '';
   Label_Notes     .Caption := '';
   Label_Allocated .Caption := '';
   Label_MIDI_Out  .Caption := '';
end; // FormCreate //

destructor TMIDI_Resources_Out_View.Destroy;
begin
   inherited destroy;
end; // Destroy //

function TMIDI_Resources_Out_View.get_MIDI_res_out: TMIDI_Resources_Out;
begin
   get_MIDI_res_out := FMIDI_Res_Out;
end; // get_MIDI_res_out //

procedure TMIDI_Resources_Out_View.set_MIDI_res_out (mr: TMIDI_Resources_Out);
begin
   FMIDI_Res_Out := mr;
   if FMIDI_Res_Out <> nil then
   begin
      show_ports;
      if n_devices > 0 then list_device (0);
   end; // if
end; // set_MIDI_res_out //

procedure TMIDI_Resources_Out_View.List_MIDI_OutMouseDown
(
   Sender: TObject;
   Button: TMouseButton;
   Shift: TShiftState;
   X, Y: Integer
);
var item: TListItem;
begin
   item := List_MIDI_Out.GetItemAt (X, Y);
   if item = nil then Exit;
   if Button = mbLeft then
   begin
      List_Device (item.Index);
      List_MIDI_Out.Selected := item;
      if List_MIDI_Out.GetHitTestInfoAt (X, Y) = [htOnStateIcon] then Check_Allocate (X, Y) else
      if List_MIDI_Out.GetHitTestInfoAt (X, Y) = [htOnIcon]      then Check_MIDI_Out (X, Y);
   end; // if
   show_ports;
end; // List_MIDI_OutMouseDown //

procedure TMIDI_Resources_Out_View.list_device (device: smallint);
// Show capabilities of requested MIDI port
var item: TListItem;
begin
   if (device >= 0) and (device < n_devices) then
   begin
      Sound_Box.Enabled := true;
      Sound_Box.Caption := MIDI_Res_Out.Port_Out [device].MIDI_Out.ProductName;
      Label_Version.Caption := inttostr (MIDI_Res_Out.Port_Out [device].MIDI_Out.DriverVersion);
      case MIDI_Res_Out.Port_Out [device].MIDI_Out.Technology of
        	opt_None:     Label_Technology.Caption := 'None';
  	      opt_MidiPort: Label_Technology.Caption := 'MIDI Port';
  	      opt_Synth:    Label_Technology.Caption := 'Generic Synth';
  	      opt_SQSynth:  Label_Technology.Caption := 'Square Wave Synth';
  	      opt_FMSynth:  Label_Technology.Caption := 'FM Synth';
  	      opt_Mapper:   Label_Technology.Caption := 'MIDI Mapper';
         else          Label_Technology.Caption := 'Unknown';
      end; // case
      Label_Voices.Caption := inttostr (MIDI_Res_Out.Port_Out [device].MIDI_Out.Voices);
      Label_Notes .Caption := inttostr (MIDI_Res_Out.Port_Out [device].MIDI_Out.Notes);
      item := List_MIDI_Out.Items [device];
      if MIDI_Res_Out.Port_Out [device].MIDI_Out.DeviceID = MIDI_Out then
      begin
         Label_MIDI_Out.Caption := 'Yes';
         item.ImageIndex := 1;
      end else
      begin
         Label_MIDI_Out.Caption := 'No';
         item.ImageIndex := 0;
      end; // if
      Application.ProcessMessages;
      if MIDI_Res_Out.Port_Out [device].MIDI_Out.Status = mosOpen then
      begin
         Label_Allocated.Caption := 'Yes';
         item.StateIndex := 2;
      end else
      begin
         Label_Allocated.Caption := 'No';
         item.StateIndex := 0;
      end; // if
   end else
   begin
      Sound_Box.Enabled := false;
   end; // if
end; // list_device //

procedure TMIDI_Resources_Out_View.show_ports;
var column: TListColumn;
    item: TListItem;
    port: integer;
    track: TTrack_Range;
begin
// Show all device in the List_MIDI_Out view
   if MIDI_Res_Out = nil then
   begin
      Label_List_Ports_Tracks.Caption := 'No assignment made';
      Exit;
   end; // if
   List_MIDI_Out.Items.BeginUpdate;
   List_MIDI_Out.Items.Clear;
   port := 0;
   while port < n_devices do
   begin
      item := List_MIDI_Out.Items.Add;
      Item.Caption := get_device_name (port);
      list_device (port);
      port := port + 1;
   end; // while
   List_MIDI_Out.Items.EndUpdate;

// Show detailed status for each track and each port in the View_Ports view.
// Each element in the crosstab can be Disabled, Mute or Play (enabled)
   View_Ports.Items.BeginUpdate;
   View_Ports.Items.Clear;
   View_Ports.Columns.Clear;
   Column := View_Ports.Columns.Add;
   Column.Caption := 'Channel';
   port := 0;
   while port < n_devices do
   begin
      Column := View_Ports.Columns.Add;
      Column.Caption := get_device_name (port);
      port := port + 1;
   end; // while
   for track := Low (TTrack_Range) to High (TTrack_Range) do
   begin
      item := View_Ports.Items.Add;
      item.Caption := IntToStr (track);
      port := 0;
      while port < n_devices do
      begin
         case MIDI_Res_Out [track, port] of
            pcDisabled: item.SubItems.Add ('Disabled');
            pcMute:     item.SubItems.Add ('Mute');
            pcPlay:     item.SubItems.Add ('Play');
         end; // for
         port := port + 1;
      end; // while
   end; // for
   View_Ports.Items.EndUpdate;
end; // show_ports //

function TMIDI_Resources_Out_View.n_devices: integer;
// Returns number of devices
begin
   if MIDI_Res_Out = nil
      then n_devices := -1
      else n_devices := MIDI_Res_Out.Count;
end; // n_devices //

function TMIDI_Resources_Out_View.is_enabled: boolean;
begin
   is_enabled := enabled and (n_devices > 0);
end; // is_enabled //

procedure TMIDI_Resources_Out_View.port_disable (port: integer);
begin
   if MIDI_Res_Out <> nil then MIDI_Res_Out.port_disable (port);
end; // port_disable //

procedure TMIDI_Resources_Out_View.port_play (port: integer);
begin
   if MIDI_Res_Out <> nil then MIDI_Res_Out.port_Play (port);
end; // port_play //

function TMIDI_Resources_Out_View.port_exclusive_play (port: integer): boolean;
begin
   port_exclusive_play := False;
   if MIDI_Res_Out <> nil then port_exclusive_play := MIDI_Res_Out.port_exclusive_play (port);
end; // port_exclusive_play //

procedure TMIDI_Resources_Out_View.port_mute (port: integer);
begin
   if MIDI_Res_Out <> nil then MIDI_Res_Out.port_mute (port);
end; // port_mute //

function TMIDI_Resources_Out_View.get_device_name (device: integer): string;
// Returns name of indexed MIDI port
begin
   get_device_name := '';
   if (MIDI_Res_Out <> nil) and (device >= 0) and (device < n_devices)
      then get_device_name := MIDI_Res_Out.Port_Out [device].MIDI_Out.ProductName;
end; // get_device_name //

procedure TMIDI_Resources_Out_View.Check_MIDI_Out (X, Y: integer);
var item: TListItem;
    device: integer;
begin
   item := List_MIDI_Out.GetItemAt (X, Y);
   if item <> nil then
   begin
      device := 0;
      while device < n_devices do
      begin
         List_MIDI_Out.Items [device].ImageIndex := 0;
         device := device + 1;
      end; // while
      List_MIDI_Out.SetFocus;
      List_MIDI_Out.Selected := item;
      if item.Index <> MIDI_Out then
      begin
         MIDI_Out := item.Index;
         List_MIDI_Out.Items [MIDI_Out].ImageIndex := 2;
      end else
      begin
         MIDI_Out := no_ports_available;
      end; // if
      list_device (item.Index);
   end; // if
end; // Check_MIDI_Out //

procedure TMIDI_Resources_Out_View.Check_Allocate (X, Y: integer);
var device: integer;
    item: TListItem;
begin
   item := List_MIDI_Out.GetItemAt (X, Y);
   if (MIDI_Res_Out <> nil) and (item <> nil) then
   begin
      List_MIDI_Out.SetFocus;
      device := Item.Index;
      List_MIDI_Out.Selected := item;
      if MIDI_Res_Out.Port_Out [device].MIDI_Out.Status <> mosOpen then
      begin
         try
            MIDI_Res_Out.Port_Out [device].MIDI_Out.Open;
            port_play (device);
         except
            on e:exception do
            begin
               port_disable (device);
               ShowMessage ('TMIDI_Resources_Out_View.Check_AllocateClick: exception while trying to open ' +
                            'device: ' + inttostr (device) + ': ' + e.Message);
            end; // on
         end; // try
      end else
      begin
         port_disable (device);
         try
            MIDI_Res_Out.Port_Out [device].MIDI_Out.Close;
         except
            on e:exception do
            begin
               ShowMessage ('TMIDI_Resources_Out_View.Check_AllocateClick: exception while trying to close ' +
                            'device: ' + inttostr (device) + ': ' + e.Message);
            end; // on
         end; // try
      end; // if
//      list_device (device);
   end; // if
   show_ports;
end; // Check_Allocate //

{
procedure TMIDI_Resources_Out_View.set_port (ports: string);
var track: TTrack_Range;
    port: integer;
    pc: TPort_Capability;
    temp: integer;
    s: string;
begin
   s := get_item (ports, ':');
   try
      track := StrToInt (s);
      port := 0;
      while (ports <> '') and (port < n_devices) do
      begin
         s := get_item (ports, ' ');
         try
            temp := StrToInt (s);
            pc := TPort_Capability (temp);
            if (get_port_capability (track, port) <> pcDisabled) and (pc = pcPlay)
               then set_port_capability (track, port, pc);
         except
            // nothing
         end; // try
         port := Succ (port);
      end; // while
   except
      // nothing
   end; // try
end; // set_ports //
}
end. // Unit: TMIDI_Resources_Out_View //
