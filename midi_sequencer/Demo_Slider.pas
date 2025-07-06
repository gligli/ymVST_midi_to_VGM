unit Demo_Slider;

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

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, Buttons, ExtCtrls,
  MSC_Definitions, MSC_Controllers;

type
   TComponent_Slider = class (TFrame)
      Panel1: TPanel;
      Label1: TLabel;
      Combo_Channel: TComboBox;
      Edit_Value: TEdit;
      Label2: TLabel;
      Tracker: TTrackBar;
      Label_Min: TLabel;
      Label_Max: TLabel;
      Popup: TPopupMenu;
      Button_CTl_Names: TSpeedButton;
      Combo_Ctl: TComboBox;

      procedure TrackerChange(Sender: TObject);
      procedure Label_CaptionClick(Sender: TObject);
      procedure Combo_CtlClick(Sender: TObject);

   private
      FCaption: string;
      FMin: integer;
      FMax: integer;
      FHolder: TController_Holder;
      FID: T7Bit;

      procedure set_caption (value: string);
      function  get_channel: TMIDI_Range;
      procedure set_channel (value: TMIDI_Range);
      function  get_value: integer;
      procedure set_value (value: integer);
      function  get_controller (index: T7bit): TMIDI_Controller;
      procedure set_holder (value: TController_Holder);
      procedure set_id (value: T7bit);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Clear;

      property Caption: string write set_caption;
      property Min: integer read FMin write FMin;
      property Max: integer read FMax write FMax;
      property channel: TMIDI_Range read get_channel write set_channel;
      property Value: integer read get_value write set_value;
      property Controller [index: T7bit]: TMIDI_Controller read get_controller;
      property Holder: TController_Holder read FHolder write set_holder;
      property ID: T7Bit read FID write set_id;
   end; // Class: TSlider //

   procedure Register;

implementation

{$R *.dfm}

procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TComponent_Slider]);
end; // Register //

{*******************************************************************
*                                                                  *
* Class TSlider                                                    *
*                                                                  *
********************************************************************}

constructor TComponent_Slider.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);
end; // Create //

destructor TComponent_Slider.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TComponent_Slider.Clear;
begin
end; // Clear //

procedure TComponent_Slider.set_caption (value: string);
begin
   FCaption := value;
end; // set_caption //

function TComponent_Slider.get_channel: TMIDI_Range;
begin
   get_channel := Combo_Channel.ItemIndex + 1;
end; // get_channel //

procedure TComponent_Slider.set_channel (value: TMIDI_Range);
begin
   Combo_Channel.ItemIndex := value - 1;
end; // set_channel //

function TComponent_Slider.get_value: integer;
begin
   get_value := Tracker.Position;
end; // get_value //

procedure TComponent_Slider.set_value (value: integer);
begin
   Tracker.Position := value;
end; // set_value //

function  TComponent_Slider.get_controller (index: T7bit): TMIDI_Controller;
begin
   get_controller := Holder.Controller [index];
end; // get_name //

procedure TComponent_Slider.set_holder (value: TController_Holder);
var ctl: TMIDI_Controller;
    i: T7bit;
begin
   FHolder := value;
   Combo_Ctl.Items.Clear;
   for i := Low (T7bit) to High (T7bit) do
   begin
      ctl := Holder.Controller [i];
      Combo_Ctl.Items.Add (Format ('%d %s', [ctl.cnt_ID, ctl.cnt_Name]));
   end; // for
   ID := 0;
end; // set_holder //

procedure TComponent_Slider.set_id (value: T7bit);
var ctl: TMIDI_Controller;
begin
   Combo_Ctl.ItemIndex := value;
   ctl := Holder.Controller [value];
   label_min.Caption := '0';
   label_max.Caption := Format ('%d', [ctl.Max]);
   Tracker.Max := ctl.Max;
   Button_Ctl_Names.Caption := Combo_Ctl.Items [combo_Ctl.ItemIndex];
end; // set_id //

procedure TComponent_Slider.TrackerChange (Sender: TObject);
begin
   Edit_Value.Text := Format ('%d', [Tracker.Position]);
end; // TrackerChange //

procedure TComponent_Slider.Label_CaptionClick (Sender: TObject);
begin
   Button_Ctl_Names.Visible := False;
   Combo_Ctl.Visible := True;
   Combo_Ctl.ItemIndex := 0;
end; // Label_CaptionClick //

procedure TComponent_Slider.Combo_CtlClick (Sender: TObject);
var p: integer;
    cap: string;
begin
   Combo_Ctl.Visible := False;
   Button_Ctl_Names.Visible := True;
   cap := Combo_Ctl.Items [combo_Ctl.ItemIndex];
   p := Pos (' ', cap);
   ID := StrToInt (Copy (cap, 1, p - 1));
end; // Combo_CtlClick //

end. // Unit: Demo_Slider //
