unit Demo_Controller_View;

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
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls,
  MSC_Definitions, MSC_Controllers;

type
   TView_Columns = (vcID, vcAux, vcNames, vcResolution);

const
   vcHeaders: array [TView_Columns] of string = ('ID', 'Aux', 'Name', 'Resolution');
   vcWidths: array [TView_Columns] of integer = (40, 40, 200, 60);

type
   TDemo_Controller_View = class (TListView)
   private
      FHolder: TController_Holder;

   protected

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Show;

   published
      property Holder: TController_Holder read FHolder write FHolder;
   end; // Class: TDemo_Controller_View //

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents(MSC_Package_Name, [TDemo_Controller_View]);
end;

constructor TDemo_Controller_View.Create (AOwner: TComponent);
var col: TListColumn;
    c: TView_Columns;
begin
   inherited Create (AOwner);

   ViewStyle     := vsReport;
   ReadOnly      := True;
   GridLines     := True;
   RowSelect     := True;
   HideSelection := False;

   for c := Low (TView_Columns) to High (TView_Columns) do
   begin
      col := Columns.Add;
      col.Caption := vcHeaders [c];
      col.Width   := vcWidths  [c];
   end; // for
   Holder := nil;
end; // Create //

destructor TDemo_Controller_View.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TDemo_Controller_View.Show;
var item: TListItem;
    ctl: TMIDI_Controller;
    i: T7bit;
begin
   if Holder = nil then Exit;
   Items.BeginUpdate;
   for i := Low (T7bit) to High (T7bit) do
   begin
      ctl  := Holder.Controller [i];
      item := Items.Add;
      item.Caption := Format ('%2.2x', [ctl.Cnt_ID]);
      item.SubItems.Add (Format ('%2.2x', [ctl.Aux_ID]));
      item.SubItems.Add (WideString (ctl.cnt_Name));
      item.SubItems.Add (IntToStr (ctl.Max));
   end; // for
   Items.EndUpdate;
end; // Show //

end. // Unit: Demo_Controller_View //
