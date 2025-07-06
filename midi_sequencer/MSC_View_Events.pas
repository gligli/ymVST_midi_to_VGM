unit MSC_View_Events;

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

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ComCtrls,
     Publish_Subscribe, MSC_Definitions, MSC_Container, MSC_View_Base,
     utilities_logging;

type
   TView_Columns = (vcTime, vcTrack, vcChannel, vcEventType, vcDB1, vcDB2, vcText);

const
   vcHeaders: array [TView_Columns] of string = ('Time', 'Track', 'Channel', 'Event',
                                                 'Data 1', 'Data 2', 'Text');
   vcWidths: array [TView_Columns] of Int32 = (50, 40, 40, 50, 80, 40, 200);

type
   TView_Events = class (TView_Base)

   private
      Scrolling: boolean;        // No scrolling between beginupdate and endupdate
      block_insert: boolean;     // the whole MIDI container must be inserted when true
      View: TListView;           // Event viewer

      function get_event_pos (time: Int32): Int32;

   protected
      procedure CustomDrawItem (Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset; override;
      procedure Alert (Sender: Tobject); override;
      procedure BeginUpdate;
      procedure EndUpdate;
      procedure fill_list_item (item: TListItem; event: TMIDI_Event);

      procedure adjust_time (index: Int32);
   end; // Class: TDemo_Container_View //

implementation

{*******************************************************************
*                                                                  *
* Class TDemo_Container_View                                       *
*                                                                  *
********************************************************************}

constructor TView_Events.Create (AOwner: TComponent);
var col: TListColumn;
    c: TView_Columns;
begin
   inherited Create (AOwner);

   View := TListView.Create (Self);
   View.Parent           := Self;
   View.Align            := alClient;
   View.ViewStyle        := vsReport;
   View.ReadOnly         := True;
   View.GridLines        := True;
   View.RowSelect        := True;
   View.HideSelection    := False;
   View.OnCustomDrawItem := CustomDrawItem;
   block_insert          := False;

   for c := Low (TView_Columns) to High (TView_Columns) do
   begin
      col := View.Columns.Add;
      col.Caption := vcHeaders [c];
      col.Width   := vcWidths  [c];
   end; // for
end; // Create //

destructor TView_Events.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TView_Events.Reset;
begin
   View.Items.BeginUpdate;
   View.Clear;
   View.Items.EndUpdate;
end; // Reset //

procedure TView_Events.Alert (Sender: Tobject);

   procedure insert_container (container: TMIDI_Container);
   var item: TListItem;
       event: TMIDI_Event;
       i: Int32;
   begin
      for i := 0 to container.Count - 1 do
      begin
         event := container.Event [i];
         item  := View.Items.Insert (i);
         fill_list_item (item, event);
      end; // if
   end; // Execute //

   procedure begin_update;
   begin
      View.Items.BeginUpdate;
      Scrolling    := False;
      block_insert := True;
   end; // begin_update //

   procedure end_update (container: TMIDI_Container);
   begin
      if block_insert
         then insert_container (container);
      View.Items.EndUpdate;
      Scrolling    := True;
      block_insert := False;
   end; // end_update //

   procedure insert_event (container: TMIDI_Container);
   var item: TListItem;
       event: TMIDI_Event;
       index: Int32;
   begin
      index := container.Last_Index;
      event := container.Event [index];
      if event <> nil then
      begin
         item := View.Items.Insert (index);
         log_write_fmt ('[viewer]%12d %4d %2x %3d %3d', [gettickcount, event.Time, event.MIDI_Message, event.Data_Byte_1, event.Data_Byte_2]);
         log_writeln;
         fill_list_item (item, event);
         Application.ProcessMessages;
//         scrolling := True;
//         adjust_time (index);
      end; // if
   end; // insert_event //

var subject: TMIDI_Container;
begin
   subject := TMIDI_Container (Subscriber.Publisher);
   case subject.Command of
      pm_Begin_Update: begin_update;
      pm_End_Update:   end_update (subject);
      pm_Insert_Event: if not block_insert then insert_event (subject);
{ TODO : update time genereert een error tijdens het spelenspelen was en de
  demo_container_view niet zichtbaar was }
//      pm_Update_Time:  adjust_time (subject.Current_Event);
      pm_Clear_Events: Reset;
   end; // case
end; // Alert //

procedure TView_Events.BeginUpdate;
begin
   View.Items.BeginUpdate;
end; // BeginUpdate //

procedure TView_Events.EndUpdate;
begin
   View.Items.EndUpdate;
end; // EndUpdate //

procedure TView_Events.fill_list_item (item: TListItem; event: TMIDI_Event);
var arg, db1: string;
begin
   if item = nil then Exit;
   with item do
   begin
      Caption := Format ('%d', [event.Time]);
      SubItems.Add (IntToStr (event.Port));
      if (event.MIDI_Message >= $F0) then // System or meta event
      begin
         SubItems.Add ('-');
         SubItems.Add (Format ('%2.2x', [event.MIDI_Message]));
      end else // Status message
      begin
         SubItems.Add (IntToStr (event.Channel));
         SubItems.Add (Format ('%2.2x', [event.Event_Type]));
      end; // if
      log_write_fmt ('[viewer]%12d %4d %2x %3d %3d', [gettickcount, event.Time, event.MIDI_Message, event.Data_Byte_1, event.Data_Byte_2]);
      log_writeln;
      SubItems.Add (Format ('%2.2x', [event.Data_Byte_1]));
      SubItems.Add (Format ('%2.2x', [event.Data_Byte_2]));
      if (event.Status = ms_Note_Off) or (event.Status = ms_Note_On)
         then db1 := Format ('%s%d', [MIDI_Note_Names [event.Data_Byte_1 mod 12], event.Data_Byte_1 div 12])
         else db1 := Format (' %2.2d ', [event.Data_Byte_1]);

      case event.Status of
         ms_Note_Off:               arg := 'note off: ' + db1;
         ms_Note_On:                arg := 'note on:  ' + db1;
         ms_Polyphonic_Aftertouch:  arg := 'polyphonic aftertouch ' + db1;
         ms_Controller_Change:      arg := 'control change ' + db1;
         ms_Program_Change:         arg := 'program change ' + db1;
         ms_Channel_Aftertouch:     arg := 'channel aftertouch ' + db1;
         ms_Pitch_Bend:             arg := 'pitch bend ' + db1;
         ms_Meta:                   arg := 'meta event ' + db1 + string (TMeta_Event (event).Translated);
      end; // case
      if (event.Size = 3) and (event.Status <> ms_Meta)
         then arg := arg + Format (' %2.2d', [event.Data_Byte_2]);
      SubItems.Add (arg);
   end; // With
end; // fill_list_item //

procedure TView_Events.adjust_time (index: Int32);

// This routine can be used to scroll the text when the music is played.
// The index points to the event in the event list. The event is
// displayed as being selected.

var delta, l, m, n, v: Int32;
begin
   if Self.Visible and Scrolling then
   begin
      v := View.VisibleRowCount;    // Count the number of Items in the view
      m := View.Items.IndexOf (View.TopItem); // Determine the index of the first item in view
      n := m + v;                   // n is the last item in view
      delta := 1;

      while (index < m) and (delta > 0) do
      begin
// when the listview is not in view the scroll command does not move
// the items resulting in an endless loop. Therefore delta is computed
// to measure the change in the TopItem. When this does not change: quit
         View.Scroll (0, -View.Height);
         l := View.Items.IndexOf (View.TopItem);
         delta := l - m; // compute delta. When zero, no change thus quit
         m := l;
         n := m + v;
      end; // while

      while (index > n) do
      begin
         View.Scroll (0, Height);
         m := View.Items.IndexOf (View.TopItem);
         if m < 0 then m := 0;
         n := m + v;
      end; // while
      View.Selected := View.Items [index];
   end; // if
end; // adjust_time //

function TView_Events.get_event_pos (time: Int32): Int32;
var pos, t: Int32;
begin
   pos := 0;
   if time > 0 then
   begin
      t := 0;
      while (t < time) and (pos < View.Items.Count) do
      begin
         t := strtoint (View.Items [pos].caption);
         pos := pos + 1;
      end; // while
   end; // if
   get_event_pos := pos;
end; // get_event_pos

procedure TView_Events.CustomDrawItem
(
   Sender: TCustomListView;   // TlistView
   Item: TListItem;           // Item to be drawn Odd (Item.Index)=alternate, else TListView.Color
   State: TCustomDrawState;   // not used
   var DefaultDraw: Boolean   // not used
);
begin
  if Odd(Item.Index) // odd list items have different background
     then Sender.Canvas.Brush.Color := clGradientInactiveCaption
     else Sender.Canvas.Brush.Color := clWindow; // even list items have window colour background
end; // TracksCustomDrawItem //

end. // unit: MSC_View_Events //
