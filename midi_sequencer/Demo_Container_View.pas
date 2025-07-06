unit Demo_Container_View;

interface

uses Windows, Messages, SysUtils, Classes, Controls, ComCtrls, AnsiStrings,
     Publish_Subscribe, MSC_Definitions, MSC_Container;

type
   TView_Columns = (vcTime, vcTrack, vcChannel, vcEventType, vcDB1, vcDB2, vcText);

const
   vcHeaders: array [TView_Columns] of string = ('Time', 'Track', 'Channel', 'Event',
                                                 'Data 1', 'Data 2', 'Text');
   vcWidths: array [TView_Columns] of Int32 = (50, 40, 40, 50, 80, 40, 200);

type
   TDemo_Container_View = class (TListView, ISubscribe)
   private
      FSubscriber: TSubscriber;  // Implement a subscriber
      Scrolling: boolean;        // No scrolling between beginupdate and endupdate
      block_insert: boolean;     // the whole MIDI container must be inserted when true

      function get_event_pos (time: Int32): Int32;

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset;
      procedure adjust_time (index: Int32);

   published
      procedure sbUpdate (Publication: TPublicationNotice);
      procedure Alert (Sender: Tobject);

      property Subscriber: TSubscriber read FSubscriber implements ISubscribe;
   end; // Class: TDemo_Container_View //

   TThreaded_Block_Insert = class (TThread)
   private
      container: TMIDI_Container;
      list_View: TListView;

   public
      constructor Create (subject: TMIDI_Container; list_View: TListView);
      destructor Destroy; override;
      procedure Execute; override;
   end; // TThreaded_Block_Insert //

procedure Register;
procedure fill_list_item (var item: TListItem; event: TMIDI_Event);

implementation

procedure Register;
begin
   RegisterComponents (MSC_Package_Name, [TDemo_Container_View]);
end; // Register //

procedure fill_list_item (var item: TListItem; event: TMIDI_Event);
begin
   if item = nil then Exit;
   with item do
   begin
      Caption := Format ('%d', [event.Time]);
      SubItems.Add (IntToStr (event.Track));
      if (event.MIDI_Message >= $F0) then // System or meta event
      begin
         SubItems.Add ('-');
         SubItems.Add (Format ('%2.2x', [event.MIDI_Message]));
      end else // Status message
      begin
         SubItems.Add (IntToStr (event.Channel));
         SubItems.Add (Format ('%2.2x', [event.Event_Type]));
      end; // if
      SubItems.Add (Format ('%2.2x', [event.Data_Byte_1]));
      SubItems.Add (Format ('%2.2x', [event.Data_Byte_2]));
      if event.MIDI_Message >= $F0
         then SubItems.Add (string (TMeta_Event (event).Translated));
   end; // With
end; // fill_list_item //

{*******************************************************************
*                                                                  *
* Class TDemo_Container_View                                       *
*                                                                  *
********************************************************************}

constructor TDemo_Container_View.Create (AOwner: TComponent);
var col: TListColumn;
    c: TView_Columns;
begin
   inherited Create (AOwner);

// Statements to unlock subscriber functionality
   FSubscriber := TSubscriber.Create;
   Subscriber.OnUpdate := Alert;

   ViewStyle     := vsReport;
   ReadOnly      := True;
   GridLines     := True;
   RowSelect     := True;
   HideSelection := False;
   block_insert  := False;

   for c := Low (TView_Columns) to High (TView_Columns) do
   begin
      col := Columns.Add;
      col.Caption := vcHeaders [c];
      col.Width   := vcWidths  [c];
   end; // for
end; // Create //

destructor TDemo_Container_View.Destroy;
begin
   FSubscriber.Free;

   inherited Destroy;
end; // Destroy //

procedure TDemo_Container_View.sbUpdate (Publication: TPublicationNotice);
begin
   if Publication = pnUpdate then
   begin
// Something to do, but what??
   end; // if
end; // sbUpdate //

procedure TDemo_Container_View.Alert (Sender: Tobject);

   procedure begin_update;
   begin
      Items.BeginUpdate;
      Scrolling    := False;
      block_insert := True;
   end; // begin_update //

   procedure end_update (container: TMIDI_Container);
   begin
      if block_insert
         then TThreaded_Block_Insert.Create (container, Self);
      Items.EndUpdate;
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
         item := Items.Insert (index);
         fill_list_item (item, event);
         adjust_time (index);
      end; // if
   end; // insert_event //

var subject: TMIDI_Container;
begin
   subject := TMIDI_Container (Subscriber.Publisher);
   case subject.Command of
      pm_Begin_Update: begin_update;
      pm_End_Update:   end_update (subject);
      pm_Insert_Event: if not block_insert then insert_event (subject);
{ TODO : gaf een error terwijl aan het spelen was en de demo_container_view niet zichtbaar was }
      pm_Update_Time:  adjust_time (subject.Current_Event);
      pm_Clear_Events: Reset;
   end; // case
end; // Alert //

procedure TDemo_Container_View.Reset;
begin
   Items.BeginUpdate;
   Clear;
   Items.EndUpdate;
end; // Reset //

procedure TDemo_Container_View.adjust_time (index: Int32);

// This routine can be used to scroll the text when the music is played.
// The index points to the event in the event list. The event is
// displayed as being selected.

var delta, l, m, n, v: Int32;
begin
   if Self.Visible and Scrolling then
   begin
      v := Self.VisibleRowCount;    // Count the number of Items in the view
      m := Items.IndexOf (TopItem); // Determine the index of the first item in view
      n := m + v;                   // n is the last item in view
      delta := 1;

      while (index < m) and (delta > 0) do
      begin
// when the listview is not in view the scroll command does not move
// the items resulting in an endless loop. Therefore delta is computed
// to measure the change in the TopItem. When this does not change: quit
         Scroll (0, -Height);
         l := Items.IndexOf (TopItem);
         delta := l - m; // compute delta. When zero, no change thus quit
         m := l;
         n := m + v;
      end; // while

      while (index > n) do
      begin
         Scroll (0, Height);
         m := Items.IndexOf (TopItem);
         if m < 0 then m := 0;
         n := m + v;
      end; // while
      Selected := Items [index];
   end; // if
end; // adjust_time //

function TDemo_Container_View.get_event_pos (time: Int32): Int32;
var pos, t: Int32;
begin
   pos := 0;
   if time > 0 then
   begin
      t := 0;
      while (t < time) and (pos < Items.Count) do
      begin
         t := strtoint (Items [pos].caption);
         pos := pos + 1;
      end; // while
   end; // if
   get_event_pos := pos;
end; // get_event_pos

{*******************************************************************
*                                                                  *
* Class TThreaded_Insert_Block                                     *
*                                                                  *
********************************************************************}

constructor TThreaded_Block_Insert.Create
(
   subject: TMIDI_Container;  // Container the get MIDI events from
   list_view: TListView       // Listview to insert items into
);
begin
   inherited Create (True);

   Container       := subject;
   Self.list_view  := list_view;
   FreeOnTerMinate := True;
   Resume;
end; // Create //

destructor TThreaded_Block_Insert.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TThreaded_Block_Insert.Execute;
var item: TListItem;
    event: TMIDI_Event;
    i: Int32;
begin
   for i := 0 to container.Count - 1 do
   begin
      event := container.Event [i];
      item  := list_view.Items.Insert (i);
      fill_list_item (item, event);
   end; // if
end; // Execute //

end. // Unit: Demo_Container_View //
