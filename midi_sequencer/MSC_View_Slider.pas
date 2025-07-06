unit MSC_View_Slider;

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
     MSC_Definitions, MSC_Container, MSC_View_Base, ExtCtrls;

const margin = 10;

type
   TView_Slider = class (TView_Base)
      Slider: TTrackBar;
      Panel: TPanel;
      Image: TImage;

      procedure FrameResize (Sender: TObject);

   private
      FTicks: Int32;
      FLength: Int32;
      FSeconds: double;
      FContainer: TMIDI_Container;
      FOnChange: TNotifyEvent;

      procedure set_ticks (value: Int32);
      procedure set_seconds (value: double);
      function  get_position: Int32;
      procedure set_position (value: Int32);
      procedure set_change (value: TNotifyEvent);
      procedure set_time_marks (time: double);

   public
      constructor Create (aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset; override;
      procedure Alert (Sender: Tobject); override;

      property Ticks: Int32 read FTicks write set_ticks;
      property Length: Int32 read FLength write FLength;
      property Seconds: double write set_seconds;
      property Position: Int32 read get_position write set_position;
      property OnChange: TNotifyEvent read FOnChange write set_change;
   end; // TView_Slider //

implementation

{$R *.dfm}

constructor TView_Slider.Create (aOwner: TComponent);
begin
   inherited Create (aOwner);

   FTicks   := 5;
   FLength  := 6;
   FSeconds := -1.0;
end; // Create //

destructor TView_Slider.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TView_Slider.Reset;
begin
   inherited Reset;
end; // Reset //

procedure TView_Slider.FrameResize (Sender: TObject);
begin
   inherited;

   set_time_marks (FSeconds);
end; // FrameResize //

procedure TView_Slider.Alert (Sender: Tobject);
   procedure begin_update;
   begin
      Slider.Enabled  := False;
      Slider.Position := 0;
   end; // begin_update //

   procedure end_update (container: TMIDI_Container);
   begin
      Max_Time := container.Max_Time;
      if Max_Time > 0 then
      begin
         Slider.Enabled  := True;
         Slider.Max      := Max_Time;
         Slider.PageSize := Max_Time div 10;
      end else
      begin
         Slider.Enabled := False;
      end; // if
   end; // end_update //

   procedure update_time (event: TMIDI_Event);
   var temp: TNotifyEvent;
   begin
      if (event <> nil) and (Int32 (event.time) <= Max_Time) then
      begin
         temp := Slider.OnChange;
         Slider.OnChange := nil; // Prevent an endless onchange/position update loop
         Slider.Position := event.time;
         Slider.OnChange := temp;
      end; // if
   end; // update_time //

begin
   FContainer := TMIDI_Container (Subscriber.Publisher);
   case FContainer.Command of
      pm_Begin_Update: begin_update;
      pm_End_Update:   end_update (FContainer);
      pm_Insert_Event: ;
      pm_Update_Time:  update_time (FContainer.Event [FContainer.Current_Event]);
      pm_Clear_Events: end_update (FContainer);
   end; // case
end; // Alert //

procedure TView_Slider.set_ticks (value: Int32);
begin
   FTicks := value;
end; // set_ticks //

procedure TView_Slider.set_seconds (value: double);
begin
   FSeconds := value;
   set_time_marks (value);
end; // set_seconds //

function TView_Slider.get_position: Int32;
begin
   get_position := Slider.Position;
end; // get_position //

procedure TView_Slider.set_position (value: Int32);
begin
   Slider.Position := Value;
end; // set_position //

procedure TView_Slider.set_change (value: TNotifyEvent);
begin
   FOnChange := value;
   Slider.OnChange := value;
end; // set_change //

procedure TView_Slider.set_time_marks (time: double);
var bitmap: TBitmap;
    slice: Int32;
    eTick: Int32;
    p, w1, w2, x: Int32;
    value: double;
    front, stime: string;
begin
   if time <= 0 then Exit;
   bitmap := TBitmap.Create;
   try
      Sleep (100);
      Application.ProcessMessages;
      bitmap.Width  := Image.Width;
      bitmap.Height := Image.Height;
      bitmap.Canvas.Brush.Color := clWindow;
      bitmap.Canvas.Brush.Style := bsSolid;
      bitmap.Canvas.Pen.Color := clWindow;
      bitmap.Canvas.Pen.Style := psSolid;
      bitmap.Canvas.Rectangle (0, 0, bitmap.Width, bitmap.Height);
      bitmap.Canvas.Pen.Color := clBlack;
      bitmap.Canvas.Pen.Width := 1;
      slice := (bitmap.Width - (2 * margin)) div (FTicks - 1);
      for eTick := 0 to FTicks - 1 do
      begin
// position where the tick should be plotted
         x := eTick * slice + margin;

// Compute value and plot it. Compute a position such that the decimal
// point of the value is exactly below the tick.
         value := eTick / (FTicks - 1.0) * time;
         stime := Format ('%.2f', [value]);
         p := Pos ('.', stime);
         if p > 0 then front := Copy (stime, 1, p - 1);
         w1 := bitmap.Canvas.TextWidth (front);
         w2 := bitmap.Canvas.TextWidth ('.') div 2;

// First plot text and next plot tick
         p := x - (w1 + w2) + 1;
         if p < 1 then p := 1 else
         if (p + bitmap.Canvas.TextWidth (stime)) >= bitmap.Width
            then p := bitmap.Width - bitmap.Canvas.TextWidth (stime);
         bitmap.Canvas.TextOut (p, FLength, stime);
         bitmap.Canvas.MoveTo (x, 0);
         bitmap.Canvas.LineTo (x, FLength);
      end; // for
      Image.Picture.Assign (bitmap);
   finally
      bitmap.Free;
   end; // try
end; // set_time_marks //

end. // unit: MSC_View_Slider //
