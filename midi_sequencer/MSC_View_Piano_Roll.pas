unit MSC_View_Piano_Roll;

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
     Dialogs, ExtCtrls, ComCtrls, ImgList,
     MSC_Definitions, MSC_Container, MSC_View_Base, utilities_math;

const basic_colors = 16;
      max_width    = 1000;

type
   TBasic_Colors = array [0..basic_colors - 1] of TColor;

const
   basic_color:  TBasic_Colors = // Colors used to plot the tracks
         ($00CC00CC, $000000FF, $0000FF00, $00FF0000,
          $00FF00FF, $00FFFF00, $008800FF, $00FF8800,
          $00008800, $00000088, $00880000, $00000000,
          $00880088, $00008888, $0088FF00, $00FF8800);

type
   TView_Piano_Roll = class (TView_Base)
      Channel_Colors: TImageList;
      Panel_Event: TPanel;
      Panel_Front: TPanel;
      Image_Front: TImage;
      Panel_Legenda: TPanel;
      Legenda: TListView;
      Panel_Roll: TPanel;
      Image: TImage;
      Bevel_Track: TBevel;

      procedure FormResize (Sender: TObject);

   private
      FRaw_Copy: TStrings;          // Log screen
      FContainer: TMIDI_Container;  // Contains MIDI events to draw
      FMax_Measures: Int32;         // Max. number of measures to plot
      FXZoom: double;               // Horizontal zoom factor
      FYZoom: double;               // Vertical zoom factor
      max_time: Int32;              // Highest time in TMIDI_Container

      procedure set_max_measures (meas: Int32);
      procedure update_legenda (container: TMIDI_Container);
      procedure plot (container: TMIDI_Container; x_zoom, y_zoom: double; color_even, color_odd: TColor);

   public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset; override;

      procedure Alert (Sender: Tobject); override;

      property Max_Measures: Int32 read FMax_Measures write set_max_measures;
      property XZoom: double read FXZoom write FXZoom;
      property YZoom: double read FYZoom write FYZoom;
      property Raw_Copy: TStrings read FRaw_Copy write FRaw_Copy;
   end; //Class: TPiano_Roll //

   TThreaded_Plot = class (TThread)
   private
      container: TMIDI_Container;   // Contains MIDI events to draw
      Image: TImage;                // Picture of piano roll
      Image_Front: TImage;          // Picture of Y-axis
      max_time: Int32;              // Time of latest event in container
      tonica: TMSC_Key;             // Indicates tonica (0 = c, 1 = c#, etc.)
      XZoom, YZoom: double;         // 1 time unit in MSC = 1/32 note. Time units = pixels/time unit
      Color_Even: TColor;           // color for even rows
      Color_Odd: TColor;            // color for odd rows

   public
      constructor Create (subject: TMIDI_Container; image: TImage; image_front: TImage; xzoom, yzoom: double; color_even, color_odd: TColor);
      destructor Destroy; override;
      function  plot_background (Image: TImage): TBitmap;
      procedure plot_y_scale (bitmap: TBitmap);
      procedure plot_event (bitmap: TBitmap; time, track, note, duration: Int32);
      procedure Execute; override;
   end; // Class: TThreaded_PLot //

implementation

{$R *.dfm}

{*******************************************************************
*                                                                  *
* Class TDemo_Piano_View                                           *
*                                                                  *
********************************************************************}


constructor TView_Piano_Roll.Create (AOwner: TComponent);
begin
   inherited Create (AOwner);

   FContainer         := nil;
   FRaw_Copy          := nil;
   Bevel_Track.Left   := 0;
   Bevel_Track.Height := 0;
end; // Create //

destructor TView_Piano_Roll.Destroy;
begin
   inherited Destroy;
end; // FormDestroy //

procedure TView_Piano_Roll.Reset;
begin
   plot (nil, 1.0, 1.0, Color_Even, Color_Odd);
end; // Reset //

procedure TView_Piano_Roll.plot
(
   container: TMIDI_Container;   // MIDI events to plot
   x_zoom, y_zoom: double;       // Zoom factors for x and y
   color_even, color_odd: TColor // colors for even and odd row
);
begin
   Self.XZoom          := x_zoom;
   Self.YZoom          := y_zoom;
   Bevel_Track. Left   := 0;
   Bevel_Track. Height := Image.Height;

   if (container <> nil) and (container.Count > 0)
      then max_time := container.Max_Time
      else max_time := 1;
   TThreaded_Plot.Create (container, Image, Image_Front, XZoom, YZoom, color_even, color_odd);
   update_legenda (container);
end; // plot //

procedure TView_Piano_Roll.Alert (Sender: Tobject);

   procedure begin_update (container: TMIDI_Container);
   begin
      if Raw_Copy <> nil then
         Raw_Copy.Add('Piano Roll: Alert: Begin Update');
   end; // begin_update //

   procedure end_update (container: TMIDI_Container);
   begin
      if Raw_Copy <> nil then
         Raw_Copy.Add('Piano Roll: Alert: End update');
      plot (container, 1.0, 1.0, Color_Even, Color_Odd);
   end; // end_update //

// a new note should be plotted. not yet implemented
   procedure insert_event (container: TMIDI_Container; index, duration: Int32);
   var event: TMIDI_Event;
   begin
      event := container.Event [index];
      if is_note_on (event) then
      begin
//         duration := container.find_duration (event, container.Current_Event);
//         if (Int32 (event.Time) + duration) <= max_time
//            then plot_event (bitmap, event.Time, event.Track, event.Data_Byte_1, duration);
      end; // if
   end; // insert_event //

// Set the time tracker on the correct position in the piano roll
   procedure update_time (event: TMIDI_Event);
   begin
      if (event <> nil) and (event.time < uInt32 (max_time))
         then Bevel_Track.Left := rescale (event.time, 0, max_time, 0, Image.Width);
   end; // update_time //

// Create an empty piano roll
   procedure clear_events (container: TMIDI_Container);
   begin
      if Raw_Copy <> nil then
         Raw_Copy.Add('Piano Roll: Alert: Clear Events');
      plot (nil, 1.0, 1.0, Color_Even, Color_Odd);
   end; // clear_events //

begin
   FContainer := TMIDI_Container (Subscriber.Publisher);
   case FContainer.Command of
      pm_Begin_Update: begin_update (FContainer);
      pm_End_Update:   end_update   (FContainer);
      pm_Insert_Event: insert_event (FContainer, FContainer.Last_Index, 0);
      pm_Update_Time:  update_time  (FContainer.Event [FContainer.Current_Event]);
      pm_Clear_Events: clear_events (FContainer);
   end; // case
end; // Alert //

procedure TView_Piano_Roll.set_max_measures (meas: Int32);
begin
   FMax_Measures := meas;
end; // set_max_measures //

procedure TView_Piano_Roll.FormResize (Sender: TObject);
begin
   plot (FContainer, 1.0, 1.0, Color_Even, Color_Odd);
end; // FormResize //

procedure TView_Piano_Roll.update_legenda (container: TMIDI_Container);
var item: TlistItem;
    bmp: TBitmap;
    color: TColor;
    eChannel: Int32;
    size: Int16;
begin
   if container = nil then Exit;

   Legenda.Items.clear;
   if container <> nil then
   begin
      size := channel_colors.Height;
      channel_colors.Masked := False;
      for eChannel := Low (TMIDI_Range) to High (TMIDI_Range) do
      if container.Voice_Count [eChannel] > 0 then
      begin
         color := basic_color [eChannel mod basic_colors];
         item  := Legenda.Items.add;
         item.Caption := container.Track_Name [eChannel];
         bmp := Tbitmap.Create;
         try
            bmp.PixelFormat := Image.Picture.Bitmap.PixelFormat;
            bmp.height := size;
            bmp.Width  := size;
            bmp.Canvas.Brush.Color := Color;
            bmp.Canvas.Pen  .Color := Color;
            bmp.Canvas.Rectangle (0, 0, size - 1, size - 1);
            item.ImageIndex := Channel_Colors.Add (bmp, nil);
         finally
            bmp.Free;
         end;
      end; // for
   end; // if
end; // update_legenda //

{*******************************************************************
*                                                                  *
* Class TThreaded_Plot                                             *
*                                                                  *
********************************************************************}

constructor TThreaded_Plot.Create
(
   subject: TMIDI_Container;     // MIDI container to plot
   image: TImage;                // Image to plot on
   image_front: TImage;          // Image to plot Y-legenda on
   xzoom, yzoom: double;         // number of pixels/unit, x = time, y = pitch
   color_even, color_odd: TColor // Colors for even resp. odd rows
);
var tzoom: double;
    h, w: Int32;
begin
   inherited Create (True);

   FreeOnTerminate  := True;
   container        := subject;
   Self.Image       := image;
   Self.Image_Front := image_front;
   Self.XZoom       := xzoom;
   Self.YZoom       := yzoom;
   Self.Color_Even  := color_even;
   Self.Color_Odd   := color_odd;
   if container <> nil then
   begin
      max_time := container.max_time;
      tonica   := Int32 (container.Key);
      if max_time <= 0
         then tzoom := 1.0
         else tzoom := max_width / max_time;
      if tzoom < xzoom then xzoom := tzoom;
      if (xzoom * max_time) > Image.Picture.Bitmap.Width
         then w := Round (xzoom * max_time)
         else w := Image.Width;
      Image.Width := w;
   end; // if

   h := Round (yzoom * C7Bit);
   Image_Front.Height := h;
   Image.Height       := h;
   Resume;
end; // Create //

destructor TThreaded_Plot.Destroy;
begin
   inherited Destroy;
end; // Destroy //

procedure TThreaded_Plot.Execute;
var bitmap: TBitmap;
    event: TMIDI_Event;
    eEvent, duration: Int32;
begin
   bitmap := nil; // avoid compiler warnings
   try
      bitmap := plot_background (Image_Front);
      plot_y_scale (bitmap);
      Image_Front.Picture.Assign (bitmap);
   finally
      bitmap.Free;
   end; // try

   try
      bitmap := plot_background (Image);
      if container <> nil then
      begin
         for eEvent := 0 to container.Count - 1 do
         begin
            event := container.Event [eEvent];
            if is_note_on (event) then
            begin
               duration := container.find_duration (event, eEvent);
               if (Int32 (event.Time) + duration) <= max_time
                  then plot_event (bitmap, event.Time, event.Port, event.Data_Byte_1, duration);
            end; // if
         end; // for
      end; // if
      Image.Picture.Assign (bitmap);
   finally
      bitmap.Free;
   end; // try
   Terminate;
end; // Execute //

function TThreaded_Plot.plot_background (Image: TImage): TBitmap;

// Plots a background of alternating colors

var bitmap: TBitmap;
    color: TColor;
    xb, xe, y, yb, ye: Int32;
    even: boolean;
    target_range: Int32;
    max: Int32;
begin
   bitmap := TBitmap.Create;
   bitmap.Height := Image.Height;
   bitmap.Width  := Image.Width;
   bitmap.PixelFormat := pf32Bit;
   bitmap.Canvas.Lock;
   even := True;
   xb   := 0;
   xe   := Image.Width;
   y    := Tonica;
   yb   := rescale (C7Bit - 1, 0, High (T7Bit), 0, Round (yzoom * Image.Height));
   target_range := Round (yzoom * Image.Height);
   max := High (T7Bit);
   while (y <= High (T7Bit)) do
   begin
      y  := y + 12;
      ye := rescale (C7Bit - y - 1, 0, max, 0, target_range);
      if even
         then color := color_even
         else color := color_odd;
      Bitmap.Canvas.Pen  .Color := color;
      Bitmap.Canvas.Brush.Color := color;
      Bitmap.Canvas.Rectangle (xb, yb, xe, ye);
      yb := ye;
      even := not even;
   end; // while

   bitmap.Canvas.Unlock;
   plot_background := bitmap;
end; // plot_back_ground //

procedure TThreaded_Plot.plot_y_scale (bitmap: TBitmap);

// This routine sets up a framework on which the piano roll is plotted

var color: TColor;
    h, i, y: Int32;
    even: boolean;
    s: string;
begin
// Set up the plot environment and reset all variables
   i := 0;
   y := tonica;
   even := True;

// Position texts in the plot
   bitmap.Canvas.Lock;
   try
      while (y <= High (T7Bit)) do
      begin
         if even
            then color := color_even
            else color := color_odd;
         s := Format (' %s%d ', [MIDI_Note_Names [tonica], i]);
         h := rescale (C7Bit - y, 0, High (T7Bit), 0, bitmap.Height) -
                  (Bitmap.Canvas.TextHeight (s)) - 4;
         Bitmap.Canvas.Brush.Color := color;
         Bitmap.Canvas.TextOut (10, h, s);
         y := y + 12;
         i := i + 1;
         even := not even;
      end; // while
   finally
      bitmap.Canvas.Unlock;
   end; // try
end; // plot_y_scale //

procedure TThreaded_Plot.plot_event (bitmap: TBitmap; time, track, note, duration: Int32);
var xb, xe, y, yb: Int32;
begin
   if (duration > 0) then // means: is a note on event
   begin
      bitmap.Canvas.Lock;
      try
         Bitmap.Canvas.Pen.Width := 1;
         Bitmap.Canvas.Brush.Style := bsSolid;
         yb := bitmap.Height div C7Bit;
         if yb < 1 then yb := 1;
         y := note;
         xb := rescale (time,            0, max_time, 0, Image.Width);
         xe := rescale (time + duration, 0, max_time, 0, Image.Width);
         y  := rescale (C7Bit - y, 0, High (T7Bit), 0, Image.Height);
         if (xe - xb) > 1 then xe := xe - 1;
         Bitmap.Canvas.Pen  .Color := basic_color [Track mod basic_colors];
         Bitmap.Canvas.Brush.Color := basic_color [Track mod basic_colors];
         Bitmap.Canvas.Rectangle (xb, y, xe, y + yb);
      finally
         bitmap.Canvas.Unlock;
      end;
   end; // if
end; // plot_event //

end. // Unit: MSC_View_Piano_Roll //
