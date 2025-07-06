unit MSC_View_Base;

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
     Dialogs,
     Publish_Subscribe;

type
   TView_Base = class (TFrame, ISubscribe)
   private
      FSubscriber: TSubscriber;  // Implement a subscriber
      FMax_Time: Int32;          // Maximum time for displaying results
      FColor_Even: TColor;       // Color for even rows
      FColor_Odd: TColor;        // Color for odd rows

      procedure set_max_time (max_time: Int32);
      procedure set_color_even (color: TColor);
      procedure set_color_odd (color: TColor);

   public
      constructor Create (aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Reset; virtual;

      procedure Alert (Sender: Tobject); virtual; abstract;

      property Subscriber: TSubscriber read FSubscriber implements ISubscribe;
      property Max_Time: Int32 read FMax_Time write set_max_time;
      property Color_Even: TColor read FColor_Even write set_color_even;
      property Color_Odd:  TColor read FColor_Odd  write set_color_odd;
   end; // Class: TTMSC_View_Base //

implementation

{$R *.dfm}

constructor TView_Base.Create (aOwner: TComponent);
begin
   inherited Create (aOwner);

// Statements to unlock subscriber functionality
   FSubscriber         := TSubscriber.Create;
   Subscriber.OnUpdate := Alert;

   FMax_Time          := -1;
   FColor_Even        := clGradientActiveCaption;
   FColor_Odd         := clGradientInactiveCaption;
end; // Create //

destructor TView_Base.Destroy;
begin
   inherited Destroy;

   FSubscriber.Free;
end; // Destroy //

procedure TView_Base.Reset;
begin
// Just empty
end; // Reset //

procedure TView_Base.set_max_time (max_time: Int32);
begin
   FMax_Time := max_time;
end; // set_max_time //

procedure TView_Base.set_color_even (color: TColor);
begin
   FColor_Even := color;
end; // set_color_even //

procedure TView_Base.set_color_odd (color: TColor);
begin
   FColor_Odd := color;
end; // set_color_odd //

end. // Unit: MSC_View_Base //
