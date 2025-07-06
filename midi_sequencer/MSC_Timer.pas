unit MSC_Timer;

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
  Windows, MMSystem, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TCallBack = procedure (uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD);

  EHiResTimer = class (Exception);

{$M+}
  THiResTimer = class (TObject)
  private
     nID: uInt32;
     FEnabled: boolean;
     FInterval: uInt32;
     FResolution: uInt32;
     FOnTimer: TNotifyEvent;
     procedure CreateTimer;

  protected
     procedure SetEnabled (value: boolean);

  public
     constructor Create;
     destructor Destroy; override;

  published
     property Enabled: boolean read FEnabled write SetEnabled default FALSE;
     property Interval: uInt32 read FInterval write FInterval default 100;
     property Resolution: uInt32 read FResolution write FResolution default 100;
     property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end; // Class: HiResTimer //
                        
implementation

procedure TimerCallback (uTimerID, uMessage: uInt32; dwUser, dw1, dw2: uInt32); stdcall;
var hr: THiResTimer;
begin
  hr := THiResTimer (dwUser);
  if Assigned (hr.FOnTimer) then hr.FOnTimer (hr);
end; // TimerCallback //

constructor THiResTimer.Create;
begin
   inherited Create;

   FEnabled := False;
   FInterval := 500;
   FResolution := 2;
end; // Create //

destructor THiResTimer.Destroy;
begin
   Enabled := False;

   inherited Destroy;
end; // Destroy //

procedure THiResTimer.SetEnabled (value: boolean);
var return: Int32;
begin
   if value <> FEnabled then
   begin
      if value then
      begin
         return := timeBeginperiod (FResolution);
         if return = TIMERR_NOCANDO then
         begin
            FEnabled := False;
            raise EHiResTimer.Create ('THiResTimer.SetEnabled - timer_nocando');
         end; // if
         CreateTimer;
      end else
      begin
         timeKillEvent (nID);
         timeEndperiod (FResolution);
      end; // if
      FEnabled := value;
   end; // if
end; // SetEnabled //

procedure THiResTimer.CreateTimer;
var
  lpTimerProc: TFNTimeCallBack;
begin
{$T-} // Switch off type checked pointers
   lpTimerProc := @TimerCallback;
{$T+} // Switch on again   
   nID := timeSetEvent (FInterval, FResolution, lpTimerProc, uInt32 (self), TIME_PERIODIC);
   if nID = 0 then
   begin
      FEnabled := FALSE;
      raise EHiResTimer.Create ('THiResTimer.CreateTimer - Unable to create a timer');
   end; // if
end; // CreateTimer //

end.
