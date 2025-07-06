unit utilities_logging;

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

uses SysUtils, Messages, Dialogs, Windows;

var log_file: TextFile;     // Logfile for debugging purposes
    log_logging: boolean = False; // True if log_file open

function  make_string (const args: array of const): string;
procedure log_start (file_name: string; appending: boolean = False);
procedure log_halt;
procedure log_write (mess: string);
procedure log_writeln (mess: string = '');
procedure log_write_fmt (fmt: string; const args: array of const);
procedure show_error (mess: string);
procedure show_warning (mess: string);
procedure show_info (mess: string);


implementation

function make_string (const args: array of const): string;
var v: TVarRec;
    i: Integer;
begin
   for i := 0 to High(Args) do
   begin
      v := args [i];
      case v.VType of
         vtInteger: Result := Result + IntToStr (v.VInteger);
         vtBoolean: if v.VBoolean then Result := Result + 'True'
                                  else Result := Result + 'False';
         vtChar:          Result := Result + string (v.VChar);
         vtExtended:      Result := Result + FloatToStr (v.VExtended^);
         vtPointer:       Result := Result + string (v.VPointer);
         vtObject:        Result := Result + v.VObject.ClassName;
         vtClass:         Result := Result + v.VClass.ClassName;
         vtCurrency:      Result := Result + FloatToStr (v.VCurrency^);
         vtVariant:       Result := Result + string (v.VVariant^);
         vtInt64:         Result := Result + IntToStr (v.VInt64^);
         vtString:        Result := Result + string (v.VString^);
         vtPChar:         Result := Result + string (v.VPChar);
         vtWideString:    Result := Result + string (v.VWideString^);
         vtWideChar:      Result := Result + v.VWideChar;
         vtPWideChar:     Result := Result + string (v.VPWideChar^);
         vtAnsiString:    Result := Result + string (v.VAnsiString);
         vtUnicodeString: Result := Result + string (v.VUnicodeString);
      end; // case
   end; // for
end; // make_string //

procedure log_start (file_name: string; appending: boolean = False);
begin
   if log_logging then log_halt;
   try
      DeleteFile (PChar (file_name));
      Assign (log_file, file_name);
      if FileExists (file_name) and appending
         then Append  (log_file)
         else ReWrite (log_file);
      log_logging := true;
      log_writeln ('[Logging started]');
   except
      try
         CloseFile (log_file);
      except
         on exception do ; // nothing
      end; // try
   end; // try
end; // open_log //

procedure log_halt;
begin
   if log_logging then
   begin
      log_writeln ('[Logging halted]');
      log_logging := false;
      CloseFile (log_file);
   end; // if
end; // close_log //

procedure log_write (mess: string);
begin
   if log_logging
      then Write (log_file, mess);
end; // log_write //

procedure log_writeln (mess: string = '');
begin
   if log_logging then
   begin
      WriteLn (log_file, mess);
      Write (log_file, DateTimeToStr (Now), ': ');
      Flush (log_file);
   end; // if
end; // write_log //

procedure log_write_fmt (fmt: string; const args: array of const);
var text: string;
begin
   if log_logging then
   begin
      text := Format (fmt, args);
      log_write (text);
   end; // if
end; // log_write_fmt //

procedure show_error (mess: string);
begin
   MessageDlg (mess, mtError, [mbOk], 0);
   log_writeln (mess);
end; // show_error //

procedure show_warning (mess: string);
begin
   MessageDlg (mess, mtWarning, [mbOk], 0);
   log_writeln (mess);
end; // show_warning //

procedure show_info (mess: string);
begin
   MessageDlg (mess, mtInformation, [mbOk], 0);
   log_writeln (mess);
end; // show_info //

end. // Unit: logfile //
