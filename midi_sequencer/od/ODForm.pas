unit ODForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type TByte_File = file of byte;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button_Open: TBitBtn;
    Dialog_Open: TOpenDialog;
    View: TListView;
    Edit_Max: TLabeledEdit;
    Button_Save: TBitBtn;
    Dialog_Save: TSaveDialog;
    procedure Button_OpenClick(Sender: TObject);
    procedure Button_SaveClick(Sender: TObject);

  private
      file_name: string;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button_OpenClick (Sender: TObject);
const break = 20;
var infile: TByte_File;
    item: TListItem;
    i: integer;
    max: integer;
    b: byte;
    line, tline, hline, dline: string;
begin
   i := 0;
   max := StrToInt (Edit_Max.Text);
   if max < 1 then max := 1;
   if Dialog_Open.Execute then
   begin
      file_name := Dialog_Open.FileName;
      View.Items.Clear;
      AssignFile (infile, file_name);
      Reset (infile);
      try
         while not EOF (infile) and (i < max) do
         begin
            i := i + 1;
            Read (infile, b);
            hline := hline + Format ('%3.2x', [b]);
            dline := dline + Format ('%3.3d ', [b]);
            if b = 0  then b := 216 else
            if b < 32 then b := 32;
            tline := tline + Chr (b);
            if i mod break = 0 then
            begin
               line := Format ('%5.5d - %5.5d', [i - break + 1, i]);
               item := View.Items.Add;
               item.Caption := line;
               Item.SubItems.Add (tline);
               Item.SubItems.Add (hline);
               Item.SubItems.Add (dline);
               tline := '';
               hline := '';
               dline := '';
            end; // if
         end; // while

         if i mod break <> 0 then
         begin
            line := Format ('%5.5d - %5.5d', [i - break + 1, i]);
            item := View.Items.Add;
            item.Caption := line;
            Item.SubItems.Add (tline);
            Item.SubItems.Add (hline);
            Item.SubItems.Add (dline);
            tline := '';
            hline := '';
            dline := '';
         end; // if
      finally
         CloseFile (infile);
      end; // try
   end; // if
end; // Button_OpenClick //

procedure TForm1.Button_SaveClick(Sender: TObject);
var infile, outfile: TByte_File;
    cr, lf, b: byte;
begin
   cr := $0D;
   lf := $0A;
   if file_name <> '' then
   begin
      if Dialog_Save.Execute then
      begin
         AssignFile (infile, file_name);
         AssignFile (outfile, Dialog_Save.FileName);
         Reset (infile);
         try
            ReWrite (outfile);
            try
               while not EOF (infile) do
               begin
                  Read (infile, b);
                  if b = lf then Write (outfile, cr, lf)
                            else Write (outfile, b);
               end; // while
            finally
               CloseFile (outfile);
            end; // try
         finally
            CloseFile (infile);
         end; // try
      end; // if
   end; // if
end;

end.
