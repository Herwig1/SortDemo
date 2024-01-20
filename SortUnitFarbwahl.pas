unit SortUnitFarbwahl;

 (* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Festlegungen der Farben für die Kurven (Zeiten, Kosten)
   Autor:   H. Niemeyer  (c) 2010 - 2017
   Version: 1.6

   letzte Änderung: 10.09.2017 *)

interface

uses SysUtils, Classes, Graphics, Forms, Controls,
  Buttons, ExtCtrls, Dialogs,
  SortUnitDefinitionen, SortUnitSprache;

const
  BtnHoehe = 25;
  BtnWeite = 120;

type

  { TFarbwahlDlg }

  TFarbwahlDlg = class(TForm)
    Panel1: TPanel;
    BitBtnAbbruch: TBitBtn;
    BitBtnUebernehmen: TBitBtn;
    ColorDialog1: TColorDialog;
    PaintBox1: TPaintBox;
    procedure BitBtnUebernehmenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
    aktSpeedButton: TSpeedButton;
    speedBtn: array[TSortTyp] of TSpeedButton;
    procedure SpeedBtnClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FarbwahlDlg: TFarbwahlDlg;

implementation

{$R *.lfm}

procedure TFarbwahlDlg.BitBtnUebernehmenClick(Sender: TObject);
var
  t: TSortTyp;
begin
  for t := sMin to sLastSort do
    SortFarb[t] := speedBtn[t].Font.color;
end;

procedure TFarbwahlDlg.FormCreate(Sender: TObject);
var
  t: TSortTyp;
  y: integer;
begin
  for t := sMin to sLastSort do
  begin
    aktSpeedButton := TSpeedButton.Create(self);
    aktSpeedButton.parent := self;
    aktSpeedButton.onClick := @SpeedBtnClick;
    aktSpeedButton.Left := 60;
    aktSpeedButton.top := Ord(t) * (BtnHoehe + 5) + 5;
    aktSpeedButton.Height := BtnHoehe;
    aktSpeedButton.Width := BtnWeite;
    aktSpeedButton.Font.color := SortFarb[t];
    aktSpeedButton.Caption := SortName[t];
    aktSpeedButton.GroupIndex := 1;
    aktSpeedButton.Tag := Ord(t);
    speedBtn[t] := aktSpeedButton;
  end;
  y := self.Height - paintbox1.Height;
  self.Height := (Ord(sLastSort) + 1) * (BtnHoehe + 5) + 5 + y;
  self.Caption := sprachTab[s_FarbenBestimmen];
  BitBtnUebernehmen.Caption := sprachTab[s_uebernehmen];
  BitBtnAbbruch.Caption := sprachTab[s_Abbrechen];
end;

procedure TFarbwahlDlg.FormDestroy(Sender: TObject);
var
  t: TSortTyp;
begin
  for t := sMin to sLastSort do
  begin
    speedBtn[t].Free;
    speedBtn[t] := nil;
  end;
end;

procedure TFarbwahlDlg.PaintBox1Paint(Sender: TObject);
var
  y: integer;
  t: TSortTyp;
begin
  if speedBtn[sMin] <> nil then
    with paintbox1.Canvas do
      for t := sMin to sLastSort do
      begin
        y := speedBtn[t].top + BtnHoehe div 2;
        Pen.Width := 1;
        Pen.Color := speedBtn[t].Font.color;
        MoveTo(5, y);
        LineTo(speedBtn[t].left, y);
        Pen.Width := 5;
        MoveTo(speedBtn[t].left + speedBtn[t].Width, y);
        LineTo(paintbox1.Width - 5, y);
      end;
end;

procedure TFarbwahlDlg.SpeedBtnClick(Sender: TObject);
var
  k: integer;
begin
  if (Sender as TSpeedButton).down then
  begin
    aktSpeedButton := (Sender as TSpeedButton);
    colorDialog1.color := aktSpeedButton.Font.color;
    if colorDialog1.Execute then
      with paintbox1.Canvas do
      begin
        aktSpeedButton.Font.color := colorDialog1.color;
        Pen.Color := aktSpeedButton.Font.color;
        k := aktSpeedButton.top + BtnHoehe div 2;
        MoveTo(5, k);
        LineTo(paintbox1.Width - 5, k);
      end;
  end;
end;

end.
