unit SortUnitFarbwahl_Demo;

 (* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Festlegungen der Farben für die Demonstrationen
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

  { TFarbwahlDlg2 }

  TFarbwahlDlg2 = class(TForm)
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
    speedBtn: array[TDemoColor] of TSpeedButton;
    procedure SpeedBtnClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FarbwahlDlg2: TFarbwahlDlg2;

implementation

{$R *.lfm}

procedure TFarbwahlDlg2.BitBtnUebernehmenClick(Sender: TObject);
var
  t: TDemoColor;
begin
  for t := inaktiv to vergleich2_QSort do
    DemoFarb[t] := speedBtn[t].Font.color;
end;

procedure TFarbwahlDlg2.FormCreate(Sender: TObject);
var
  t: TDemoColor;
  y: integer;
  DemoName: array[TDemoColor] of shortstring;
begin
  for t := inaktiv to MinMarker do
  begin
    DemoName[t] := sprachTab[s_inaktiv + Ord(t)];
  end;
  DemoName[marker_QSort] := DemoName[PosMarker] + '2';
  DemoName[vergleich2_QSort] := DemoName[vergleichen] + '2';
  for t := inaktiv to vergleich2_QSort do
  begin
    aktSpeedButton := TSpeedButton.Create(self);
    aktSpeedButton.parent := self;
    aktSpeedButton.onClick := @SpeedBtnClick;
    aktSpeedButton.Left := 60;
    aktSpeedButton.top := Ord(t) * (BtnHoehe + 5) + 5;
    aktSpeedButton.Height := BtnHoehe;
    aktSpeedButton.Width := BtnWeite;
    aktSpeedButton.Font.color := DemoFarb[t];
    aktSpeedButton.Caption := DemoName[t];
    aktSpeedButton.GroupIndex := 1;
    aktSpeedButton.Tag := Ord(t);
    speedBtn[t] := aktSpeedButton;
  end;
  y := self.Height - paintbox1.Height;
  self.Height := (Ord(vergleich2_QSort) + 1) * (BtnHoehe + 5) + 5 + y;
  self.Caption := sprachTab[s_FarbenBestimmen];
  BitBtnUebernehmen.Caption := sprachTab[s_uebernehmen];
  BitBtnAbbruch.Caption := sprachTab[s_Abbrechen];
end;

procedure TFarbwahlDlg2.FormDestroy(Sender: TObject);
var
  t: TDemoColor;
begin
  for t := inaktiv to vergleich2_QSort do
  begin
    speedBtn[t].Free;
    speedBtn[t] := nil;
  end;
end;

procedure TFarbwahlDlg2.PaintBox1Paint(Sender: TObject);
var
  y: integer;
  t: TDemoColor;
begin
  if speedBtn[inaktiv] <> nil then
    with paintbox1.Canvas do
      for t := inaktiv to vergleich2_QSort do
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

procedure TFarbwahlDlg2.SpeedBtnClick(Sender: TObject);
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
