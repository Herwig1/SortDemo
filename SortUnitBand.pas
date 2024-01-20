unit SortUnitBand;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Modul zum Sortieren von Bändern ( MergeSort mit und ohne Heap )
   Autor:   H. Niemeyer  (c) 2010 - 2017, 2023, 2024
   Version: 1.8

   letzte Änderung: 20.01.2024 *)

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, Spin,
  SortUnitDefinitionen, SortUnitSprache, SortZahlenfeldDlg;

const
  maxSpalten = 80;
  abstand = 4;
  maxfeldAnz = maxSpalten div abstand; { Laenge der Baender }
  feld = maxfeldAnz + 10;

  heapmax = 7;  { Groesse des Heaps }
  leer = '  ';
  bandanzahl = 4;
  x0 = 5;
  oLine = 1;
  zLine = 3;
  ioLine = 4;
  ein = True;
  aus = False;

type
  tapenr = -1..bandanzahl;
  heapnr = 0..heapmax;
  feldnr = integer;{ 1..feld;}
  numStr = string[2];
  reihe = array[1..feld] of numStr;
  TBaender = array[0..bandanzahl] of reihe;
  TAnordnung = array[-1..heapmax] of integer;

  TFeldPos = record
    x, y: integer;
    wert: numStr;
    fontCol, FrameCol: TColor
  end;
  TPosition = array[-1..heapmax] of TFeldPos;

const
  // für heapmax = 7
  mitte = 5;
  positionX: TAnordnung = (0, mitte, mitte, mitte - 2, mitte + 2, mitte - 3,
    mitte - 1, mitte + 1, mitte + 3);
  lin: TAnordnung = (0, ioLine, zLine + 4, zLine + 8, zLine + 8,
    zLine + 12, zLine + 12, zLine + 12, zLine + 12);

{  // für heapmax = 15
  mitte =7;
  positionX : TAnordnung = (mitte,mitte,mitte-4,mitte+4,mitte-6,mitte-2,mitte+2,mitte+6,mitte-7,mitte-5,mitte-3,mitte-1,mitte+1,mitte+3,mitte+5,mitte+7);
  lin : TAnordnung = (ioLine,zLine+4,zLine+8,zLine+8,zLine+12,zLine+12,zLine+12,zLine+12,zLine+15,zLine+15,zLine+15,zLine+15);
  }
  einP: byte = mitte + 1;
  ausP: byte = mitte - 1;
  Farbe: array[0..8] of TColor =
    (clWhite, clBlack, clRed, clBlue, clGreen, clYellow, clGray, clFuchsia, clTeal);

type

  { TFormBandSort }

  TFormBandSort = class(TForm)
    BitBtnAnimationWeiter: TBitBtn;
    LabelHeapGroesse: TLabel;
    EditInfo: TEdit;
    LabelMaxBandLang: TLabel;
    PanelCommand: TPanel;
    PanelSteuerung: TPanel;
    PanelOben: TPanel;
    ButtonNeuesFeld: TButton;
    RadioGroupListArt: TRadioGroup;
    CheckBoxAuto: TCheckBox;
    PanelAuswahl: TPanel;
    SpinEditBandGroesse: TSpinEdit;
    LabelVerzoegerung: TLabel;
    SpinEditPause: TSpinEdit;
    SpinEditHeapSize: TSpinEdit;
    LabelHeapGr: TLabel;
    PanelUnten: TPanel;
    BitBtnSchliessen: TBitBtn;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    CheckBoxSplitinTwoFiles: TCheckBox;
    BitBtnAnimationEnde: TBitBtn;
    BitBtnAnimationPause: TBitBtn;
    BitBtnAnimationStarten: TBitBtn;
    procedure BitBtnAnimationWeiterClick(Sender: TObject);
    procedure ButtonNeuesFeldClick(Sender: TObject);
    procedure BitBtnAnimationStartenClick(Sender: TObject);
    procedure BitBtnAnimationEndeClick(Sender: TObject);
    procedure SpinEditHeapSizeChange(Sender: TObject);
    procedure SpinEditBandGroesseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditPauseChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RadioGroupListArtClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure CheckBoxSplitinTwoFilesClick(Sender: TObject);
    procedure BitBtnAnimationPauseClick(Sender: TObject);
  private
    { Private-Deklarationen }
    listArt: TListenTyp;
    zahlenfeld: TFeld;
    blattSizeX, chSizeX, eingabeBandPos, ausgabeBandPos: integer;
    maxFeld, heapAnzElemente, Count, heapHoehe, aktHeap, th, offsetX: integer;
    zeig: array[0..bandanzahl] of feldnr;
    neueof: array[0..bandanzahl] of boolean;
    bandArr: TBaender;
    fEin0, fEin1, fAus0, fAus1: string;
    lastElement: numStr;
    pbResized: boolean;
    blatt: TPosition;
    procedure BewegeBand(var band: string; z: numStr; nr: tapeNr; eing: boolean);
    procedure CalcHoeheWeite;
    procedure Distribute;
    procedure EnableCommands(b: boolean);
    procedure InitDistribute;
    procedure LiesVonBaendern(b1, b2: tapenr);
    procedure Line(x0, y0, x1, y1: integer);
    procedure LockBlatt(l: integer);
    procedure LoeschBlatt(i: integer);
    procedure LoeschZahl(xp, yp: integer);
    procedure NeuZeichnen;
    procedure NewBlatt(s: numStr; i: integer);
    procedure SchreibOutputTape(i: tapenr; mitHeap: boolean);
    procedure SetDirektion(i: integer);
    procedure SetzePfeil(i: integer);
    procedure SetzeZurueck(i: tapenr);
    procedure ShowBlattInaktiv(i: integer);
    procedure VergleicheUndZeige(n1, n2: integer; farbe: TColor; wdh, ms: integer);
    procedure VerschiebeVonNach(n1, n2: integer; direkt: boolean);
    procedure VerschiebeXYVonNach(n1, n2: integer);
    procedure WechselBaender(var band2_Nr: tapeNr);
    procedure ZeigZahlenfeldInfo;
  public
    { Public-Deklarationen }
  end;

var
  FormBandSort: TFormBandSort;

implementation

{$R *.lfm}

{ TForm1 }

var
  inputStr, outputStr: string;

procedure TFormBandSort.BewegeBand(var band: string; z: numStr; nr: tapeNr;
  eing: boolean);
const
  faktorEin = 4;
  faktorAus = 2;
var
  xBand, yBand, xBlatt, yBlatt, start, wBand, k, xp, yp, stepX, stepy,
  wElement: integer;
  s: string;
  bitMapBand, bitmapElement, bitMapHG: TBitMap;
begin
  xBlatt := blatt[0].x + 2;
  yBlatt := blatt[0].y;
  yBand := oLine * th;
  if nr > 1 then
    yBand := yBand + th + th div 2;
  stepy := (yBlatt - yBand) div chSizeX;
  if eing then
  begin
    xBand := eingabeBandPos;
    stepX := abstand + 2;
    s := band + '    ';
    s[2] := ' ';
    s[3] := ' ';
  end
  else
  begin
    xBand := ausgabeBandPos;
    stepX := abstand;
    s := band + ' ';
  end;
  wBand := paintbox1.canvas.TextWidth(s);
  bitMapBand := TBitmap.Create;
  bitMapBand.Width := wBand;
  bitMapBand.Height := th;
  bitMapBand.Canvas.Font.Assign(paintbox1.Canvas.Font);
  bitMapBand.Canvas.TextOut(1, 1, s);

  wElement := paintbox1.canvas.TextWidth(z);
  bitmapElement := TBitmap.Create;
  bitmapElement.Width := wElement;
  bitmapElement.Height := th;
  bitmapElement.Canvas.Font.Assign(paintbox1.Canvas.Font);
  bitmapElement.Canvas.TextOut(1, 1, z);

  bitMapHG := TBitmap.Create;
  bitMapHG.Width := wElement;
  bitMapHG.Height := th;
  xp := xBand;
  yp := yBand;

  if eing then
    with paintbox1.canvas do
    begin
      k := 1;
      bitBlt(handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, whiteness);
      while (k <= chSizeX) and not abbruch do
      begin
        if k > 1 then
          bitBlt(handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy);
        xp := xp - stepx;
        yp := yp + stepy;
        bitBlt(BitMapHG.canvas.handle, 0, 0, wElement, th,
          handle, xp, yp, SrcCopy);
        bitBlt(handle, xBand, yBand, wBand, th, BitMapBand.canvas.handle,
          k * faktorEin, 0, SrcCopy);
        bitBlt(handle, xp, yp, wElement, th, BitMapElement.canvas.handle,
          0, 0, SrcCopy);
        WarteMS(pause);
        k := k + 1;
      end;
      if not abbruch then
      begin
        bitBlt(handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy);
        NewBlatt(z, 0);
        bitBlt(handle, xBand, yBand, wBand, th, BitMapBand.canvas.handle,
          blattSizeX + chSizeX div 2, 0, SrcCopy);
      end;
    end
  else
    with paintbox1.canvas do
    begin
      ShowBlattInaktiv(0);
      start := xBand - wBand;
      k := 1;
      while (k <= chSizeX) and not abbruch do
      begin
        if k > 1 then
          bitBlt(Handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy);
        xp := xBlatt - k * stepx;
        yp := yBlatt - k * stepy;
        bitBlt(BitMapHG.canvas.handle, 0, 0, wElement, th,
          Handle, xp, yp, SrcCopy);
        bitBlt(Handle, xp, yp, wElement, th, BitMapElement.canvas.handle,
          0, 0, SrcCopy);
        bitBlt(Handle, start - k * faktorAus, yBand, wBand, th,
          BitMapBand.canvas.handle, 0, 0, SrcCopy);
        WarteMS(pause);
        k := k + 1;
      end;
      if not abbruch then
      begin
        bitBlt(handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy);
        s := s + z + ' ';
        start := xBand - paintbox1.canvas.TextWidth(s);
        paintbox1.canvas.TextOut(start, yBand + 1, s);
      end;
    end;
  bitMapHG.Free;
  bitmapElement.Free;
  bitMapBand.Free;
  if eing then
    Delete(s, 1, abstand);
  band := s;
end;

procedure TFormBandSort.BitBtnAnimationEndeClick(Sender: TObject);
begin
  abbruch := True;
end;

procedure TFormBandSort.BitBtnAnimationStartenClick(Sender: TObject);
begin
  EditInfo.Text := '';
  with paintBox1.Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Color := clWhite;
    Rectangle(0, 0, paintbox1.Width, paintbox1.Height);
  end;
  heapAnzElemente := aktheap;
  heapHoehe := heapAnzElemente div 2;
  EnableCommands(False);
  InitDistribute;
  Distribute;
  EnableCommands(True);
end;

procedure TFormBandSort.BitBtnAnimationPauseClick(Sender: TObject);
begin
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := False;
  BitBtnAnimationWeiter.Enabled := True;
  {$EndIf}
  showSteps := False;
  checkPause(0);
end;

procedure TFormBandSort.BitBtnAnimationWeiterClick(Sender: TObject);
begin
  showSteps := True;
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := True;
  BitBtnAnimationWeiter.Enabled := False;
  {$EndIf}
end;

procedure TFormBandSort.ButtonNeuesFeldClick(Sender: TObject);
var
  b: boolean;
begin
  if CheckBoxAuto.Checked then
    InitZahlenFeld(listArt, maxFeld, zahlenFeld)
  else
  begin
    ZahlenfeldDlg := TZahlenfeldDlg.Create(self);
    ZahlenfeldDlg.SetAnzahl(maxFeld);
    ZahlenFeldDlg.SetZahlen(zahlenfeld);
    ZahlenFeldDlg.SetSplit(CheckBoxSplitinTwoFiles.Checked);
    ZahlenFeldDlg.SetWeite(clientwidth);
    ZahlenFeldDlg.SetHeap(aktheap);
    b := (ZahlenFeldDlg.ShowModal = mrOk);
    if b then
    begin
      ZahlenFeldDlg.GetZahlen(zahlenfeld);
      if maxFeld <> ZahlenFeldDlg.GetAnzahl then
      begin
        maxFeld := ZahlenFeldDlg.GetAnzahl;
        SpinEditBandGroesse.Value := maxFeld;
      end;
      if aktheap <> ZahlenFeldDlg.GetHeap then
        SpinEditHeapSize.Value := ZahlenFeldDlg.GetHeap;
      if CheckBoxSplitinTwoFiles.Checked <> ZahlenFeldDlg.GetSplit then
        CheckBoxSplitinTwoFiles.Checked := ZahlenFeldDlg.GetSplit;
    end;
    ZahlenfeldDlg.Free;
    if not b then
      exit;
  end;
  InitDistribute;
  ZeigZahlenfeldInfo;
  NeuZeichnen;
  BitBtnAnimationStarten.Enabled := True;
end;

procedure TFormBandSort.CalcHoeheWeite;
var
  w, h, fh, oldw, oldh, xw, k, tempOffsetX: integer;
  ok: boolean;
begin
  oldw := chSizeX;
  oldh := th;
  h := self.ClientHeight div (lin[heapmax] + 2 + 1);
  fh := h - 3;
  xw := panel1.ClientWidth;
  w := (positionX[heapmax] + 1);
  ok := False;
  repeat
    Paintbox1.Font.Height := fh;
    Paintbox1.Canvas.Font.Height := fh;
    if w * Paintbox1.Canvas.TextWidth('4567') > xw then
      fh := fh - 1
    else
      ok := True;
  until ok or (fh < 8);
  chSizeX := Paintbox1.Canvas.TextWidth('4567') div 4;
  h := fh + 3;
  pbResized := (oldw <> chSizeX) or (oldh <> h);
  blattSizeX := abstand * chSizeX;
  th := fh + 3;
  EditInfo.Font.Height := fh;
  tempOffsetX := (xw div 2) - mitte * blattSizeX;
  for k := 0 to heapmax do
  begin
    blatt[k].x := positionX[k] * blattSizeX + tempOffsetX;
    blatt[k].y := lin[k] * th;
  end;
  eingabeBandPos := (mitte + 1) * blattSizeX + tempOffsetX + 2 * chSizeX + 2;
  ausgabeBandPos := (mitte - 1) * blattSizeX + tempOffsetX;
  offsetX := (xw div 2) - (mitte * blattSizeX - chSizeX - (chSizeX div 2));
end;

procedure TFormBandSort.CheckBoxSplitinTwoFilesClick(Sender: TObject);
begin
  if fEin0 <> '' then
  begin
    InitDistribute;
    ZeigZahlenfeldInfo;
    NeuZeichnen;
  end;
end;

procedure TFormBandSort.Distribute;
var
  tape, no, nr, ne1: tapenr;
  istSortiert: boolean;

  procedure ChangeOutputTape;
  begin
    Count := Count + 1;
    EditInfo.Text := ' ' + sprachTab[s_Lauf] + ' : ' + IntToStr(Count);
    if nr = 1 then
      nr := 3
    else
      nr := 1;
    istSortiert := False;
  end;

  procedure DistributeOhneHeap;
  var
    z: numStr;
  begin
    LiesVonBaendern(no, ne1);
    while (((no >= 0) and not (neueof[no])) or ((ne1 >= 0) and not (neueof[ne1]))) and
      not abbruch do
    begin
      z := blatt[0].wert;
      SchreibOutputTape(nr, False);
      LiesVonBaendern(no, ne1);
      if z > blatt[0].wert then
        ChangeOutputTape;
    end; {while}
    if not abbruch then
      SchreibOutputTape(nr, False);
  end;

  procedure Sift(li, re: integer);
  var
    i, j, hilfPos: integer;
    weiter: boolean;
    hilf: numStr;
  begin
    if abbruch then
      exit;
    SetzePfeil(0);
    paintbox1.Canvas.Pen.Color := Farbe[1];
    if li = re then
      exit;
    i := li;
    j := 2 * i;
    hilf := blatt[i].wert;
    hilfPos := i;
    self.blatt[-1] := blatt[i];
    weiter := (j <= re);
    if not weiter then
      exit;
    while weiter and not abbruch do
    begin
      if j < re then
      begin
        VergleicheUndZeige(j, j + 1, DemoFarb[vergleichen], 6, pause);
        if (blatt[j].wert > blatt[j + 1].wert) then
          j := j + 1;
      end;
      VergleicheUndZeige(-1, j, DemoFarb[vergleichen], 6, pause);
      if hilf > blatt[j].wert then
      begin
        if hilfpos = i then
        begin
          LoeschBlatt(-1);
          blatt[-1].x := blatt[j].x;
          VerschiebeXYVonNach(i, -1);
        end;
        VerschiebeVonNach(j, i, True);
        i := j;
        j := 2 * i;
        weiter := (j <= re);
      end
      else
        weiter := False;
    end;
    LoeschBlatt(-1);
    if hilfpos <> i then
      VerschiebeXYVonNach(-1, i);
    NewBlatt(hilf, i);
  end;

var
  k, li, re: heapnr;
  z: numStr;
begin
  no := 0;
  if fEin1 = '' then
    ne1 := -1
  else
    ne1 := 2;
  repeat
    // EingabeBänder zeigen
    for k := 0 to heapAnzElemente do
      blatt[k].wert := leer;
    NeuZeichnen;
    Count := 0;
    nr := 0;
    ChangeOutputTape;

    for tape := 0 to bandanzahl do
      SetzeZurueck(tape);
    istSortiert := True;
    lastElement := leer;
    if heapAnzElemente < 1 then
      DistributeOhneHeap
    else
    begin
      //Endblätter des Heap belegen
      li := heapAnzElemente;
      repeat
        LiesVonBaendern(no, ne1);
        VerschiebeXYVonNach(0, li);
        li := Pred(li)
      until (li = heapHoehe) or abbruch;
      // Heap weiter füllen und kleinstes Element nach oben
      if heapHoehe > 0 then
        repeat
          LiesVonBaendern(no, ne1);
          VerschiebeXYVonNach(0, li);
          sift(li, heapAnzElemente);
          li := Pred(li)
        until (li = 0) or abbruch;

      li := heapAnzElemente;
      while (((no >= 0) and not (neueof[no])) or ((ne1 >= 0) and not (neueof[ne1]))) and
        not abbruch do
      begin
        z := blatt[1].wert;
        SchreibOutputTape(nr, True);
        LiesVonBaendern(no, ne1);
        if blatt[1].wert <= blatt[0].wert then
        begin
          VerschiebeXYVonNach(0, 1);
          sift(1, li);
        end
        else
        begin
          VerschiebeXYVonNach(li, 1);
          LockBlatt(li);
          sift(1, Pred(li));
          VerschiebeXYVonNach(0, li);
          if li <= heapHoehe then
            sift(li, heapAnzElemente);
          li := Pred(li);
          if li = 0 then
          begin
            for k := 1 to heapAnzElemente do
              LockBlatt(k);
            li := heapAnzElemente;
            if blatt[1].wert < z then
              ChangeOutputTape;
          end;
        end;
      end; {while}
      re := heapAnzElemente;
      if not abbruch then
        repeat
          SchreibOutputTape(nr, True);
          VerschiebeXYVonNach(li, 1);
          sift(1, Pred(li));
          if re <> li then
          begin
            LockBlatt(re);
            VerschiebeXYVonNach(re, li);
            LockBlatt(li);
          end;
          re := Pred(re);
          if li <= heapHoehe then
            sift(li, re);
          li := Pred(li)
        until (li = 0) or abbruch;
      if (re > 0) and not abbruch then
      begin
        ChangeOutputTape;
        for k := 1 to re do
          LockBlatt(k);
        while (re > 0) and not abbruch do
        begin
          SchreibOutputTape(nr, True);
          if re > 1 then
          begin
            VerschiebeXYVonNach(re, 1);
            sift(1, Pred(re));
          end
          else
            ShowBlattInaktiv(1);
          re := Pred(re);
        end;
      end;
    end;
    if (not abbruch) and (not istSortiert) then
      WechselBaender(ne1);
  until istSortiert or abbruch;
  if istSortiert then
    EditInfo.Text := fAus0;
  abbruch := True;
end;

procedure TFormBandSort.EnableCommands(b: boolean);
begin
  PanelOben.Enabled := b;
  PanelAuswahl.Enabled := b;
  BitBtnAnimationStarten.Enabled := b;
  BitBtnAnimationEnde.Enabled := not b;
  BitBtnAnimationPause.Enabled := not b;
  {$IfNDef Windows}
  BitBtnAnimationPause.Enabled := not b;
  {$EndIf}
  BitBtnSchliessen.Enabled := b;
end;

procedure TFormBandSort.FormCreate(Sender: TObject);
begin
  aktheap := 7;
  heapAnzElemente := 7;
  heapHoehe := 3;
  pause := 200;
  maxFeld := 20;
  self.Caption := sprachTab[s_DemoBandSortierung];
  ButtonNeuesFeld.Caption := sprachTab[s_ZahlenfeldErzeugen];
  CheckBoxSplitinTwoFiles.Caption := sprachTab[s_SplitteZahlenfeld];
  RadioGroupListArt.Caption := sprachTab[s_ListenArt];
  RadioGroupListArt.Items[0] := sprachTab[s_normal];
  RadioGroupListArt.Items[1] := sprachTab[s_ohneDoppel];
  RadioGroupListArt.Items[2] := sprachTab[s_sortiert];
  RadioGroupListArt.Items[3] := sprachTab[s_umgekehrt];
  LabelMaxBandLang.Caption := sprachTab[s_maxBandlang];
  LabelHeapGroesse.Caption := sprachTab[s_HeapGroesse];
  LabelVerzoegerung.Caption := sprachTab[s_Verzoegerung];
  BitBtnAnimationStarten.Caption := sprachTab[s_AniStarten];
  BitBtnAnimationPause.Caption := sprachTab[s_AniAnhalten];
  BitBtnAnimationEnde.Caption := sprachTab[s_AniAbbrechen];
  BitBtnSchliessen.Caption := sprachTab[s_Schliessen];
  inputStr := '<<==== ' + sprachTab[s_Ein] + ' =======';
  outputStr := '<<==== ' + sprachTab[s_Aus] + ' =======';
end;

procedure TFormBandSort.FormResize(Sender: TObject);
begin
  CalcHoeheWeite;
end;

procedure TFormBandSort.InitDistribute;
var
  n, k, g: integer;
  s: string;
  split: boolean;
begin
  abbruch := False;
  fAus0 := '';
  fAus1 := '';
  fEin0 := '';
  fEin1 := '';
  lastElement := leer;
  split := CheckBoxSplitinTwoFiles.Checked and (maxFeld > 1);
  g := maxfeld div 2;
  for k := 0 to bandanzahl do
    for n := 1 to maxFeldAnz do
      bandArr[k, n] := leer;
  for n := 1 to maxfeld do
  begin
    s := IntToStr(zahlenfeld[n]);
    if Length(s) < 2 then
      s := ' ' + s;
    if split and (n > g) then
    begin
      bandArr[2, n - g] := s;
      fEin1 := fEin1 + ' ' + s + ' ';
    end
    else
    begin
      bandArr[0, n] := s;
      fEin0 := fEin0 + ' ' + s + ' ';
    end;
  end;
  for n := maxFeld + 1 to maxfeldAnz do
  begin
    if split and (n > g) then
      fEin1 := fEin1 + ' ' + leer + ' '
    else
      fEin0 := fEin0 + ' ' + leer + ' ';
  end;
  for k := -1 to heapMax do
    blatt[k].wert := leer;
end;

procedure TFormBandSort.LiesVonBaendern(b1, b2: tapenr);
var
  z1, z2, z0, z: numStr;
  band1: boolean;
  x, y1, y2, k: integer;
  old: TColor;
begin
  if (b1 >= 0) and not neuEof[b1] then
    z1 := bandArr[b1, zeig[b1]]
  else
    z1 := 'zz';
  if (b2 >= 0) and not neuEof[b2] then
    z2 := bandArr[b2, zeig[b2]]
  else
    z2 := 'zz';

  if (z1 <> 'zz') and (z2 <> 'zz') then
    with paintBox1.Canvas do // zeige Vergleich  bei 2 belegten Baendern
    begin
      x := eingabeBandPos + 2 * chSizeX + 5;
      y1 := oLine * th + (3 * th) div 4;
      y2 := y1 + (3 * th) div 4;
      old := Pen.color;
      pen.Width := 2;
      k := 1;
      repeat
        if odd(k) then
          Pen.color := DemoFarb[vergleichen]
        else
          Pen.color := clWhite;
        polyline([point(x, y1), point(x - 4, y1 + 4), point(x + 4, y1 + 4),
          point(x, y1), point(x, y2), point(x - 4, y2 - 4),
          point(x + 4, y2 - 4), point(x, y2)]);
        WarteMS(4 * pause);
        k := k + 1;
      until (k > 6) or abbruch;
      Pen.color := old;
      pen.Width := 1;
    end;

  if z1 <= z2                                // Bandbewegung simulieren
  then
  begin
    z := z1;
    z0 := z2;
    band1 := True;
  end
  else
  begin
    z := z2;
    z0 := z1;
    band1 := False;
  end;
  if (z < lastElement) and (z0 <> 'zz') and (z0 >= lastElement) then
  begin
    z := z0;
    band1 := not band1;
  end;
  if heapAnzElemente > 0 then
    SetzePfeil(-1);
  setDirektion(0);
  if band1 then
  begin
    BewegeBand(fEin0, z, b1, ein);
    zeig[b1] := zeig[b1] + 1;
    neueof[b1] := (zeig[b1] > maxfeldAnz) or (bandArr[b1, zeig[b1]] = leer);
  end
  else
  begin
    BewegeBand(fEin1, z, b2, ein);
    zeig[b2] := zeig[b2] + 1;
    neueof[b2] := (zeig[b2] > maxfeldAnz) or (bandArr[b2, zeig[b2]] = leer);
  end;
  NewBlatt(z, 0);
  blatt[0].wert := z;
end;

procedure TFormBandSort.Line(x0, y0, x1, y1: integer);
begin
  paintbox1.Canvas.MoveTo(x0, y0);
  paintbox1.Canvas.LineTo(x1, y1);
end;

procedure TFormBandSort.LockBlatt(l: integer);
var
  xpos, ypos: integer;
begin
  xPos := blatt[l].x - chSizeX div 2;
  ypos := blatt[l].y - 5;
  paintbox1.Canvas.Pen.Mode := pmXor;
  paintbox1.Canvas.Rectangle(xPos, yPos, xPos + blattSizeX - 4, yPos + 4);
  paintbox1.Canvas.Rectangle(xPos, yPos + 5, xPos + 4, yPos + th);
  paintbox1.Canvas.Pen.Mode := pmCopy;
end;

procedure TFormBandSort.LoeschZahl(xp, yp: integer);
begin
  paintBox1.Canvas.Rectangle(xp, yp, xp + 3 * chSizeX, yp + th);
end;

procedure TFormBandSort.LoeschBlatt(i: integer);
var
  xp, yp: integer;
begin
  paintBox1.Canvas.pen.Color := clWhite;
  xp := blatt[i].x;
  yp := blatt[i].y;
  LoeschZahl(xp, yp);
end;

procedure TFormBandSort.NeuZeichnen;
var
  n, yp0, yp1, start: integer;
  x, y: TAnordnung;
begin
  x := Default(TAnordnung);
  y := Default(TAnordnung);
  with paintBox1.Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Color := clWhite;
    Rectangle(0, 0, paintbox1.Width, paintbox1.Height);
    Pen.Color := clBlack;

    TextOut(1, 2, outputStr);
    start := paintbox1.Width - paintbox1.canvas.TextWidth(inputStr);
    TextOut(start, 2, inputStr);

    yp0 := oLine * th;
    yp1 := yp0 + th + th div 2;

    TextOut(eingabeBandPos, yp0, fEin0);
    if fEin1 <> '' then
      TextOut(eingabeBandPos, yp1, fEin1);

    if fAus0 <> '' then
    begin
      start := ausgabeBandPos - TextWidth(fAus0);
      TextOut(start, yp0, fAus0);
    end;
    if fAus1 <> '' then
    begin
      start := ausgabeBandPos - TextWidth(fAus1);
      TextOut(start, yp1, fAus1);
    end;

    for n := 1 to heapAnzElemente do
    begin
      x[n] := blatt[n].x + 3 * chSizeX div 2;
      y[n] := blatt[n].y + th - 3;
    end;
    Pen.Color := Farbe[2];
  end;
  for n := heapAnzElemente downto 2 do
    Line(x[n], y[n], x[n div 2], y[n div 2]);

  NewBlatt(blatt[0].wert, 0);
  for n := 1 to heapAnzElemente do
    NewBlatt(blatt[n].wert, n);
end;

procedure TFormBandSort.NewBlatt(s: numStr; i: integer);
var
  xp, yp: integer;
begin
  paintBox1.Canvas.pen.Color := Farbe[4];
  xp := blatt[i].x;
  yp := blatt[i].y;
  LoeschZahl(xp, yp);
  if s <> leer then
    paintBox1.Canvas.TextOut(xp + 2, yp + 2, s);
end;

procedure TFormBandSort.PaintBox1Paint(Sender: TObject);
begin
  NeuZeichnen;
end;

procedure TFormBandSort.RadioGroupListArtClick(Sender: TObject);
begin
  listArt := TListenTyp(RadiogroupListArt.ItemIndex);
end;

procedure TFormBandSort.SchreibOutputTape(i: tapenr; mitHeap: boolean);
var
  z: numStr;
begin
  setDirektion(1);
  if mitHeap then
  begin
    z := blatt[1].wert;
    SetzePfeil(1);
    VerschiebeXYVonNach(1, 0);
  end
  else
    z := blatt[0].wert;
  bandArr[i, zeig[i]] := z;
  lastElement := z;
  if i = 1 then
    BewegeBand(fAus0, z, i, aus)
  else
    BewegeBand(fAus1, z, i, aus);
  ShowBlattInaktiv(0);
  zeig[i] := zeig[i] + 1;
end;

procedure TFormBandSort.SetDirektion(i: integer);
var
  startX: integer;
begin
  startX := blatt[0].x + 3 * chSizeX div 2;
  paintbox1.Canvas.Pen.Color := Farbe[i];
  Line(startX, ioLine * th - 2, startX - 3 * chSizeX, zLine * th + 2);
  paintbox1.Canvas.Pen.Color := Farbe[1 - i];
  Line(startX, ioLine * th - 2, startX + 3 * chSizeX, zLine * th + 2);
end;

procedure TFormBandSort.SetzePfeil(i: integer);
var
  xp, yp, z: integer;
begin
  xp := blatt[0].x + 3 * chSizeX div 2;
  yp := (zLine + 3) * th + 2;
  paintbox1.Canvas.Pen.Color := clWhite;
  paintbox1.Canvas.Rectangle(xp - 6, yp - th + 2, xp + 6, yp + th - 4);
  if i <> 0 then
    with paintbox1.Canvas do
    begin
      z := i * (th - 4);
      Pen.Width := 2;
      Pen.Color := DemoFarb[verschiebung];
      MoveTo(xp - 5, yp);
      LineTo(xp, yp - z);
      LineTo(xp + 5, yp);
      lineTo(xp, yp - z);
      LineTo(xp, yp + z);
      Pen.Width := 1;
    end;
end;

procedure TFormBandSort.SetzeZurueck(i: tapenr);
begin
  zeig[i] := 1;
  neuEof[i] := False;
end;

procedure TFormBandSort.ShowBlattInaktiv(i: integer);
begin
  with paintBox1.Canvas do
  begin
    Font.Color := DemoFarb[inaktiv];
    TextOut(blatt[i].x + 2, blatt[i].y + 2, blatt[i].wert);
    Font.Color := clBlack;
  end;
end;

procedure TFormBandSort.SpinEditBandGroesseChange(Sender: TObject);
begin
  try
    maxFeld := SpinEditBandGroesse.Value;
  finally
  end;
end;

procedure TFormBandSort.SpinEditHeapSizeChange(Sender: TObject);
begin
  try
    aktheap := SpinEditHeapSize.Value;
    heapAnzElemente := aktheap;
    NeuZeichnen;
  finally
  end;
end;

procedure TFormBandSort.SpinEditPauseChange(Sender: TObject);
begin
  try
    pause := SpinEditPause.Value;
  finally
  end;
end;

procedure TFormBandSort.VergleicheUndZeige(n1, n2: integer; farbe: TColor;
  wdh, ms: integer);
var
  mx1, mx2, my1, my2, k: integer;
  pfeilR, pfeilL: boolean;
  old: TColor;

  procedure PfeilHoritontal(x, y, p: integer);     // p>0 : spitze nach rechts
  begin
    paintbox1.canvas.polyline(
      [point(x, y), point(x - p, y + p), point(x - p, y - p), point(x, y)]);
  end;

  procedure PfeilVertikal(x, y, p: integer);       // p>0 : spitze nach oben
  begin
    paintbox1.canvas.polyline(
      [point(x, y), point(x - p, y + p), point(x + p, y + p), point(x, y)]);
  end;

begin
  if abbruch or (n1 = n2) then
    exit;
  if n1 > n2 then
  begin
    k := n1;
    n1 := n2;
    n2 := k;
  end;

  mx1 := blatt[n1].x + 2 * chSizeX;
  my1 := blatt[n1].y + th div 2;

  mx2 := blatt[n2].x + 2 * chSizeX;
  my2 := blatt[n2].y + th div 2;
  pfeilR := False;
  pfeilL := False;
  if my1 = my2 then
  begin
    mx1 := mx1 + chSizeX + 2;
    mx2 := mx2 - 2 * chSizeX - 4;
    pfeilR := True;
    pfeilL := True;
  end
  else
  begin
    if my1 < my2 then
    begin
      my2 := my2 - th div 2 - 8;
      if mx1 < mx2 then
      begin
        mx1 := mx1 + 2 * chSizeX + 2;
        pfeilL := True;
      end
      else
      begin
        mx1 := mx1 - 2 * chSizeX - 4;
        pfeilR := True;
      end;
    end;
  end;
  old := paintbox1.canvas.Pen.color;
  paintbox1.canvas.pen.Width := 2;
  if ms = 0 then
    wdh := wdh - 2;
  k := 1;
  repeat
    if odd(k) then
      paintbox1.canvas.Pen.color := farbe
    else
      paintbox1.canvas.Pen.color := clWhite;
    if pfeilR then
    begin
      if pfeilL then
      begin
        PfeilHoritontal(mx1, my1, -3);
        PfeilHoritontal(mx2, my2, 3);
      end
      else
      begin
        PfeilHoritontal(mx1, my1, 3);
        PfeilVertikal(mx2, my2, -3);
      end;
    end
    else
    begin
      PfeilHoritontal(mx1, my1, -3);
      PfeilVertikal(mx2, my2, -3);
    end;

    paintbox1.canvas.polyline([point(mx1, my1), point(mx2, my1), point(mx2, my2)]);
    WarteMS(2 * ms);
    k := k + 1;
  until (k > wdh) or abbruch;
  paintbox1.canvas.Pen.color := old;
  paintbox1.canvas.pen.Width := 1;
end;

procedure TFormBandSort.VerschiebeVonNach(n1, n2: integer; direkt: boolean);
var
  mx1, mx2, my1, my2, zeit, wElement, stepX, stepY, xp, yp: integer;
  bitmapElement, bitMapHG: TBitMap;
  z: numStr;
  ok: boolean;
begin
  if abbruch or (n1 = n2) then
    exit;
  mx1 := blatt[n1].x;
  my1 := blatt[n1].y;
  mx2 := blatt[n2].x;
  my2 := blatt[n2].y;
  z := blatt[n1].wert;
  wElement := paintbox1.canvas.TextWidth(z);
  bitmapElement := TBitmap.Create;
  bitmapElement.Width := wElement;
  bitmapElement.Height := th;
  bitmapElement.Canvas.Pen.Color := clWhite;
  bitmapElement.Canvas.brush.Color := clWhite;
  bitmapElement.Canvas.Rectangle(0, 0, wElement, th);
  bitmapElement.Canvas.Pen.Color := clBlack;
  bitmapElement.Canvas.Font.Assign(paintbox1.Canvas.Font);
  bitmapElement.Canvas.TextOut(1, 1, z);

  bitMapHG := TBitmap.Create;
  bitMapHG.Width := wElement;
  bitMapHG.Height := th;
  ok := False;

  if n1 >= 0 then
    ShowBlattInaktiv(n1);
  stepX := (mx2 - mx1) div chSizeX;
  stepY := (my2 - my1) div chSizeX;
  xp := mx1;
  yp := my1;

  with paintbox1.Canvas do
    if direkt then
      while (xp <> mx2) and (yp <> my2) and not abbruch do
      begin
        if ok then
          bitBlt(Handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy)
        else
          ok := True;
        xp := xp + stepX;
        if (stepX > 0) then
        begin
          if xp > mx2 then
            xp := mx2;
        end
        else if (stepX < 0) and (xp < mx2) then
          xp := mx2;
        yp := yp + stepY;
        if (stepY > 0) then
        begin
          if yp > my2 then
            yp := my2;
        end
        else if (stepY < 0) and (yp < my2) then
          yp := my2;
        bitBlt(BitMapHG.canvas.handle, 0, 0, wElement, th, Handle, xp, yp, SrcCopy);
        bitBlt(Handle, xp, yp, wElement, th, BitMapElement.canvas.handle, 0, 0, SrcCopy);
        WarteMS(pause);
      end
    else
    begin
      zeit := pause div 2;
      while (xp <> mx2) and not abbruch do
      begin
        if ok then
          bitBlt(Handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy)
        else
          ok := True;
        xp := xp + stepX;
        if (stepX > 0) then
        begin
          if xp > mx2 then
            xp := mx2;
        end
        else if (stepX < 0) and (xp < mx2) then
          xp := mx2;
        bitBlt(BitMapHG.canvas.handle, 0, 0, wElement, th,
          Handle, xp, yp, SrcCopy);
        bitBlt(Handle, xp, yp, wElement, th, BitMapElement.canvas.handle,
          0, 0, SrcCopy);
        WarteMS(zeit);
      end;
      while (yp <> my2) and not abbruch do
      begin
        if ok then
          bitBlt(Handle, xp, yp, wElement, th, BitMapHG.canvas.handle, 0, 0, SrcCopy)
        else
          ok := True;
        yp := yp + stepY;
        if (stepY > 0) then
        begin
          if yp > my2 then
            yp := my2;
        end
        else if (stepY < 0) and (yp < my2) then
          yp := my2;
        bitBlt(BitMapHG.canvas.handle, 0, 0, wElement, th,
          Handle, xp, yp, SrcCopy);
        bitBlt(Handle, xp, yp, wElement, th, BitMapElement.canvas.handle,
          0, 0, SrcCopy);
        WarteMS(zeit);
      end;
    end;
  blatt[n2].wert := z;
  NewBlatt(z, n2);
  bitMapHG.Free;
  bitmapElement.Free;
end;

procedure TFormBandSort.VerschiebeXYVonNach(n1, n2: integer);
begin
  VerschiebeVonNach(n1, n2, False);
end;

procedure TFormBandSort.WechselBaender(var band2_Nr: tapeNr);
var
  k: integer;
  z: string;
begin
  fEin0 := '';
  fEin1 := '';
  fAus0 := '';
  fAus1 := '';
  for k := 1 to maxfeldAnz do
  begin
    z := bandArr[1, k];
    if z = '' then
      z := leer;
    bandArr[0, k] := z;
    bandArr[1, k] := leer;
    if z <> leer then
      fEin0 := fEin0 + ' ' + z + ' ';
    z := bandArr[3, k];
    if z = '' then
      z := leer;
    bandArr[2, k] := z;
    bandArr[3, k] := leer;
    if z <> leer then
      fEin1 := fEin1 + ' ' + z + ' ';
  end;

  if fEin1 = '' then
    band2_Nr := -1
  else
    band2_Nr := 2;
end;

procedure TFormBandSort.ZeigZahlenfeldInfo;
var
  info: string;
begin
  info := fEin0;
  if fEin1 <> '' then
    info := info + ' ## ' + fEin1;
  EditInfo.Text := info;
end;

end.
