unit SortUnitAni;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Hauptmodul des Projekts
   Autor:   H. Niemeyer  (c) 2010 - 2021, 2024
   Version: 1.8

   letzte Änderung: 20.01.2024 *)

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Types, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, Buttons, Menus,
  SortUnitDefinitionen, sortUnitSprache,
  SortZahlenfeldDlg, SortFrame;

const
  vorgabe_feldWeite = 24;
  vorgabe_feldHoehe = 24;
  ofs = 2;
  noBreakPoint = -2;
  hilf1 = 0;
  hilf2 = -1;

type
  TFeldPos = record
    x, y: integer;
    wert: integer;
    fontCol, FrameCol: TColor
  end;

  TPosition = array[hilf2..maxDemoAnzahl] of TFeldPos;

type

  { TSortFormAni }

  TSortFormAni = class(TForm)
    Paintbox1: TPaintBox;
    PanelCommand: TPanel;
    PanelOben: TPanel;
    ButtonNeuesFeld: TButton;
    CheckBoxAuto: TCheckBox;
    RadioGroup1: TRadioGroup;
    LabelFeldGroesse: TLabel;
    SpinEditFeldGroesse: TSpinEdit;
    PanelAuswahl: TPanel;
    mySortFrame: TFrame1;
    PanelUnten: TPanel;
    LabelVerzoegerung: TLabel;
    SpinEditPause: TSpinEdit;
    BitBtnAnimationEnde: TBitBtn;
    BitBtnAnimationPause: TBitBtn;
    BitBtnAnimationWeiter: TBitBtn;
    BitBtnAlgAnimationBeenden: TBitBtn;
    procedure BitBtnAnimationEndeClick(Sender: TObject);
    procedure BitBtnAnimationPauseClick(Sender: TObject);
    procedure BitBtnAnimationWeiterClick(Sender: TObject);
    procedure ButtonNeuesFeldClick(Sender: TObject);
    procedure ButtonSortClick(Sender: TObject);
    procedure CheckBoxAutoClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Paintbox1Paint(Sender: TObject);
    procedure Paintbox1Resize(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEditFeldGroesseChange(Sender: TObject);
    procedure SpinEditPauseChange(Sender: TObject);
  private
    { Private-Deklarationen }
    listArt: TListenTyp;
    zahlenfeld: TFeld;
    lastSortArt: TSortTyp;
    f: TPosition;
    maxFeld, aktPause, feldWeite, feldHoehe: integer;
    demoRunning: boolean;
    breakPointNr, breakNr: longint;
    procedure ButtonsAktivieren(ja: boolean);
    procedure CalcHoeheWeite;
    procedure FontSizeAnpassen;
    procedure InitFeldPosXY(maxF, aktZ: integer);
    procedure NeuZeigenZahlenFeld(feld: TFeld; zeile: integer; von, bis: integer);
    procedure StarteDemo(sortArt: TSortTyp);
    procedure ZeigeZahlenFeld(aCanvas: TCanvas; feld: TFeld; zeile: integer;
      von, bis: integer);
    procedure ZeigeSortDemo(aktPaintbox: TPaintbox; sortArt: TSortTyp);
  public
    { Public-Deklarationen }
  end;

var
  SortFormAni: TSortFormAni;

implementation

{$R *.lfm}

type
  moveRec = record
    vx, vy, nx, ny, zx, zy, sx, sy: integer;
  end;
  TWeg = array[1..3] of moveRec;

procedure TSortFormAni.BitBtnAnimationEndeClick(Sender: TObject);
begin
  abbruch := True;
end;

procedure TSortFormAni.BitBtnAnimationPauseClick(Sender: TObject);
begin
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := False;
  BitBtnAnimationWeiter.Enabled := True;
  {$EndIf}
  showSteps := False;
  CheckPause(0);
end;

procedure TSortFormAni.BitBtnAnimationWeiterClick(Sender: TObject);
begin
  showSteps := True;
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := True;
  BitBtnAnimationWeiter.Enabled := False;
  {$EndIf}
end;

procedure TSortFormAni.ButtonsAktivieren(ja: boolean);
begin
  buttonNeuesFeld.Enabled := ja;
  radioGroup1.Enabled := ja;
  mySortFrame.SortButtonsAktivieren(ja);
  BitBtnAnimationEnde.Enabled := not ja;
  BitBtnAnimationPause.Enabled := not ja;
  {$IfNDef Windows}
  BitBtnAnimationWeiter.Enabled := not ja;
  {$EndIf}
  SpinEditFeldGroesse.Enabled := ja;
  BitBtnAlgAnimationBeenden.Enabled := ja;
  demoRunning := not ja;
end;

procedure TSortFormAni.ButtonNeuesFeldClick(Sender: TObject);
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
    ZahlenFeldDlg.SetWeite(clientwidth);
    b := (ZahlenFeldDlg.ShowModal = mrOk);
    if b then
    begin
      if ZahlenFeldDlg.GetAnzahl <> maxFeld then
        SpinEditFeldGroesse.Value := ZahlenFeldDlg.GetAnzahl;
      ZahlenFeldDlg.GetZahlen(zahlenfeld);
    end;
    ZahlenfeldDlg.Free;
    if not b then
      exit;
  end;
  InitFeldPosXY(maxFeld, 1);
  NeuZeigenZahlenFeld(zahlenFeld, 1, 0, 0);
  ButtonsAktivieren(True);
end;

procedure TSortFormAni.ButtonSortClick(Sender: TObject);
begin
  breakPointNr := noBreakPoint;
  lastSortArt := TSortTyp((Sender as TButton).tag);
  StarteDemo(lastSortArt);
end;

procedure TSortFormAni.CalcHoeheWeite;
var
  w, h, oldw, oldh, maxF: integer;
begin
  oldw := feldWeite;
  oldh := feldHoehe;
  feldWeite := vorgabe_feldWeite;
  feldHoehe := vorgabe_feldHoehe;
  maxF := SpinEditFeldGroesse.Value;
  if maxF < 2 then
    exit;
  w := Paintbox1.ClientWidth div maxF;
  h := Paintbox1.ClientHeight div (maxF + 2);
  if w > h then
    w := h;
  w := (w div 8) * 8;
  if w < vorgabe_feldWeite then
    w := vorgabe_feldWeite;
  h := w;
  feldWeite := w;
  feldHoehe := h;
  if (oldw <> w) or (oldh <> h) then FontSizeAnpassen;
end;

procedure TSortFormAni.CheckBoxAutoClick(Sender: TObject);
begin
  RadioGroup1.Enabled := CheckBoxAuto.Checked;
end;

procedure TSortFormAni.FontSizeAnpassen;
begin
  {$IfDef Windows}
  paintBox1.Font.Height := Round(4 * GetFontSizeFromPixels('24',
    feldWeite - 2, feldHoehe - 2, paintBox1.Font) div 3);
  {$Else}
  paintBox1.Font.Height := GetFontSizeFromPixels('24', feldWeite,
    feldHoehe, paintBox1.Font);
  {$EndIf}
end;

procedure TSortFormAni.FormActivate(Sender: TObject);
var
  ts: TTextStyle;
begin
  aktPause := SpinEditPause.Value;
  maxFeld := SpinEditFeldGroesse.Value;
  listArt := zufall;
  zahlenFeld[1] := -1;
  abbruch := True;
  mySortFrame.InitFrame(True, nil);
  mySortFrame.SortButtonsAktivieren(False);
  feldweite := 0;
  lastSortArt := sMin;
  ts := paintBox1.Canvas.TextStyle;
  ts.Alignment := taCenter;
  ts.Layout := tlCenter;
  paintBox1.Canvas.TextStyle := ts;
  CalcHoeheWeite;
  InitZahlenFeld(listArt, maxFeld, zahlenFeld);
  ButtonsAktivieren(True);
  paintBox1.Repaint;
end;

procedure TSortFormAni.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_SortierDemo];
  ButtonNeuesFeld.Caption := sprachTab[s_ZahlenfeldErzeugen];
  RadioGroup1.Caption := sprachTab[s_ListenArt];
  RadioGroup1.Items[0] := sprachTab[s_normal];
  RadioGroup1.Items[1] := sprachTab[s_ohneDoppel];
  RadioGroup1.Items[2] := sprachTab[s_sortiert];
  RadioGroup1.Items[3] := sprachTab[s_umgekehrt];
  LabelFeldGroesse.Caption := sprachTab[s_FeldGroesse];
  LabelVerzoegerung.Caption := sprachTab[s_Verzoegerung];
  BitBtnAnimationPause.Caption := sprachTab[s_AniAnhalten];
  BitBtnAnimationweiter.Caption := sprachTab[s_AniFortsetzen];
  BitBtnAnimationEnde.Caption := sprachTab[s_AniAbbrechen];
  BitBtnAlgAnimationBeenden.Caption := sprachTab[s_AniSchliessen];
  demoRunning := False;
end;

procedure TSortFormAni.InitFeldPosXY(maxF, aktZ: integer);
var
  k, y0: integer;
begin
  y0 := (aktZ - 1) * feldHoehe;
  for k := 1 to maxF do
    with f[k] do
    begin
      x := (k - 1) * feldWeite;
      y := y0;
      if aktZ = 1 then
      begin
        wert := zahlenfeld[k];
        fontCol := clBlack;
        frameCol := clBlack;
      end;
    end;
  f[hilf1].x := 0;
  f[hilf1].y := y0;
  f[hilf2].x := 0;
  f[hilf2].y := y0;
end;

procedure TSortFormAni.NeuZeigenZahlenFeld(feld: TFeld; zeile: integer;
  von, bis: integer);
begin
  with Paintbox1.Canvas do
  begin
    pen.color := clWhite;
    rectangle(0, (zeile - 1) * feldHoehe, Paintbox1.Width, Paintbox1.Height);
    pen.color := clBlack;
  end;
  Application.ProcessMessages;
  ZeigeZahlenFeld(Paintbox1.canvas, feld, zeile, von, bis);
end;

procedure TSortFormAni.Paintbox1Paint(Sender: TObject);
begin
  if not demoRunning then
  begin
    FontSizeAnpassen;
    InitFeldPosXY(maxFeld, 1);
    Paintbox1.Canvas.pen.Width := 1;
    NeuZeigenZahlenFeld(zahlenFeld, 1, 0, 0);
  end;
end;

procedure TSortFormAni.Paintbox1Resize(Sender: TObject);
begin
  CalcHoeheWeite;
  if demoRunning then
  begin
    breakPointNr := breakNr;
    abbruch := True;
  end;
end;

procedure TSortFormAni.RadioGroup1Click(Sender: TObject);
begin
  listArt := TListenTyp(radiogroup1.ItemIndex);
end;

procedure TSortFormAni.SpinEditFeldGroesseChange(Sender: TObject);
begin
  try
    maxFeld := SpinEditFeldGroesse.Value;
    if zahlenFeld[1] >= 0 then
      NeuZeigenZahlenFeld(zahlenFeld, 1, 0, 0);
  finally
  end;
end;

procedure TSortFormAni.SpinEditPauseChange(Sender: TObject);
begin
  try
    aktPause := SpinEditPause.Value;
  finally
  end;
end;

procedure TSortFormAni.StarteDemo(sortArt: TSortTyp);
begin
  ButtonsAktivieren(False);
  FontSizeAnpassen;
  breakPointNr := noBreakPoint;
  repeat
    showSteps := True;
    abbruch := False;
    zeigeSortDemo(Paintbox1, sortArt);
  until breakPointNr = noBreakPoint;
  ButtonsAktivieren(True);
  {$IfNDef Windows}
  breakPointNr := $7FFFFF;
  showSteps := True;
  abbruch := False;
  zeigeSortDemo(Paintbox1, sortArt);
  {$EndIf}
end;

procedure TSortFormAni.ZeigeZahlenFeld(aCanvas: TCanvas; feld: TFeld;
  zeile: integer; von, bis: integer);
var
  k, x, y: integer;
  s: string;
begin
  y := (zeile - 1) * feldHoehe;
  for k := 1 to maxFeld do
  begin
    x := (k - 1) * feldWeite;
    s := IntToStr(feld[k]);
    if (von <= k) and (k <= bis) then
      aCanvas.Font.Color := clRed
    else
      aCanvas.Font.Color := clBlack;
    aCanvas.Rectangle(x, y, x + feldWeite - 1, y + feldHoehe - 3);
    aCanvas.TextOut(x + 2, y + 2, s);
    Application.ProcessMessages;
  end;
end;

procedure TSortFormAni.ZeigeSortDemo(aktPaintbox: TPaintbox; sortArt: TSortTyp);
var
  aCanvas: TCanvas;
  noAnimation: boolean;
  zeileNr: integer;

  procedure CheckNoAnimation;
  begin
    noAnimation := (breakNr < BreakPointNr) or abbruch;
    if not noAnimation then BreakPointNr := -2;
    breakNr := breakNr + 1;
  end;

  procedure InitFeldPos(zahlenFeld: TFeld; maxFeld, aktZeile: integer);
  var
    k: integer;
  begin
    InitFeldPosXY(maxFeld, aktZeile);
    for k := 1 to maxFeld do
      with f[k] do
      begin
        wert := zahlenfeld[k];
        fontCol := clBlack;
        frameCol := clBlack;
      end;
    f[hilf1] := f[1];
    f[hilf2] := f[1];
    f[hilf2].wert := -1;
  end;

  procedure NeueZeile;
  var
    k: integer;
  begin
    zeileNr := zeileNr + 1;
    for k := 1 to maxFeld do
      f[k].y := f[k].y + feldHoehe;
  end;

  procedure Ausgabe(const f: TPosition; von, bis: integer);
  var
    k: integer;
    myRect: TRect;
  begin
    for k := von to bis do
      with f[k] do
      begin
        aCanvas.pen.Color := frameCol;
        myRect := Rect(x + 1, y + 1, x + feldWeite - 1, y + feldHoehe - 3);
        aCanvas.Rectangle(myRect);
        if wert >= 0 then
        begin
          aCanvas.Font.color := fontCol;
          aCanvas.TextRect(myRect, x, y, IntToStr(wert));
        end;
      end;
    aCanvas.pen.Color := clBlack;
  end;

  procedure LoeschePosVonBis(const f: TPosition; von, bis: integer);
  begin
    aCanvas.pen.Color := clWhite;
    aCanvas.Rectangle(f[von].x, f[von].y, f[bis].x + feldWeite - 1,
      f[von].y + feldHoehe - 3);
  end;

  procedure ZeigeSortiert(f: TPosition);
  var
    k: integer;
  begin
    for k := 1 to maxFeld do
    begin
      f[k].fontCol := DemoFarb[istSortiert];
      f[k].frameCol := DemoFarb[istSortiert];
    end;
    Ausgabe(f, 1, maxfeld);
  end;

  procedure MakeWeg(vonx, vony, nachx, nachy, step: integer; var mm: TWeg;
  var nr: integer);
  var
    stepx, stepy: integer;
  begin
    if vonx = nachx then
      stepx := 0
    else if step <> 0 then
      stepx := abs(step)
    else
      stepx := 1;
    if vony = nachy then
      stepy := 0
    else if step <> 0 then
      stepy := abs(step)
    else
      stepy := 1;
    if (stepx = 0) and (stepy = 0) then
      exit;
    nr := nr + 1;
    with mm[nr] do
    begin
      if vonx <= nachx then
      begin
        vx := vonx;
        nx := nachx + feldWeite;
        zx := 0;
        sx := stepX;
      end
      else
      begin
        nx := vonx + feldWeite;
        vx := nachx;
        zx := vonx - nachx;
        sx := -stepX;
      end;
      if vony <= nachy then
      begin
        vy := vony;
        ny := nachy + feldHoehe;
        zy := 0;
        sy := stepY;
      end
      else
      begin
        ny := vony + feldHoehe;
        vy := nachy;
        zy := vony - nachy;
        sy := -stepY;
      end;
    end;
  end;

  procedure Verschieben(var f: TPosition; von, nach, step: integer;
    mitHG, mitHilf: boolean);
  var
    zx, zy, dx, dy, nr, k: integer;
    bitMapHG, bitMapVG, BM_Hilf: TBitMap;
    m: TWeg;
  begin
    CheckNoAnimation;
    if noAnimation then exit;
    nr := 0;
    m := DeFault(TWeg);
    dx := f[von].x;
    dy := f[von].y;
    zx := f[nach].x;
    zy := f[nach].y;
    if mitHilf then
      MakeWeg(dx, dy, zx, dy, step, m, nr)
    else if dy = f[nach].y then
    begin
      if dx = zx then
        exit;
      if Abs(dx - zx) = feldWeite then
        MakeWeg(dx, dy, zx, dy, step, m, nr)
      else
      begin
        MakeWeg(dx, dy, dx, dy + feldHoehe, step, m, nr);
        MakeWeg(dx, dy + feldHoehe, zx, dy + feldHoehe, step, m, nr);
        MakeWeg(zx, dy + feldHoehe, zx, dy, step, m, nr);
      end;
    end
    else
    begin
      if (step < 0) and (dy > zy) then
      begin
        MakeWeg(dx, dy, dx, zy, step, m, nr);
        MakeWeg(dx, zy, zx, zy, step, m, nr);
      end
      else
      begin
        MakeWeg(dx, dy, zx, dy, step, m, nr);
        MakeWeg(zx, dy, zx, zy, step, m, nr);
      end;
    end;
    if nr = 0 then
      exit;
    bitMapVG := TBitmap.Create;
    bitMapVG.Width := feldWeite - 1;
    bitMapVG.Height := feldHoehe;
    bitBlt(bitMapVG.canvas.handle, 0, 0, feldWeite - 1, feldHoehe,
      aCanvas.handle, dx, dy, srcCopy);
    for k := 1 to nr do
    begin
      dx := m[k].nx - m[k].vx;
      dy := m[k].ny - m[k].vy;
      zx := m[k].zx;
      zy := m[k].zy;

      BM_Hilf := TBitmap.Create;
      BM_Hilf.Width := dx;
      BM_Hilf.Height := dy;

      bitMapHG := TBitmap.Create;
      bitMapHG.Width := dx;
      bitMapHG.Height := dy;
      bitBlt(bitMapHG.canvas.handle, 0, 0, dx, dy, aCanvas.handle,
        m[k].vx, m[k].vy, srcCopy);
      if (k = 1) and not mitHG then
        with m[1] do
          bitBlt(bitMapHG.canvas.handle, zx, zy, zx + feldWeite - 1,
            zy + feldHoehe - 3, bitMapVG.canvas.handle, 0, 0, whiteness);
      while (0 <= zx) and (zx <= dx - feldWeite) and (0 <= zy) and
        (zy <= dy - feldHoehe) do
      begin
        bitBlt(BM_Hilf.canvas.handle, 0, 0, dx, dy, bitMapHG.canvas.handle,
          0, 0, SrcCopy);
        bitBlt(BM_Hilf.canvas.handle, zx, zy, zx + feldWeite - 1, zy +
          feldHoehe - 3, bitMapVG.canvas.handle, 0, 0, SrcCopy);
        bitBlt(aCanvas.handle, m[k].vx, m[k].vy, m[k].nx, m[k].ny,
          BM_Hilf.canvas.handle, 0, 0, SrcCopy);
        aCanvas.refresh;
        WarteMS(aktPause);
        zx := zx + m[k].sx;
        zy := zy + m[k].sy;
      end;
      BM_Hilf.Free;
      if k < nr then
        bitBlt(aCanvas.handle, m[k].vx, m[k].vy, m[k].nx, m[k].ny,
          bitMapHG.canvas.handle, 0, 0, SrcCopy);

      bitMapHG.Free;
      if mitHilf then
        f[hilf2].x := f[nach].x;
    end;
    bitMapVG.Free;
  end;

  procedure VerschiebeFeld(var f: TPosition; von, nach, step: integer; mitHG: boolean);
  begin
    f[nach].wert := f[von].wert;
    if (von <> nach) then Verschieben(f, von, nach, step, mitHG, False);
  end;

  procedure VerschiebeFeldHilf2(var f: TPosition; nach, step: integer; mitHG: boolean);
  begin
    if (hilf2 <> nach) and (f[hilf2].x <> f[nach].x) then
      Verschieben(f, hilf2, nach, step, mitHG, True);
  end;

  procedure VergleicheUndZeige(const f1: TPosition; n1: integer;
  const f2: TPosition; n2: integer; farbe: TColor; wdh, ms: integer);
  var
    mx1, mx2, my1, my2, y, k, p1, p2: integer;
    b: boolean;
    old: TColor;
  begin
    if abbruch or (n1 = n2) or noAnimation then
      exit;
    k := 1;
    b := (@f1 = @f2);
    mx1 := f1[n1].x + (feldWeite div 2);
    mx2 := f2[n2].x + (feldWeite div 2);
    my1 := f1[n1].y + feldHoehe + 2;
    p1 := 3;
    p2 := 3;
    if f1[n1].y = f2[n2].y then
    begin
      if mx1 = mx2 then
        exit;
      my2 := my1;
      y := my1 - 2 + feldhoehe div 2;
    end
    else
    begin
      if (mx1 = mx2) and (Abs(f1[n1].y - f2[n2].y) = feldHoehe) then
        exit;
      if f1[n1].y < f2[n2].y then
      begin
        my2 := f2[n2].y - 2;
        my1 := my1 - (feldhoehe div 2) - 4;
        p1 := 0;
        p2 := -3;
        if mx1 <> mx2 then
          if mx1 < mx2 then
            mx2 := mx2 + 2
          else
            mx2 := mx2 - 2
        else
        begin
          my1 := f1[n1].y + feldHoehe + 4;
          if b then
            my2 := f2[n2].y - 2
          else
            my2 := f2[n2].y - (feldhoehe div 2) - 4;
        end;
      end
      else
      begin
        my2 := f2[n2].y + feldHoehe + 2;
        my1 := my1 - (feldhoehe div 2);
        p1 := 0;
        if (mx1 = mx2) or not b then
        begin
          my1 := f1[n1].y - 2;
          my2 := f2[n2].y + feldHoehe + 2;
        end;
      end;
      y := my1;
      if f1[n1].x < f2[n2].x then
        mx1 := mx1 + (feldWeite div 2) + 2
      else if f1[n1].x > f2[n2].x then
        mx1 := mx1 - (feldWeite div 2) - 2;
    end;
    old := aCanvas.Pen.color;
    aCanvas.pen.Width := 2;
    if ms = 0 then
      wdh := wdh - 2;
    repeat
      if odd(k) then
        aCanvas.Pen.color := farbe
      else
        aCanvas.Pen.color := clWhite;
      if p1 <> 0 then
        aCanvas.polyline([point(mx1, my1), point(mx1 - p1, my1 + p1),
          point(mx1 + p1, my1 + p1), point(mx1, my1), point(mx1, y)]);
      aCanvas.polyline([point(mx1, y), point(mx2, y), point(mx2, my2),
        point(mx2 - p2, my2 + p2), point(mx2 + p2, my2 + p2), point(mx2, my2)]);
      WarteMS(ms);
      k := k + 1;
    until (k > wdh) or abbruch;
    aCanvas.Pen.color := old;
    aCanvas.pen.Width := 1;
  end;

  procedure ZeigeVergleich(const f: TPosition; n1, n2: integer;
    farbe: TColor; wdh, ms: integer);
  begin
    VergleicheUndZeige(f, n1, f, n2, farbe, wdh, ms);
  end;

  procedure VerschiebeNachHilf(var f: TPosition; k, dy: integer; mitHG: boolean);
  begin
    f[hilf1].x := f[k].x;
    f[hilf1].y := f[k].y + dy * feldHoehe;
    f[hilf1].wert := f[k].wert;
    f[hilf1].FrameCol := clBlack;
    f[hilf1].fontCol := clBlack;
    VerschiebeFeld(f, k, hilf1, 2, mitHG);
  end;

  procedure VerschiebeNachHilf2(var f: TPosition; k, dy: integer; mitHG: boolean);
  begin
    f[hilf2].x := f[k].x;
    f[hilf2].y := f[k].y + dy * feldHoehe;
    f[hilf2].wert := f[k].wert;
    f[hilf2].FrameCol := clBlack;
    f[hilf2].fontCol := clBlack;
    VerschiebeFeld(f, k, hilf2, 2, mitHG);
  end;

  procedure ZeigePfeil(const f: TPosition; p: integer; farbe: TColor; wdh, ms: integer);
  var
    mitte, y, y2, k: integer;
    old: TColor;
  begin
    if abbruch or noAnimation then
      exit;
    k := 1;
    mitte := f[p].x + (feldWeite div 2);
    y := f[p].y + feldHoehe + 2;
    y2 := y + feldHoehe - 7;
    old := aCanvas.Pen.color;
    aCanvas.pen.Width := 2;
    if ms = 0 then
      wdh := wdh - 2;
    repeat
      if odd(k) then
        aCanvas.Pen.color := farbe
      else
        aCanvas.Pen.color := clWhite;
      aCanvas.Polyline([point(mitte, y2), point(mitte, y), point(
        mitte - 3, y + 3), point(mitte + 3, y + 3), point(mitte, y)]);
      WarteMS(aktPause);
      k := k + 1;
    until (k > wdh) or abbruch;
    aCanvas.Pen.color := old;
    aCanvas.pen.Width := 1;
  end;


  procedure SortiereMitMinSort(feld: TFeld; zeile: integer);
  var
    k, p: integer;
    min: TElementTyp;
    minStelle: integer;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    for k := 1 to maxfeld - 1 do
    begin
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      // Minimum im Restfeld suchen
      min := feld[k];
      minStelle := k;
      VerschiebeNachHilf(f, minStelle, 2, True);
      ZeigePfeil(f, minStelle, DemoFarb[MinMarker], 1, 0);
      for p := k + 1 to maxfeld do
      begin
        ZeigeVergleich(f, 0, p, DemoFarb[vergleichen], 4, 4 * aktPause);
        if feld[p] < min then
        begin
          VerschiebeFeld(f, 0, minStelle, 2, False);
          if minStelle <> k then
            ZeigePfeil(f, minStelle, clwhite, 1, 0);
          ZeigePfeil(f, p, DemoFarb[PosMarker], 1, 0);
          min := feld[p];
          minStelle := p;
          VerschiebeNachHilf(f, minStelle, 2, True);
        end;
      end;
      //vertauschen
      if k <> minStelle then
      begin
        ZeigePfeil(f, minStelle, clWhite, 1, 0);
        ZeigePfeil(f, k, clWhite, 1, 0);
        ZeigeVergleich(f, k, minStelle, DemoFarb[verschiebung], 1, 0);
        VerschiebeFeld(f, k, minStelle, 2, True);
        VerschiebeFeld(f, 0, k, 2, False);
        ZeigeVergleich(f, k, minStelle, clWhite, 1, 0);
        feld[minStelle] := feld[k];
      end
      else
      begin
        ZeigePfeil(f, minStelle, DemoFarb[PosMarker], 1, 0);
        VerschiebeFeld(f, 0, minStelle, 2, False);
        ZeigePfeil(f, minStelle, clWhite, 1, 0);
      end;
      feld[k] := min;
      //Zwischenergebnis zeigen
      f[k].fontCol := DemoFarb[istSortiert];
      f[k].frameCol := DemoFarb[istSortiert];
      Ausgabe(f, k, k);
      NeueZeile;
    end;
    ZeigeSortiert(f);
  end;

  procedure SortiereMitEinSort(feld: TFeld; zeile: integer);
  var
    k, p: integer;
    akt: TElementTyp;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    f[1].fontCol := DemoFarb[istSortiert];
    for k := 3 to maxFeld do
    begin
      f[k].fontCol := DemoFarb[inaktiv];
      f[k].frameCol := DemoFarb[inaktiv];
    end;

    for k := 2 to maxfeld do
    begin
      with f[k] do
      begin
        fontCol := clBlack;
        frameCol := clBlack;
      end;
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      akt := feld[k];
      ZeigePfeil(f, k, DemoFarb[PosMarker], 1, 0);
      VerschiebeNachHilf(f, k, 2, True);
      ZeigeVergleich(f, 0, k - 1, DemoFarb[vergleichen], 6, aktpause * 2);
      // aktuelles Element einsortieren
      p := k;
      while (p > 1) and (akt < feld[p - 1]) do
      begin
        VerschiebeFeld(f, p - 1, p, 1, True);
        feld[p] := feld[p - 1];
        p := p - 1;
        if p > 1 then
          ZeigeVergleich(f, 0, p - 1, DemoFarb[vergleichen], 6, aktpause * 2);
      end;
      //vertauschen
      feld[p] := akt;
      if p <> k then
      begin
        ZeigePfeil(f, k, clWhite, 1, 0);
        ZeigeVergleich(f, 0, p, DemoFarb[verschiebung], 1, 0);
        VerschiebeFeld(f, 0, p, 2, False);
        ZeigeVergleich(f, 0, p, clWhite, 1, 0);
      end
      else
      begin
        VerschiebeFeld(f, 0, p, 2, False);
        ZeigePfeil(f, p, clWhite, 1, 0);
      end;
      //Zwischenergebnis zeigen
      f[p].fontCol := DemoFarb[istSortiert];
      f[k].fontCol := DemoFarb[istSortiert];
      NeueZeile;
      Ausgabe(f, 1, maxFeld);
    end;
    ZeigeSortiert(f);
  end;

  procedure SortiereMitBinSort(feld: TFeld; zeile: integer);
  var
    i, m, j, k, l: integer;
    hilf: TElementTyp;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    f[1].fontCol := DemoFarb[istSortiert];
    for k := 2 to maxFeld do
      with f[k] do
      begin
        fontCol := DemoFarb[inaktiv];
        frameCol := DemoFarb[inaktiv];
      end;
    for i := 2 to maxfeld do
    begin
      with f[i] do
      begin
        fontCol := clBlack;
        frameCol := clBlack;
      end;
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      hilf := feld[i];
      l := 1;
      k := i - 1;
      VerschiebeNachHilf(f, i, 2, True);
      while l <= k do
      begin
        m := (l + k) div 2;
        ZeigeVergleich(f, 0, m, DemoFarb[vergleichen], 6, aktPause * 2);
        if hilf < feld[m] then
          k := m - 1
        else
          l := m + 1;
      end;
      ZeigeVergleich(f, 0, l, DemoFarb[verschiebung], 1, 0);
      for j := i - 1 downto l do
      begin
        feld[j + 1] := feld[j];
        VerschiebeFeld(f, j, j + 1, 2, True);
      end;
      feld[l] := hilf;
      VerschiebeFeld(f, 0, l, 2, False);
      ZeigeVergleich(f, 0, l, clWhite, 1, 0);
      f[i].fontCol := DemoFarb[istSortiert];
      NeueZeile;
    end;
    ZeigeSortiert(f);
  end;   { BinaeresEinsortieren }

  procedure SortiereMitBubble1Sort(feld: TFeld; zeile: integer);
  var
    k, p: integer;
    element: TElementTyp;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    for k := 2 to maxfeld do
    begin
      CheckNoAnimation;
      Ausgabe(f, 1, MaxFeld);
      for p := maxfeld downto k do
      begin
        ZeigeVergleich(f, p - 1, p, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[p - 1] > feld[p] then
        begin
          element := feld[p];
          ZeigeVergleich(f, p - 1, p, DemoFarb[verschiebung], 1, 0);
          VerschiebeNachHilf(f, p, 1, True);
          VerschiebeFeld(f, p - 1, p, 2, True);
          VerschiebeFeld(f, 0, p - 1, 2, False);
          ZeigeVergleich(f, p - 1, p, clWhite, 1, 0);
          feld[p] := feld[p - 1];
          feld[p - 1] := element;
        end;
      end;
      f[k - 1].fontCol := DemoFarb[istSortiert];
      f[k - 1].frameCol := DemoFarb[istSortiert];
      Ausgabe(f, k - 1, k - 1);
      NeueZeile;
    end;
    ZeigeSortiert(f);
  end;

  procedure SortiereMitBubble2Sort(feld: TFeld; zeile: integer);
  var
    k, ende: integer;
    hilf: TElementTyp;
    fertig: boolean;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    ende := maxFeld;
    repeat
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      ende := ende - 1;
      fertig := True;
      for k := 1 to ende do
      begin
        ZeigeVergleich(f, k, k + 1, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[k] > feld[k + 1] then
        begin
          ZeigeVergleich(f, k, k + 1, DemoFarb[verschiebung], 1, 0);
          VerschiebeNachHilf(f, k, 1, True);
          VerschiebeFeld(f, k + 1, k, 2, True);
          VerschiebeFeld(f, 0, k + 1, 2, False);
          ZeigeVergleich(f, k, k + 1, clWhite, 1, 0);
          hilf := feld[k];
          feld[k] := feld[k + 1];
          feld[k + 1] := hilf;
          fertig := False;
        end;
      end;
      f[ende + 1].fontCol := DemoFarb[istSortiert];
      f[ende + 1].frameCol := DemoFarb[istSortiert];
      Ausgabe(f, ende + 1, ende + 1);
      if not fertig then
        NeueZeile;
    until fertig;
    ZeigeSortiert(f);
  end;  { Bubble2Sort }

  procedure SortiereMitCombSort(feld: TFeld; zeile: integer);
  const
    schrumpfFaktor = 1.3;
  var
    k, j, weite: integer;
    hilf: TElementTyp;
    fertig: boolean;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    weite := maxFeld;
    repeat
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      weite := Trunc(weite / schrumpfFaktor);
      case weite of
        0: weite := 1;
        9, 10: weite := 11;
      end;
      fertig := True;
      j := weite;
      for k := 1 to maxFeld - weite do
      begin
        Inc(j);
        ZeigeVergleich(f, k, j, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[k] > feld[j] then
        begin
          ZeigeVergleich(f, k, j, DemoFarb[verschiebung], 1, 0);
          VerschiebeNachHilf(f, k, 2, True);
          VerschiebeFeld(f, j, k, 2, True);
          VerschiebeFeld(f, 0, j, 2, False);
          ZeigeVergleich(f, k, j, clWhite, 1, 0);
          hilf := feld[k];
          feld[k] := feld[j];
          feld[j] := hilf;
          fertig := False;
        end;
      end;
      NeueZeile;
    until fertig and (weite = 1);
    ZeigeSortiert(f);
  end;  { CombSort }

  procedure SortiereMitFlashSort(feld: TFeld; zeile: integer);
  const
    maxKlassenNr = 5;
  var
    c1, c2: double;
    stellenNr, grenze: array[1..maxKlassenNr] of integer;

    procedure KlassenVorbereiten;
    var
      i, k, temp, x1, x2: integer;
      nmin, nmax: integer;
    begin
      nmin := 1;
      nmax := 1;
      for i := 2 to maxFeld do
      begin
        if feld[i] < feld[nmin] then
          nmin := i;
        if feld[i] > feld[nmax] then
          nmax := i;
      end;
      if feld[nmax] <> feld[nmin] then
        c1 := (maxKlassenNr - 1) / (feld[nmax] - feld[nmin])
      else
        c1 := 0;
      c2 := c1 * feld[nmin];

      //for k := 1 to maxKlassenNr do stellenNr[k] := 0;
      FillChar(stellenNr, SizeOf(stellenNr), 0);
      for i := 1 to maxFeld do
      begin
        k := 1 + trunc(c1 * feld[i] - c2);
        stellenNr[k] := stellenNr[k] + 1;
      end;
      for k := 2 to maxKlassenNr do
        stellenNr[k] := stellenNr[k] + stellenNr[k - 1];

      grenze[1] := 1;
      temp := 1;
      for k := 1 to maxKlassenNr - 1 do
      begin
        x1 := stellenNr[k];
        x2 := stellenNr[k + 1];
        if x1 < x2 then
        begin
          for i := x1 + 1 to maxFeld do
            f[i].x := f[i].x + 8;
          temp := temp + 1;
          grenze[temp] := x1 + 1;
        end;
      end;
      CheckNoAnimation;
      for k := temp + 1 to maxKlassenNr do
        grenze[k] := maxFeld;
      Ausgabe(f, 1, maxFeld);
      VerschiebeNachHilf(f, nmax, 2, True);
      VerschiebeFeld(f, 1, nmax, 2, True);
      VerschiebeFeld(f, 0, 1, 2, False);
      temp := feld[nmax];
      feld[nmax] := feld[1];
      feld[1] := temp;
    end;

    procedure Perm;
    var
      nmove, n, j, k, aktStelle, FLASH, temp: integer;
    begin
      for k := 1 to maxFeld do
        f[k].fontCol := DemoFarb[inaktiv];
      Ausgabe(f, 1, maxFeld);
      for k := 1 to maxKlassenNr do
        ZeigePfeil(f, stellenNr[k], DemoFarb[PosMarker], 2, 0);

      nmove := 0;
      j := 1;
      k := maxKlassenNr;
      while nmove < (maxFeld - 1) do
      begin
        CheckNoAnimation;
        while j > stellenNr[k] do
        begin
          j := j + 1;
          k := 1 + Trunc((c1 * feld[j] - c2));
          VerschiebeFeldHilf2(f, j, 2, False);
        end;
        FLASH := feld[j];
        VerschiebeNachHilf2(f, j, 3, True);

        while j <> (stellenNr[k] + 1) do
        begin
          k := 1 + Trunc(c1 * FLASH - c2);
          aktStelle := stellenNr[k];
          ZeigeVergleich(f, -1, aktstelle, clGreen, 1, 0);

          temp := feld[aktStelle];
          VerschiebeNachHilf(f, aktStelle, 2, True);

          ZeigeVergleich(f, -1, aktstelle, clWhite, 1, 0);
          ZeigePfeil(f, aktStelle, DemoFarb[PosMarker], 2, 0);

          VerschiebeFeldHilf2(f, aktStelle, 2, False);

          feld[aktStelle] := FLASH;
          VerschiebeFeld(f, -1, aktStelle, 1, True);

          ZeigePfeil(f, aktStelle, clWhite, 2, 0);
          if aktStelle > grenze[k] then
            ZeigePfeil(f, aktStelle - 1, DemoFarb[PosMarker], 2, 0);
          if aktStelle = maxFeld then
            f[maxFeld].fontCol := DemoFarb[istSortiert]
          else
            f[aktStelle].fontCol := clBlack;
          Ausgabe(f, aktStelle, aktStelle);

          FLASH := temp;
          VerschiebeFeld(f, 0, -1, 1, False);

          stellenNr[k] := stellenNr[k] - 1;
          nmove := nmove + 1;
        end;
      end;
      LoeschePosVonBis(f, hilf2, hilf2);
      for n := 1 to maxFeld - 1 do
        f[n].fontCol := clBlack;
      Ausgabe(f, 1, maxFeld);
    end;

    procedure Insert;
    var
      i, j, k, temp: integer;
    begin
      f[maxFeld].fontCol := DemoFarb[istSortiert];
      k := maxKlassenNr;
      while (grenze[k] = maxFeld) and (k > 0) do
        k := k - 1;
      for i := maxFeld - 2 downto 1 do
      begin
        CheckNoAnimation;
        ZeigeVergleich(f, i + 1, i, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[i + 1] < feld[i] then
        begin
          VerschiebeNachHilf(f, i, 2, True);
          temp := feld[i];
          j := i;
          while feld[j + 1] < temp do
          begin
            VerschiebeFeld(f, j + 1, j, 2, False);
            feld[j] := feld[j + 1];
            j := j + 1;
            if j < maxFeld then
              ZeigeVergleich(f, 0, j + 1, DemoFarb[vergleichen], 6, aktpause * 2);
          end;
          VerschiebeFeld(f, 0, j, 2, False);
          feld[j] := temp;
        end;
        if i = grenze[k] then
        begin
          if k = maxKlassenNr then
            for j := i to maxFeld - 1 do
              f[j].fontCol := DemoFarb[istSortiert]
          else
            for j := i to grenze[k + 1] do
              f[j].fontCol := DemoFarb[istSortiert];
          k := k - 1;
        end;
        if i > 1 then
          NeueZeile;
        Ausgabe(f, 1, maxFeld);
      end;
    end;

  begin
    zeile := 2;
    InitFeldPos(Feld, maxFeld, zeile);
    Ausgabe(f, 1, maxFeld);
    NeueZeile;
    KlassenVorbereiten;
    Perm;
    NeueZeile;
    Ausgabe(f, 1, maxFeld);
    Insert;
    ZeigeSortiert(f);
  end;

  procedure SortiereMitMergeSort(feld: TFeld; zeile: integer);
  var
    maxZeile, k: integer;

    procedure Merge(var A: TFeld; p, q, r: integer);
    var
      i, j, k, n1: integer;
      B: TFeld;
      buf: TPosition;
    begin { Merge }
      buf := Default(TPosition);
      B := Default(TFeld);
      n1 := q - p + 1;
      for k := 1 to n1 do
      begin
        j := p + k - 1;
        VerschiebeNachHilf2(f, j, 3, True);
        buf[k] := f[-1];
        f[j].fontCol := DemoFarb[inaktiv];
        B[k] := A[j];
        Ausgabe(f, j, j);
      end;
      Ausgabe(buf, 1, n1);
      i := 1;
      j := q + 1;
      k := p;
      while ((i <= n1) and (j <= r)) do
      begin
        VergleicheUndZeige(buf, i, f, j, DemoFarb[vergleichen], 6, aktpause * 2);
        if (B[i] < A[j]) then
        begin
          f[-1] := buf[i];
          VerschiebeFeld(f, -1, k, 2, True);
          A[k] := B[i];
          f[k].fontCol := clBlack;
          buf[i].fontCol := DemoFarb[inaktiv];
          Ausgabe(buf, i, i);
          i := i + 1;
        end
        else
        begin
          VerschiebeFeld(f, j, k, 2, True);
          f[j].fontCol := DemoFarb[inaktiv];
          f[k].fontCol := clBlack;
          A[k] := A[j];
          Ausgabe(f, k, j);
          j := j + 1;
        end;
        k := k + 1;
      end;

      while (i <= n1) do
      begin
        f[-1] := Buf[i];
        VerschiebeFeld(f, -1, k, 2, True);
        A[k] := B[i];
        f[k].fontCol := clBlack;
        buf[i].fontCol := DemoFarb[inaktiv];
        Ausgabe(buf, i, i);
        k := k + 1;
        i := i + 1;
      end;
      for k := 1 to n1 do
      begin
        buf[k].fontCol := clWhite;
        buf[k].frameCol := clWhite;
      end;
      Ausgabe(buf, 1, n1);
    end;

    procedure MergeSort(var A: TFeld; p, r, z: integer);
    var
      q, k: integer;
    begin { MergeSort }
      if (p < r) then
      begin
        CheckNoAnimation;
        for k := p to r do
          f[k].y := (z - 1) * feldHoehe;
        zeile := z;
        q := (p + r) div 2;
        for k := q + 1 to maxFeld do
          f[k].x := f[k].x + 6;

        MergeSort(A, p, q, z - 1);
        MergeSort(A, q + 1, r, z - 1);

        for k := p to r do
          f[k].y := z * feldHoehe;
        Ausgabe(f, p, r);
        Merge(A, p, q, r);
        LoeschePosVonBis(f, q, r);
        for k := q + 1 to r do
          f[k].x := f[k - 1].x + feldWeite;
        Ausgabe(f, p, r);
      end
      else
      begin
        f[p].y := z * feldHoehe;
        Ausgabe(f, p, p);
        if p = maxFeld then
        begin
          f[p].y := (z - 1) * feldHoehe;
          Ausgabe(f, p, p);
        end;
      end;
    end;

  begin
    zeile := 1;
    maxZeile := 2;
    k := maxFeld;
    while k - (k div 2) > 1 do
    begin
      k := k - (k div 2);
      Inc(maxZeile);
    end;
    InitFeldPos(Feld, maxFeld, zeile);
    Ausgabe(f, 1, maxFeld);
    NeueZeile;
    MergeSort(feld, 1, maxFeld, maxZeile);
    zeile := MaxZeile + 1;
    ZeigeSortiert(f);
  end;

  procedure SortiereMitShakeSort(feld: TFeld; zeile: integer);
  var
    j, k, l, r, n, p: integer;
    hilf: TElementTyp;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    p := 0;
    l := 2;
    r := maxfeld;
    k := maxfeld;
    repeat
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      p := p + 1;
      for j := r downto l do
      begin
        ZeigeVergleich(f, j - 1, j, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[j - 1] > feld[j] then
        begin
          ZeigeVergleich(f, j - 1, j, DemoFarb[verschiebung], 1, 0);
          VerschiebeNachHilf(f, j, 1, True);
          VerschiebeFeld(f, j - 1, j, 2, True);
          VerschiebeFeld(f, 0, j - 1, 2, False);
          ZeigeVergleich(f, j - 1, j, clWhite, 1, 0);
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
        end;
      end;

      for n := p to k - 1 do
      begin
        f[n].fontCol := DemoFarb[istSortiert];
        f[n].FrameCol := DemoFarb[istSortiert];
      end;
      Ausgabe(f, p, k - 1);
      l := k + 1;
      for j := l to r do
      begin
        ZeigeVergleich(f, j - 1, j, DemoFarb[vergleichen], 6, aktPause * 2);
        if feld[j - 1] > feld[j] then
        begin
          ZeigeVergleich(f, j - 1, j, DemoFarb[verschiebung], 1, 0);
          VerschiebeNachHilf(f, j, 1, True);
          VerschiebeFeld(f, j - 1, j, 2, True);
          VerschiebeFeld(f, 0, j - 1, 2, False);
          ZeigeVergleich(f, j - 1, j, clWhite, 1, 0);
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
        end;
      end;
      for n := k to maxFeld + 1 - p do
      begin
        f[n].fontCol := DemoFarb[istSortiert];
        f[n].frameCol := DemoFarb[istSortiert];
      end;
      Ausgabe(f, k, maxFeld + 1 - p);
      for n := l to k - 1 do
        f[n].fontCol := clBlack;
      NeueZeile;
      r := k - 1
    until l > r;
    ZeigeSortiert(f);
  end;   { ShakSort }

  procedure SortiereMitShellSort(feld: TFeld; zeile: integer);
  var
    i, j, k: integer;
    hilf: TElementTyp;
    m: 1..15;
  begin
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    m := 1;
    while (shellSortSteps[m] >= maxfeld) do
      Inc(m);
    for m := m to 15 do
    begin
      CheckNoAnimation;
      Ausgabe(f, 1, maxFeld);
      k := shellSortSteps[m];
      for i := k + 1 to maxfeld do
      begin
        hilf := feld[i];
        j := i - k;
        VerschiebeNachHilf(f, i, 2, True);
        ZeigeVergleich(f, i, j, DemoFarb[vergleichen], 6, aktPause * 2);
        while (j > 0) and (hilf < feld[j]) do
        begin
          ZeigeVergleich(f, j, j + k, DemoFarb[verschiebung], 1, 0);
          VerschiebeFeld(f, j, j + k, 2, True);
          ZeigeVergleich(f, j, j + k, clWhite, 1, 0);
          feld[j + k] := feld[j];
          j := j - k;
          if j > 0 then
            ZeigeVergleich(f, i, j, DemoFarb[vergleichen], 6, aktPause * 2);
        end;
        ZeigeVergleich(f, 0, j + k, DemoFarb[verschiebung], 1, 0);
        VerschiebeFeld(f, 0, j + k, 2, False);
        ZeigeVergleich(f, 0, j + k, clWhite, 1, 0);
        feld[j + k] := hilf;
      end;
      NeueZeile;
    end;
    ZeigeSortiert(f);
  end;   { ShellSort }

  procedure SortiereMitHeapSort(feld: TFeld; zeile: integer);
  var
    l, r, k: integer;
    hilf: TElementTyp;
    b: TPosition;
    heapAufbauen: boolean;

    procedure ZeigeAst(n: integer; farbe: TColor);
    var
      x1, y1, x2, y2, k: integer;
    begin
      if (n <= 0) or (maxFeld < n) then
        exit;
      x1 := b[n].x + feldWeite div 2;
      y1 := b[n].y;
      k := n div 2;
      if k = 0 then
      begin
        x2 := x1;
        y2 := y1 - feldHoehe;
      end
      else
      begin
        if 2 * k < n then
          x2 := b[k].x + feldWeite
        else
          x2 := b[k].x;
        y2 := b[k].y + feldHoehe div 2;
      end;
      aCanvas.pen.color := farbe;
      aCanvas.PolyLine([point(x1, y1), point(x1, y2), point(x2, y2)]);
      aCanvas.pen.color := clBlack;
    end;

    procedure InitBaum(zeile: integer);
    var
      r, s, a, pot2, k, x: integer;
    begin
      aCanvas.Pen.Color := PaintBox1.Color;
      aCanvas.Rectangle(0, (zeile + 2) * feldHoehe, paintBox1.ClientWidth,
        paintBox1.ClientHeight);
      aCanvas.Pen.Color := clBlack;
      zeile := zeile + 3;
      r := 1;
      a := 1;
      while a < maxFeld do
      begin
        r := r + a;
        zeile := zeile + 2;
        a := a * 2;
      end;
      if maxFeld > 15 then
        r := r - 1;
      a := a div 2;
      pot2 := 1;
      s := 2;
      x := 0;
      for k := 2 * a - 1 downto 1 do
      begin
        if k < a then
        begin
          zeile := zeile - 2;
          r := r - pot2;
          pot2 := pot2 * 2;
          s := s + pot2;
          a := a div 2;
          x := 0;
        end;
        if k <= maxfeld then
        begin
          b[k].x := (r - x - 1) * FeldWeite;
          b[k].y := (zeile - 1) * feldHoehe;
          b[k].wert := -1;
          b[k].frameCol := DemoFarb[inaktiv];
          b[k].fontCol := clBlack;
        end;
        x := x + s;
      end;
      if maxFeld > 15 then
      begin
        s := (b[9].x - b[8].x) div 2;
        for k := 12 to 15 do b[k].x := b[k - 1].x + s;
        for k := 6 to 7 do b[k].x := (b[2 * k].x + b[2 * k + 1].x) div 2;
        b[3].x := (b[6].x + b[7].x) div 2;
        b[1].x := (b[11].x + b[12].x) div 2;
      end;
      for k := maxFeld downto 1 do
      begin
        ZeigeAst(k, b[k].FrameCol);
        Ausgabe(b, k, k);
      end;
    end;

    procedure FuelleBaum(von, bis: integer);
    var
      k, p: integer;
    begin
      p := aktPause;
      aktPause := 0;
      for k := bis downto von do
      begin
        verschiebeNachHilf(f, k, 1, True);
        b[0] := f[0];
        VerschiebeFeld(b, 0, k, 8, False);
        b[k].FrameCol := clBlack;
        ZeigeAst(k, clBlack);
        Ausgabe(b, k, k);
      end;
      aktPause := p;
    end;

    procedure ZeigeBaumVergleich(i, j: integer; farbe: TColor; wdh, ms: integer);
    begin
      ZeigeVergleich(f, i, j, farbe, wdh, ms);
      ZeigeVergleich(b, i, j, farbe, wdh, ms);
    end;

    procedure Sift;
    var
      i, j, alti, k: integer;
      weiter: boolean;
    begin
      i := l;
      j := 2 * i;
      hilf := feld[i];
      weiter := (j <= r);
      if heapAufbauen then
      begin
        FuelleBaum(i, i);
        b[hilf1] := b[i];
        b[hilf1].y := b[i].y - feldHoehe;
        b[i].wert := -1;
        Ausgabe(b, i, i);
        if not noAnimation then Ausgabe(b, hilf1, hilf1);
        ZeigeAst(2 * i, clBlack);
        ZeigeAst(2 * i + 1, clBlack);
      end
      else if weiter then
        VerschiebeNachHilf(b, i, -1, True);
      if weiter then
      begin
        alti := i;
        VerschiebeNachHilf(f, i, 2, True);
      end
      else
        alti := 0;
      while weiter do
      begin
        if j < r then
        begin
          if aktPause > 0 then
            for k := 1 to 2 do
              ZeigeBaumVergleich(j, j + 1, DemoFarb[vergleichen], 2, aktpause * 2);
          if feld[j] < feld[j + 1] then
            Inc(j);
        end;
        if aktPause > 0 then
          for k := 1 to 3 do
            ZeigeBaumVergleich(0, j, DemoFarb[vergleichen], 2, aktpause * 2);
        if hilf < feld[j] then
        begin
          ZeigeBaumVergleich(i, j, DemoFarb[verschiebung], 1, 0);
          VerschiebeFeld(b, j, i, -2, True);
          ZeigeVergleich(b, i, j, clWhite, 1, 0);
          ZeigeAst(j, clblack);

          VerschiebeFeld(f, j, i, 4, True);
          ZeigeVergleich(f, i, j, clWhite, 1, 0);
          feld[i] := feld[j];
          i := j;
          j := 2 * i;
          weiter := (j <= r);
        end
        else
          weiter := False;
      end;
      ZeigeBaumVergleich(hilf1, i, DemoFarb[verschiebung], 1, 0);
      VerschiebeFeld(b, hilf1, i, -4, False);
      ZeigeVergleich(b, hilf1, i, clWhite, 1, 0);
      zeigeAst(alti, clBlack);
      zeigeAst(i, clBlack);
      VerschiebeFeld(f, hilf1, i, 8, False);
      ZeigeVergleich(f, hilf1, i, clWhite, 1, 0);
      feld[i] := hilf;
    end;

  begin  {Heapsort}
    B := Default(TPosition);
    InitFeldPos(Feld, maxFeld, Zeile + 1);
    CheckNoAnimation;
    l := (maxfeld div 2) + 1;
    r := maxfeld;
    for k := 1 to l - 1 do
    begin
      f[k].fontCol := DemoFarb[inaktiv];
      f[k].frameCol := DemoFarb[inaktiv];
    end;
    Ausgabe(f, 1, r);
    InitBaum(zeile);
    FuelleBaum(l, r);
    heapAufbauen := True;
    while l > 1 do
    begin
      Dec(l);
      f[l].fontCol := clBlack;
      f[l].frameCol := clBlack;
      Ausgabe(f, l, l);
      Sift;
    end;
    heapAufbauen := False;
    while r > 1 do
    begin
      hilf := feld[l];
      feld[l] := feld[r];
      feld[r] := hilf;
      ZeigeBaumVergleich(l, r, DemoFarb[verschiebung], 1, 0);
      b[hilf1].x := b[r].x;
      b[hilf1].y := b[l].y - feldHoehe;
      VerschiebeFeld(b, l, hilf1, -4, True);
      VerschiebeFeld(b, r, l, -4, True);
      VerschiebeFeld(b, hilf1, r, 2, False);
      b[r].fontCol := DemoFarb[istSortiert];
      b[r].FrameCol := DemoFarb[istSortiert];
      ZeigeVergleich(b, l, r, clWhite, 1, 0);
      ZeigeAst(r, DemoFarb[istSortiert]);
      Ausgabe(b, r, r);

      VerschiebeNachHilf(f, l, 2, True);
      VerschiebeFeld(f, r, l, 4, True);
      VerschiebeFeld(f, hilf1, r, 2, False);
      ZeigeVergleich(f, l, r, clWhite, 1, 0);
      f[r].fontCol := DemoFarb[istSortiert];
      f[r].FrameCol := DemoFarb[istSortiert];
      Ausgabe(f, r, maxFeld);
      Dec(r);
      if r > 1 then
        Sift;
    end;
    ZeigeSortiert(f);
    ZeigeSortiert(b);
  end;  {Heapsort}

  procedure SortiereMitQuickSort(feld: TFeld; zeile: integer);

    procedure Move(k: integer);
    var
      bVG: TBitMap;
      j: integer;
    begin
      j := maxFeld;
      while (j >= k) do
      begin
        if (f[j].x - f[j - 1].x > feldweite) and (f[j].y <> f[j - 1].y) then
        begin
          bVG := TBitMap.Create;
          bVG.Width := f[maxfeld].x + feldWeite + 6 - f[j].x;
          bVG.Height := f[j - 1].y - f[j].y;
          bitBlt(bVG.canvas.handle, 0, 0, bVG.Width, bVG.Height,
            canvas.handle, f[j].x, f[j].y, srcCopy);
          canvas.Pen.color := clWhite;
          canvas.Rectangle(f[j].x, f[j].y, f[j].x + 6, f[j - 1].y + feldhoehe);
          canvas.Pen.color := clBlack;
          bitBlt(canvas.handle, f[j].x + 6, f[j].y, bVG.Width, bVG.Height,
            bVG.canvas.handle, 0, 0, srcCopy);
          bVG.Free;
        end;
        j := j - 1;
      end;
    end;

    procedure Quick1(l, r, z: integer);
    var
      i, j, k: integer;
      hilf, help: TElementTyp;
      hRec: TFeldPos;
    begin
      if z > zeile then
        zeile := z;
      zeileNr := zeile;
      CheckNoAnimation;
      for k := l to r do
      begin
        f[k].fontCol := clBlack;
        f[k].y := z;
      end;
      Ausgabe(f, l, r);
      i := l;
      j := r;
      k := (l + r) div 2;
      hilf := feld[k];
      //neu
      f[k].fontCol := DemoFarb[marker_QSort];
      Ausgabe(f, k, k);

      VerschiebeNachHilf(f, k, 2, True);
      hRec := f[0];
      f[k].fontCol := DemoFarb[marker_QSort];
      Ausgabe(f, k, k);

      repeat
        ZeigePfeil(f, i, DemoFarb[vergleichen], 1, 0);
        ZeigePfeil(f, j, DemoFarb[vergleich2_QSort], 1, 0);
        ZeigeVergleich(f, 0, i, DemoFarb[vergleichen], 6, aktPause * 2);
        while feld[i] < hilf do
        begin
          Inc(i);
          ZeigeVergleich(f, 0, i, DemoFarb[vergleichen], 6, aktPause * 2);
        end;
        ZeigePfeil(f, i, DemoFarb[PosMarker], 1, 0);

        ZeigeVergleich(f, 0, j, DemoFarb[vergleich2_QSort], 6, aktPause * 2);
        while hilf < feld[j] do
        begin
          Dec(j);
          ZeigeVergleich(f, 0, j, DemoFarb[vergleich2_QSort], 6, aktPause * 2);
        end;
        ZeigePfeil(f, i, clWhite, 1, 0);

        if i <= j then
        begin
          if i < j then
          begin
            ZeigeVergleich(f, i, j, DemoFarb[verschiebung], 1, 0);
            VerschiebeNachHilf(f, i, 2, True);
            VerschiebeFeld(f, j, i, 2, True);
            VerschiebeFeld(f, 0, j, 2, False);
            ZeigeVergleich(f, i, j, clWhite, 1, 0);
            f[0] := hRec;
            Ausgabe(f, 0, 0);
            help := feld[i];
            feld[i] := feld[j];
            feld[j] := help;
          end;
          Inc(i);
          Dec(j);
        end
      until i > j;
      f[0].wert := -1;
      f[0].FrameCol := ClWhite;
      Ausgabe(f, 0, 0);
      f[0].FrameCol := clBlack;

      if j <= l then
      begin
        f[l].fontCol := DemoFarb[istSortiert];
        f[l].frameCol := DemoFarb[istSortiert];
      end;
      if i >= r then
      begin
        f[r].fontCol := DemoFarb[istSortiert];
        f[r].frameCol := DemoFarb[istSortiert];
      end;
      for k := j + 1 to i - 1 do
      begin
        f[k].fontCol := DemoFarb[istSortiert];
        f[k].FrameCol := DemoFarb[istSortiert];
      end;

      if (j >= l) and (i <= r) and (r - l > 1) then
      begin
        if (j + 1 <= i) then
          move(r + 1);
        if (j + 2 <= i) then
          move(r + 1);

        if (j + 1 <= i) then
          for k := j + 1 to maxFeld do
            f[k].x := f[k].x + 6;
        if (j + 2 <= i) then
          for k := i to maxFeld do
            f[k].x := f[k].x + 6;
      end;
      canvas.Pen.color := clWhite;
      canvas.Rectangle(f[l].x, f[l].y, f[r].x + feldWeite, f[r].y + feldhoehe);
      canvas.Pen.color := clBlack;
      Ausgabe(f, l, r);

      if l < j then
        Quick1(l, j, z + feldHoehe);
      if i < r then
        Quick1(i, r, z + feldHoehe);
    end;

  var
    k: integer;
  begin  {Quicksort}
    zeile := 2;
    InitFeldPos(Feld, maxFeld, zeile);
    Quick1(1, maxfeld, zeile + feldHoehe);
    for k := 1 to maxFeld do
      f[k].y := zeile + feldHoehe;
    ZeigeSortiert(f);
  end;  {Quicksort}

begin   { ZeigeSortDemo }
  aCanvas := aktPaintbox.Canvas;
  breakNr := 0;
  NeuZeigenZahlenFeld(zahlenFeld, 1, 0, 0);
  zeileNr := 1;
  case sortArt of
    sMin: SortiereMitMinSort(zahlenfeld, 1);
    sEin: SortiereMitEinSort(zahlenfeld, 1);
    sBinEin: SortiereMitBinSort(zahlenfeld, 1);
    sBubble1: SortiereMitBubble1Sort(zahlenfeld, 1);
    sBubble2: SortiereMitBubble2Sort(zahlenfeld, 1);
    sComb: SortiereMitCombSort(zahlenfeld, 1);
    sShake: SortiereMitShakeSort(zahlenfeld, 1);
    sShell: SortiereMitShellSort(zahlenfeld, 1);
    sHeap: SortiereMitHeapSort(zahlenfeld, 1);
    sQuick: SortiereMitQuickSort(zahlenfeld, 1);
    sMerge: SortiereMitMergeSort(zahlenfeld, 1);
    sFlash: SortiereMitFlashSort(zahlenfeld, 1);
  end;
end;

end.
