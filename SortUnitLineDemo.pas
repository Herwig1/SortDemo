unit SortUnitLineDemo;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Darstellung des Sortierverfahrens anhand der Sortierung von "Stäben"
   Autor:   H. Niemeyer  (c) 2010 - 2021, 2023, 2024
   Version: 1.8

   letzte Änderung: 17.01.2024 *)

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Spin, Types,
  SortUnitDefinitionen, SortUnitSprache, SortUnitAlgorithmen,
  SortUnitListArtDlg, SortFrame;

type

  { TFormLineDemo }

  TFormLineDemo = class(TForm)
    BitBtnAnimationEnde: TBitBtn;
    BitBtnAnimationPause: TBitBtn;
    BitBtnAnimationWeiter: TBitBtn;
    BitBtnEnde: TBitBtn;
    BitBtnListenArt: TBitBtn;
    BitBtnNewList: TBitBtn;
    mySortFrame: TFrame1;
    PanelPaint: TPanel;
    PanelAnimat: TPanel;
    PanelCommand: TPanel;
    PanelAbbruch: TPanel;
    PaneListe: TPanel;
    PaintBox1: TPaintBox;
    Splitter1: TSplitter;
    LabelVerzoegerung: TLabel;
    SpinEditPause: TSpinEdit;
    LabelMaxLaenge: TLabel;
    SpinEditLaenge: TSpinEdit;
    procedure BitBtnAnimationEndeClick(Sender: TObject);
    procedure BitBtnAnimationPauseClick(Sender: TObject);
    procedure BitBtnAnimationWeiterClick(Sender: TObject);
    procedure BitBtnListArtClick(Sender: TObject);
    procedure BitBtnNewListClick(Sender: TObject);
    procedure ButtonSortClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PanelAbbruchResize(Sender: TObject);
    procedure SpinEditPauseChange(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    { Private-Deklarationen }
    orgFeld, B: TTestFeld;
    farbe, tempFarbe: TColor;
    feldLang, lineX, line0, lineOben: integer;
    listArt: TListenTyp;
    neuZeichnen: boolean;
    procedure ButtonsAktivieren(ja: boolean);
    procedure SortLines(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp);
    procedure ZeichneLines(sorting: boolean);
  public
    { Public-Deklarationen }
  end;

var
  FormLineDemo: TFormLineDemo;

implementation

{$R *.lfm}

const
  aktpause: integer = 20;

procedure TFormLineDemo.BitBtnAnimationEndeClick(Sender: TObject);
begin
  abbruch := True;
end;

procedure TFormLineDemo.BitBtnAnimationPauseClick(Sender: TObject);
begin
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := False;
  BitBtnAnimationWeiter.Enabled := True;
  {$EndIf}
  showSteps := False;
  checkPause(0);
end;

procedure TFormLineDemo.BitBtnAnimationWeiterClick(Sender: TObject);
begin
  showSteps := True;
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := True;
  BitBtnAnimationWeiter.Enabled := False;
  {$EndIf}
end;

procedure TFormLineDemo.BitBtnListArtClick(Sender: TObject);
begin
  ListArtDlg := TListArtDlg.Create(self);
  ListArtDlg.SetMarkierung(listArt);
  if ListArtDlg.showModal = mrOk then
    listArt := ListArtDlg.GetListArt;
  ListArtDlg.Free;
end;

procedure TFormLineDemo.BitBtnNewListClick(Sender: TObject);
var
  k, p: integer;
  element: TElementTyp;
begin
  orgFeld := Default(TTestFeld);
  try
    feldLang := spinEditLaenge.Value
  except
    feldLang := paintbox1.ClientWidth - 2;
    if feldLang > maxFeldLaenge then feldLang := maxFeldLaenge;
  end;
  case listArt of
    zufall:
    begin
      randomize;
      for k := 1 to feldLang do
        orgFeld[k] := random(feldLang) + 1;
    end;
    ohneDoppel:
    begin
      for k := 1 to feldLang do
        orgFeld[k] := k;
      randomize;
      for k := 1 to feldLang do
      begin
        p := random(feldLang) + 1;
        element := orgFeld[k];
        orgFeld[k] := orgFeld[p];
        orgFeld[p] := element;
      end;
    end;
    sortiert: for k := 1 to feldLang do
        orgFeld[k] := k;
    umgekehrtSortiert: for k := 1 to feldLang do
        orgFeld[k] := feldLang - k + 1;
  end;
  paintbox1paint(nil);
  ZeichneLines(False);
end;

procedure TFormLineDemo.ButtonsAktivieren(ja: boolean);
begin
  mySortFrame.SortButtonsAktivieren(ja);
  BitBtnNewList.Enabled := ja;
  BitBtnListenArt.Enabled := ja;
  SpinEditLaenge.Enabled := ja;
  BitBtnAnimationEnde.Enabled := not ja;
  BitBtnAnimationPause.Enabled := not ja;
  {$IfNDef Windows}
  BitBtnAnimationWeiter.Enabled := not ja;
  {$EndIf}
  BitBtnEnde.Enabled := ja;
end;

procedure TFormLineDemo.ButtonSortClick(Sender: TObject);
var
  sortArt: TSortTyp;
begin
  sortArt := TSortTyp((Sender as TButton).tag);
  abbruch := False;
  ButtonsAktivieren(False);
  SortLines(orgFeld, feldLang, sortArt);
  paintbox1.canvas.pen.Color := paintBox1.Color;
  ButtonsAktivieren(True);
end;

procedure TFormLineDemo.FormActivate(Sender: TObject);
begin
  feldLang := (paintbox1.Width - offset - 1) div 2;
  SpinEditLaenge.Value := feldLang;
  SpinEditLaenge.MaxValue := maxFeldLaenge;
  SpinEditLaenge.MinValue := minFeldLaenge;
  feldLang := 0;
  listArt := ohneDoppel;
  SpinEditPause.Value := aktPause;
  mySortFrame.InitFrame(False, paintBox1);
  BitBtnNewListClick(Sender);
end;

procedure TFormLineDemo.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_DemoGraphDarstellung];
  BitBtnAnimationPause.Caption := sprachTab[s_AniAnhalten];
  BitBtnAnimationWeiter.Caption := sprachTab[s_AniFortsetzen];
  BitBtnAnimationEnde.Caption := sprachTab[s_AniAbbrechen];
  BitBtnEnde.Caption := sprachTab[s_Schliessen];
  BitBtnListenArt.Caption := sprachTab[s_ListenArt];
  BitBtnNewList.Caption := sprachTab[s_neueListe];
  LabelMaxLaenge.Caption := sprachTab[s_maxListenlaenge];
  LabelVerzoegerung.Caption := sprachTab[s_Verzoegerung];
end;

procedure TFormLineDemo.FormResize(Sender: TObject);
begin
  paintbox1.Width := PanelPaint.Width - 5;
  Splitter1Moved(nil);
end;

procedure TFormLineDemo.PaintBox1Paint(Sender: TObject);
var
  h, w, y: integer;
begin
  h := paintBox1.Height;
  w := paintBox1.Width;
  lineX := h - offSet;
  line0 := lineX - 1;
  lineOben := lineX - w;
  if lineOben < 0 then
    lineOben := 0;
  with paintbox1.canvas do
  begin
    pen.color := clWhite;
    rectangle(0, 0, w, paintBox1.Height);
    if not mySortFrame.buttonMinSort.Enabled then
      neuzeichnen := True;

    pen.color := clBlack;
    PolyLine([Point(0, lineX), Point(w, lineX), point(w - 3, lineX - 3),
      Point(w - 3, lineX + 3), point(w, lineX)]);
    TextOut(w - 10, lineX + 4, 'n');

    PolyLine([point(offset, h), point(offSet, 0), point(offset - 3, 3),
      point(offset + 3, 3), point(offset, 0)]);
    TextOut(offSet - 10, 4, 'l');
    y := 10000;
    while y > lineX do
      y := y div 10;
    h := lineX - round(y);
    PolyLine([point(offset - 5, h), point(offset + 5, h)]);
    TextOut(offset + 6, h - 8, IntToStr(y));
  end;
end;

procedure TFormLineDemo.PanelAbbruchResize(Sender: TObject);
begin
  BitBtnEnde.top := PanelAbbruch.ClientHeight - BitBtnEnde.Height - 10;
end;

procedure TFormLineDemo.SortLines(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp);

// Hilfsproceduren
  procedure LoescheBufferLine(xOffs, nr: integer);
  var
    x: integer;
  begin
    x := offset - 1 + (nr + xOffs) * 2;
    with paintBox1.Canvas do
    begin
      pen.color := clWhite;
      MoveTo(x, line0);
      LineTo(x, lineX - B[nr]);
    end;
  end;

  procedure LoescheLine(nr: integer);
  var
    x: integer;
  begin
    x := offset + nr * 2;
    with paintBox1.Canvas do
    begin
      pen.color := clWhite;
      MoveTo(x, line0);
      LineTo(x, line0 - feld[nr]);
    end;
  end;

  procedure LoeschePseudoBufferLine(nr: integer);
  var
    x: integer;
  begin
    x := offset + 1 + nr * 2;
    with paintBox1.Canvas do
    begin
      pen.color := clWhite;
      MoveTo(x, line0);
      LineTo(x, line0 - feld[nr]);
    end;
  end;

  procedure UeberschreibeLines(hilf, nach, verzoegerung: integer);
  var
    x: integer;
  begin
    if hilf = feld[nach] then
      exit;
    if neuZeichnen then
      ZeichneLines(True);
    x := offset + nach + nach;
    with paintBox1.Canvas do
    begin
      pen.color := tempFarbe;
      MoveTo(x, line0);
      LineTo(x, lineX - hilf);
      pen.color := clWhite;
      MoveTo(x, line0 - hilf);
      LineTo(x, lineOben);
    end;
    feld[nach] := hilf;
    WarteMS(verzoegerung);
  end;

  procedure VerschiebeFlashSortLines(hilf, von, nach, verzoegerung: integer);
  var
    startx, endx, step, x: integer;
  begin
    if (verzoegerung > 5) then
      if Abs(von - nach) > 1 then
        with paintBox1.Canvas do
        begin
          if von < nach then
          begin
            startx := offset - 1 + 2 * von;
            endx := offset - 1 + 2 * nach;
            step := 2;
          end
          else
          begin
            startx := offset + 1 + 2 * von;
            endx := offset + 1 + 2 * nach;
            step := -2;
          end;
          x := startX;
          while (x <> endX) do
          begin
            x := x + step;
            pen.color := clRed;
            MoveTo(x, line0);
            LineTo(x, lineX - hilf);
            WarteMS(verzoegerung);
            pen.color := clWhite;
            LineTo(x, line0);
            if abbruch then
              exit;
          end;
        end;
    UeberschreibeLines(hilf, nach, verzoegerung);
  end;

  procedure VerschiebeLines(von, nach, verzoegerung: integer);
  begin
    UeberschreibeLines(feld[von], nach, verzoegerung);
  end;

  procedure ZeichneBufferLines(xOffs, anz: integer);
  var
    k, x: integer;
  begin
    with paintBox1.Canvas do
    begin
      pen.color := clRed;
      x := offset - 1 + 2 * xOffs;
      for k := 1 to anz do
      begin
        MoveTo(x + k * 2, line0);
        LineTo(x + k * 2, lineX - B[k]);
      end;
    end;
  end;

  procedure ZeichneNeueFarbe(farb: TColor);
  var
    k, x: integer;
  begin
    with paintBox1.Canvas do
    begin
      pen.color := farb;
      x := offset;
      for k := 1 to laenge do
      begin
        MoveTo(x + k * 2, line0);
        LineTo(x + k * 2, lineX - feld[k]);
      end;
    end;
  end;

  procedure ZeichnePseudoBufferLines(von, bis: integer);
  var
    k: integer;
  begin
    with paintBox1.Canvas do
    begin
      pen.color := clGreen;
      for k := von to bis do
      begin
        MoveTo(offset + 1 + k * 2, line0);
        LineTo(offset + 1 + k * 2, lineX - feld[k]);
      end;
    end;
  end;

  procedure Austausch(n1, n2, verzoegerung: integer);
  var
    hilf: integer;
  begin
    hilf := feld[n1];
    UeberschreibeLines(feld[n2], n1, 10);
    UeberschreibeLines(hilf, n2, verzoegerung);
  end;

  // Sortierverfahren
  procedure Einsortieren;
  var
    k, anfang: integer;
  begin
    for anfang := 2 to laenge do
    begin
      feld[0] := feld[anfang];
      k := anfang - 1;
      while feld[0] < feld[k] do
      begin
        {feld[k+1]:=feld[k];} VerschiebeLines(k, k + 1, aktpause);
        Dec(k);
      end;
      //feld[k+1]:=feld[0];
      if abbruch then
        exit;
      VerschiebeLines(0, k + 1, aktpause * 2);
      if abbruch then
        exit;
    end;
  end;  { Einsortieren }

  procedure BinaeresEinsortieren;
  var
    i, m, j, k, l: integer;
    hilf: TElementTyp;
  begin
    for i := 2 to laenge do
    begin
      hilf := feld[i];
      l := 1;
      k := i - 1;
      while l <= k do
      begin
        m := (l + k) div 2;
        if hilf < feld[m] then
          k := m - 1
        else
          l := m + 1;
      end;
      for j := i - 1 downto l do
        VerschiebeLines(j, j + 1, aktpause); //feld[j+1]:=feld[j];
      // feld[l]:=hilf;
      UeberschreibeLines(hilf, l, aktpause * 2);
      if abbruch then
        exit;
    end;
  end;   { BinaeresEinsortieren }

  procedure MinSort;
  var
    l, k, anfang: integer;
    hilf: TElementTyp;
  begin
    for anfang := 1 to laenge - 1 do
    begin
      l := anfang;
      hilf := feld[anfang];
      for k := anfang + 1 to laenge do
        if feld[k] < hilf then
        begin
          l := k;
          hilf := feld[k];
        end;
      //feld[l]:=feld[anfang]; feld[anfang]:=hilf;
      VerschiebeLines(anfang, l, aktpause div 2);
      UeberschreibeLines(hilf, anfang, aktpause);
      if abbruch then
        exit;
    end;
  end;   { MinSort }

  procedure Bubble1Sort;
  var
    k, ende: integer;
  begin
    for ende := 2 to laenge do
      for k := laenge downto ende do
        if feld[k - 1] > feld[k] then
        begin
          Austausch(k, k - 1, aktpause);
          if abbruch then
            exit;
        end;
  end;   { Bubble1Sort }

  procedure Bubble2Sort;
  var
    k, ende: integer;
    fertig: boolean;
  begin
    ende := laenge;
    repeat
      ende := ende - 1;
      fertig := True;
      for k := 1 to ende do
        if feld[k] > feld[k + 1] then
        begin
          fertig := False;
          Austausch(k, k + 1, aktpause);
          if abbruch then
            exit;
        end
    until fertig;
  end;  { Bubble2Sort }

  procedure CombSort;    { s. BYTE 4/91   S. 315 ff }
  const
    schrumpfFaktor = 1.3;
  var
    k, j, weite: integer;
    fertig: boolean;
  begin
    weite := laenge;
    repeat
      weite := Trunc(weite / schrumpfFaktor);
      case weite of
        0: weite := 1;
        9, 10: weite := 11;
      end;
      fertig := True;
      j := weite;

      for k := 1 to laenge - weite do
      begin
        Inc(j);
        if feld[k] > feld[j] then
        begin
          // hilf:=feld[k]; feld[k]:=feld[j]; feld[j]:=hilf;
          Austausch(k, j, aktpause);
          fertig := False;
          if abbruch then
            exit;
        end;
      end
    until fertig and (weite = 1) or abbruch;
  end;  { CombSort }

  procedure ShakeSort;
  var
    j, k, l, r: integer;
  begin
    l := 2;
    r := laenge;
    k := laenge;
    repeat
      for j := r downto l do
        if feld[j - 1] > feld[j] then
        begin
          // hilf:=feld[j]; feld[j]:=feld[j-1]; feld[j-1]:=hilf;
          Austausch(j, j - 1, aktpause);
          k := j;
          if abbruch then
            exit;
        end;

      l := k + 1;
      for j := l to r do
        if feld[j - 1] > feld[j] then
        begin
          // hilf:=feld[j]; feld[j]:=feld[j-1]; feld[j-1]:=hilf;
          Austausch(j, j - 1, aktpause);
          if abbruch then
            exit;
          k := j;
        end;
      r := k - 1
    until (l > r) or abbruch;
  end;   { ShakSort }

  procedure ShellSort(step: TShellSteps; maxStep: integer);
  var
    i, j, k: integer;
    hilf: TElementTyp;
    m: 1..15;
  begin

    for m := 1 to maxStep do
    begin
      k := step[m];

      for i := k + 1 to laenge do
      begin
        hilf := feld[i];
        j := i - k;
        while (j > 0) and (hilf < feld[j]) do
        begin
          // feld[j+k]:=feld[j];
          VerschiebeLines(j, j + k, aktpause);
          if abbruch then
            exit;
          j := j - k;
        end;
        // feld[j+k]:=hilf;
        UeberschreibeLines(hilf, j + k, aktpause * 2);
        if abbruch then
          exit;
      end;
    end;

  end;   { ShellSort }

  procedure Heapsort;
  var
    l, r: integer;
    hilf: TElementTyp;

    procedure Sift;
    var
      i, j: integer;
      weiter: boolean;
    begin
      i := l;
      j := 2 * i;
      hilf := feld[i];
      weiter := (j <= r);
      while weiter do
      begin
        if j < r then
        begin
          if feld[j] < feld[j + 1] then
            Inc(j);
        end;
        if hilf < feld[j] then
        begin
          //  feld[i]:=feld[j];
          VerschiebeLines(j, i, aktpause);
          if abbruch then
            exit;
          i := j;
          j := 2 * i;
          weiter := (j <= r);
        end
        else
          weiter := False;
      end;
      // feld[i]:=hilf
      UeberschreibeLines(hilf, i, aktpause * 2);
      if abbruch then
        exit;
    end;

  begin  {Heapsort}
    l := (laenge div 2) + 1;
    r := laenge;
    while l > 1 do
    begin
      Dec(l);
      Sift;
    end;
    while r > 1 do
    begin
      // hilf:=feld[l]; feld[l]:=feld[r]; feld[r]:=hilf;
      Austausch(l, r, aktpause * 2);
      if abbruch then
        exit;
      Dec(r);
      Sift;
    end;
  end;  {Heapsort}

  procedure Quicksort;

    procedure Quick1(l, r: integer);
    var
      i, j: integer;
      hilf: TElementTyp;
    begin
      i := l;
      j := r;
      hilf := feld[(l + r) div 2];
      repeat
        while feld[i] < hilf do
          Inc(i);
        while hilf < feld[j] do
          Dec(j);
        if i <= j then
        begin
          if i < j then
          begin
            //  help:=feld[i]; feld[i]:=feld[j]; feld[j]:=help;
            Austausch(i, j, aktpause * 2);
            if abbruch then
              exit;
          end;
          Inc(i);
          Dec(j);
        end
      until i > j;
      if l < j then
        Quick1(l, j);
      if i < r then
        Quick1(i, r);
    end;

  begin  {Quicksort}
    Quick1(1, laenge);
  end;  {Quicksort}


  procedure StartMergeSort;

    procedure Merge(p, q, r: integer);
    var
      i, j, k, n1: integer;
    begin { Merge }
      n1 := q - p + 1;
      for k := 1 to n1 do
        B[k] := feld[p + k - 1];
      ZeichneBufferLines(p, n1);
      ZeichnePseudoBufferLines(q + 1, r);
      i := 1;
      j := q + 1;
      k := p;
      while ((i <= n1) and (j <= r)) and not abbruch do
      begin
        if (B[i] < feld[j]) then
        begin
          //  feld[k] := B[i];
          LoescheBufferLine(p, i);
          UeberschreibeLines(B[i], k, aktpause * 2);
          i := i + 1;
        end
        else
        begin
          //  feld[k] := feld[j];
          LoescheLine(j);
          LoeschePseudoBufferLine(j);
          UeberschreibeLines(feld[j], k, aktpause * 2);
          j := j + 1;
        end;
        k := k + 1;
      end;
      while (i <= n1) and not abbruch do
      begin
        // feld[k] := B[i];
        LoescheBufferLine(p, i);
        UeberschreibeLines(B[i], k, aktpause * 2);
        k := k + 1;
        i := i + 1;
      end;
      while j <= r do
      begin
        LoeschePseudoBufferLine(j);
        j := j + 1;
      end;

    end;

    procedure MergeSort(p, r: integer);
    var
      q: integer;
    begin { MergeSort }
      if (p < r) then
      begin
        q := (p + r) div 2;
        MergeSort(p, q);
        if abbruch then
          exit;
        MergeSort(q + 1, r);
        if abbruch then
          exit;
        Merge(p, q, r);
      end;
    end;

  begin
    MergeSort(1, laenge);
  end;

  procedure StartFlashSort;
  var
    stellenNr: array[1..100] of integer;
    maxKlassenNr: integer;
    feldMin: integer;
    c1: double;

    procedure KlassenVorbereiten;
    var
      nmax, i, k: integer;
    begin
      if laenge < 20 then
        k := 4
      else if laenge < 50 then
        k := 6
      else
        k := 10;
      maxKlassenNr := laenge div k;
      if maxKlassenNr = 0 then
        maxKlassenNr := 1;
      if maxKlassenNr > 100 then
        maxKlassenNr := 100;
      feldMin := feld[1];
      nmax := 1;
      for i := 2 to laenge do
      begin
        if feld[i] < feldMin then
          feldMin := feld[i]
        else if feld[i] > feld[nmax] then
          nmax := i;
      end;
      c1 := (maxKlassenNr - 1) / (feld[nmax] - feldMin);
      FillChar(stellenNr, SizeOf(stellenNr), 0);
      for i := 1 to laenge do
      begin
        k := 1 + trunc(c1 * (feld[i] - feldMin));
        stellenNr[k] := stellenNr[k] + 1;
      end;
      for k := 2 to maxKlassenNr do
        stellenNr[k] := stellenNr[k] + stellenNr[k - 1];
      Austausch(nmax, 1, aktpause * 2);
    end;

    procedure Perm;
    var
      nmove, j, k, flashPos: integer;
      FLASH, temp: TElementTyp;
    begin
      nmove := 0;
      j := 1;
      k := maxKlassenNr;
      while (nmove < (laenge - 1)) do
      begin
        while j > stellenNr[k] do
        begin
          j := j + 1;
          k := 1 + Trunc(c1 * (feld[j] - feldMin));
        end;
        FLASH := feld[j];
        flashPos := j;
        while (j <> (stellenNr[k] + 1)) do
        begin
          k := 1 + Trunc(c1 * (FLASH - FeldMin));
          temp := feld[stellenNr[k]];
          //  UeberschreibeLines(flash,stellenNr[k],aktpause);
          //  feld[stellenNr[k]] := FLASH;
          VerschiebeFlashSortLines(flash, flashPos, stellenNr[k], aktpause);
          if abbruch then
            exit;
          FLASH := temp;
          flashPos := stellenNr[k];
          stellenNr[k] := stellenNr[k] - 1;
          nmove := nmove + 1;
        end;
      end;
    end;

    procedure Insert;
    var
      i, j: integer;
      temp: TElementTyp;
    begin
      for i := laenge - 2 downto 1 do
      begin
        if feld[i + 1] < feld[i] then
        begin
          temp := feld[i];
          j := i;
          while feld[j + 1] < temp do
          begin
            //  feld[j] := feld[j + 1];
            UeberschreibeLines(feld[j + 1], j, aktpause);
            j := j + 1;
            if abbruch then
              exit;
          end;
          UeberschreibeLines(temp, j, aktpause);
          //  feld[j] := temp
        end;
        if abbruch then
          exit;
      end;
      ZeichneNeueFarbe(farbe);
    end;

  begin
    KlassenVorbereiten;
    if not abbruch then
      Perm;
    tempFarbe := farbe;
    if not abbruch then
      Insert;
  end;

begin { SortLines }
  farbe := SortFarb[sortTyp];
  tempFarbe := farbe;
  paintbox1paint(nil);
  ZeichneLines(True);
  abbruch := False;
  case sortTyp of
    sMin: MinSort;
    sEin: Einsortieren;
    sBinEin: BinaeresEinsortieren;
    sBubble1: Bubble1Sort;
    sBubble2: Bubble2Sort;
    sComb: CombSort;
    sShake: ShakeSort;
    sShell: ShellSort(shellSortSteps, 15);
    sHeap: HeapSort;
    sQuick: QuickSort;
    sMerge: StartMergeSort;
    sFlash:
    begin
      tempFarbe := clBlue;
      StartFlashSort;
    end;
  end;
end;

procedure TFormLineDemo.SpinEditPauseChange(Sender: TObject);
begin
  try
    aktPause := SpinEditPause.Value * 2;
  finally
  end;
end;

procedure TFormLineDemo.Splitter1Moved(Sender: TObject);
var
  neuLang: integer;
begin
  neuLang := (paintbox1.Width - offset - 1) div 2;
  if neuLang < 0 then
    paintbox1.Width := offset + offset div 2;
  if (minFeldLaenge <= neuLang) and (neuLang <= maxFeldLaenge) then
    SpinEditLaenge.Value := neuLang;
end;

procedure TFormLineDemo.ZeichneLines(sorting: boolean);
var
  k: integer;
begin
  with paintBox1.Canvas do
  begin
    if sorting then
      pen.color := farbe
    else
      pen.color := clBlack;
    for k := 1 to feldLang do
    begin
      MoveTo(offset + k * 2, line0);
      LineTo(offset + k * 2, lineX - orgFeld[k]);
    end;
    if sorting then
    begin
      pen.color := clWhite;
      for k := 1 to feldLang do
      begin
        MoveTo(offset + k * 2, 0);
        LineTo(offset + k * 2, line0 - orgFeld[k]);
      end;
      neuzeichnen := False;
    end;
  end;
end;

end.
