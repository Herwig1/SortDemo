unit SortUnitMessungen;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Graphische Darstellung der Sortierzeiten, Verschiebungen oder Vergleiche in Abhängigkeit der Listenlänge
   Autor:   H. Niemeyer  (c) 2010 - 2020, 2023, 2024
   Version: 1.8

   letzte Änderung: 20.01.2024 *)

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Types, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Spin, ComCtrls,
  epiktimer,
  SortUnitDefinitionen, SortUnitSprache, SortUnitAlgorithmen,
  SortUnitAusgleich, SortUnitAuswahlDlg, SortUnitListArtDlg, SortUnitKostenDlg,
  SortUnitKurveDlg, SimpFunktion, SortFrame;

const
  durchlaufVorgabe = 20;

type
  TVglSpeicher = array[TSortTyp] of TSpeicher;
  TLongIntSpeicher = array[TSortTyp] of longword;

  { TFormMessung }

  TFormMessung = class(TForm)
    BitBtnKosten: TBitBtn;
    BitBtnSchliessen: TBitBtn;
    mySortFrame: TFrame1;
    LabelMaxListLength: TLabel;
    PanelCommand: TPanel;
    PanelAbbruch: TPanel;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Splitter1: TSplitter;
    PanelEinstellungen: TPanel;
    TrackBar1: TTrackBar;
    LabelCPUPause: TLabel;
    SpinEditRecoverTime: TSpinEdit;
    PanelAnzahlen: TPanel;
    LabelmaxLaenge: TLabel;
    SpinEditLaenge: TSpinEdit;
    LabelAnzDurchlauf: TLabel;
    SpinEditDurchLauf: TSpinEdit;
    CheckBoxAlleZeigen: TCheckBox;
    PanelStart: TPanel;
    ButtonSortAll: TButton;
    BitBtnClearDiagramm: TBitBtn;
    BitBtnListenArt: TBitBtn;
    BitBtnSortierverfahren: TBitBtn;
    BitBtnGraphicNeuzeichnen: TBitBtn;
    PanelAnimat: TPanel;
    BitBtnAnimationEnde: TBitBtn;
    BitBtnAnimationPause: TBitBtn;
    BitBtnAnimationWeiter: TBitBtn;
    procedure BitBtnAnimationEndeClick(Sender: TObject);
    procedure BitBtnAnimationPauseClick(Sender: TObject);
    procedure BitBtnAnimationWeiterClick(Sender: TObject);
    procedure BitBtnClearDiagrammClick(Sender: TObject);
    procedure BitBtnKostenClick(Sender: TObject);
    procedure BitBtnListArtClick(Sender: TObject);

    procedure BitBtnSortierverfahrenClick(Sender: TObject);
    procedure ButtonSortClick(Sender: TObject);
    procedure ButtonSortAllClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Splitter1Moved(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnGraphicNeuzeichnenClick(Sender: TObject);
  private
    { Private-Deklarationen }
    feld: TTestFeld;
    messung, vglFeld, kFeld: TVglSpeicher;
    bestF, theorie: array[TSortTyp] of TFkt;
    lineX, vergleichsArt: integer;
    yFaktor, fvKosten: real;
    auswahl: TSortierVerfahren;
    gewaehlt: array of TSortTyp;
    listArt: TListenTyp;
    myTimer: TEpikTimer;
    procedure AusgleichsKurve(const daten: TVglSpeicher; sortArt: TSortTyp;
      regTyp: integer);
    procedure ButtonsAktivieren(ja: boolean);
    function FaktorBestimmen(laenge: integer): real;
    procedure FortschrittsAnzeigeAndCheck(const aktLang: integer);
    procedure FortschrittsAnzeigeLoeschen(mxLang: integer);
    procedure FuelleZahlenFeld(bis: integer);
    function GetAchsName: char;
    procedure GetGewaehlt;
    procedure GewaehltAdd(aArt: TSortTyp);
    procedure SetCaptionKosten;
    procedure TesteZeiten(sortArt: TSortTyp);
    procedure TesteVerschiebungenUndVergleiche(sortArt: TSortTyp);
    procedure Vorbereiten(out laenge, durchlauf: integer; var faktor: real;
      out jedenAnzeigen: boolean);
    procedure ZeichneFunktion(aFkt: TFkt; yFak: double);
    procedure ZeichneMarkierung(x: integer; farbe: TColor);
  public
    { Public-Deklarationen }
    procedure SetVergleichsArt(aArt: integer);
  end;

var
  FormMessung: TFormMessung;

implementation

{$R *.lfm}

function xU(aX: integer): integer; inline;       // Umrechnung in x-Koordinate
begin
  Result := aX + offset;
end;

procedure TFormMessung.AusgleichsKurve(const daten: TVglSpeicher;
  sortArt: TSortTyp; regTyp: integer);
var
  s, t, fName: string;
  m, c, d: double;
  myMsgBox: TFormKurveDlg;
  myFkt: TFkt;
begin
  AusgleichFunktion(daten[sortArt], regTyp, m, c, d, s);
  myMsgBox := TFormKurveDlg.Create(self);
  fName := GetAchsName + '(n) = ';
  myMsgBox.Init(sortart, s, fName);
  myMsgBox.ShowModal;
  if myMsgBox.ModalResult = mrYes then
  begin
    myFkt := TFkt.Create(self);
    myFkt.DiagrammName := SortName[sortArt] + 'A';
    myFkt.color := SortFarb[sortArt];
    myFkt.Formel := s;
    myFkt.Visible := True;
    bestF[sortArt] := myFkt;
    ZeichneFunktion(bestF[sortArt], yFaktor);
  end;
  if myMsgBox.ModalResult = mrTheorie then
  begin
    FunktionEditor := TFunktionEditor.Create(self);
    myFkt := TFkt.Create(self);
    if theorie[sortArt] <> nil then
      myFkt.Assign(theorie[sortArt])
    else
    begin
      myFkt.DiagrammName := SortName[sortArt];
      myFkt.color := SortFarb[sortArt];
      myFkt.Formel := s;
    end;
    t := sortName[sortArt] + ': ' + sprachTab[s_TheorieKurve] + fName; //' t(n) =';
    FunktionEditor.Caption := t;
    if FunktionEditor.ExecuteDlg(myFkt) then
    begin
      if theorie[sortArt] <> nil then
      begin
        theorie[sortArt].Assign(myFkt);
        myFkt.Free;
      end
      else
        theorie[sortArt] := myFkt;
      theorie[sortArt].Visible := True;
      ZeichneFunktion(theorie[sortArt], yFaktor);
    end
    else
      myFkt.Free;
    FunktionEditor.Free;
  end;
  myMsgBox.Free;
end;

procedure TFormMessung.BitBtnAnimationEndeClick(Sender: TObject);
begin
  abbruch := True;
end;

procedure TFormMessung.BitBtnAnimationPauseClick(Sender: TObject);
begin
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := False;
  BitBtnAnimationWeiter.Enabled := True;
  {$EndIf}
  showSteps := False;
  // KEIN CheckStop(0); da sonst die Zeitmessungen verfälscht werden!
end;

procedure TFormMessung.BitBtnAnimationWeiterClick(Sender: TObject);
begin
  showSteps := True;
  {$IfDef Windows}
  BitBtnAnimationPause.Enabled := True;
  BitBtnAnimationWeiter.Enabled := False;
  {$EndIf}
end;

procedure TFormMessung.BitBtnClearDiagrammClick(Sender: TObject);
var
  sortArt: TSortTyp;
begin
  gewaehlt := nil;
  Fillchar(messung, SizeOf(messung), 0);
  for sortArt := sMin to sLastSort do
  begin
    if bestF[sortArt] <> nil then
    begin
      bestF[sortArt].Free;
      bestF[sortArt] := nil;
    end;
    if theorie[sortArt] <> nil then
    begin
      theorie[sortArt].Free;
      theorie[sortArt] := nil;
    end;
  end;
  paintbox1.Repaint;
end;

procedure TFormMessung.BitBtnKostenClick(Sender: TObject);
begin
  if vergleichsArt = vglKosten then
  begin
    KostenDlg := TKostenDlg.Create(self);
    KostenDlg.Start(fvKosten);
    if KostenDlg.ShowModal = mrOk then
    begin
      fvKosten := KostenDlg.GetFaktor;
      SetCaptionKosten;
    end;
    KostenDlg.Free;
  end;
end;

procedure TFormMessung.BitBtnGraphicNeuzeichnenClick(Sender: TObject);
begin
  paintbox1.Repaint;
end;

procedure TFormMessung.BitBtnListArtClick(Sender: TObject);
var
  b: boolean;
begin
  ListArtDlg := TListArtDlg.Create(self);
  ListArtDlg.SetMarkierung(listArt);
  if ListArtDlg.showModal = mrOk then
  begin
    listArt := ListArtDlg.GetListArt;
    b := (listart <= ohneDoppel);
    if b then
      SpinEditDurchLauf.Color := clWhite
    else
      SpinEditDurchLauf.Color := clSilver;
    SpinEditDurchLauf.Enabled := b;
    CheckBoxAlleZeigen.Enabled := b;
  end;
  ListArtDlg.Free;
end;

procedure TFormMessung.ButtonsAktivieren(ja: boolean);
begin
  mySortFrame.SortButtonsAktivieren(ja);
  buttonSortAll.Enabled := ja and (auswahl <> []);
  BitBtnSortierverfahren.Enabled := ja;
  BitBtnGraphicNeuzeichnen.Enabled := ja and (auswahl <> []);
  BitBtnClearDiagramm.Enabled := ja;
  BitBtnListenArt.Enabled := ja;
  SpinEditDurchLauf.Enabled := ja;
  SpinEditLaenge.Enabled := ja;
  CheckBoxAlleZeigen.Enabled := ja;
  BitBtnAnimationEnde.Enabled := not ja;
  BitBtnAnimationPause.Enabled := not ja;
  {$IfNDef Windows}
  BitBtnAnimationWeiter.Enabled := not ja;
  {$EndIf}
  BitBtnKosten.Enabled := ja;
  BitBtnSchliessen.Enabled := ja;
end;

procedure TFormMessung.BitBtnSortierverfahrenClick(Sender: TObject);
begin
  SortAuswahlDlg := TSortAuswahlDlg.Create(self);
  SortAuswahlDlg.Vorbereiten(auswahl);
  if SortAuswahlDlg.showModal = mrOk then
    SortAuswahlDlg.NeueAuswahl(auswahl);
  SortAuswahlDlg.Free;
  buttonSortAll.Enabled := (auswahl <> []);
end;

procedure TFormMessung.ButtonSortAllClick(Sender: TObject);
var
  maxLaenge, durchLauf, n: integer;
  jedenAnzeigen: boolean;
  sortArt: TSortTyp;

  procedure AlleZeiten;
  var
    k, n, aktLaenge, recoverTime: integer;
    zeit: TLongIntSpeicher;
    tHilf: longword;
    sortArt: TSortTyp;
  begin
    messung := Default(TVglSpeicher);
    recoverTime := SpinEditRecoverTime.Value;
    for aktLaenge := minFeldLaenge to maxLaenge do
    begin
      zeit := Default(TLongIntSpeicher);
      for k := 1 to durchLauf do
      begin
        FuelleZahlenFeld(aktLaenge);
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          TestSortZeit(feld, aktlaenge, sortArt, myTimer, thilf);
          zeit[sortArt] := zeit[sortArt] + tHilf;
          if jedenAnzeigen then
            paintbox1.canvas.pixels[xU(aktLaenge), lineX - round(thilf / yFaktor)] :=
              SortFarb[sortArt];
        end;
        checkPause(0);
        sleep(recoverTime);   //Processor in normalen Takt (?)
      end;
      for n := 0 to High(gewaehlt) do
      begin
        sortArt := gewaehlt[n];
        thilf := round(zeit[sortArt] / durchLauf);
        messung[sortArt, aktLaenge] := thilf;
        paintbox1.canvas.pixels[xU(aktLaenge), lineX - round(thilf / yFaktor)] :=
          SortFarb[sortArt];
      end;
      FortschrittsAnzeigeAndCheck(aktLaenge);
      if abbruch then
        break;
    end;
  end;

  procedure AlleVerschiebungen;
  var
    k, n, aktLaenge, y: integer;
    vglHilf, verschiebHilf, kostenHilf: longword;
    vgl, verschieb, kosten: TLongIntSpeicher;
    sortArt: TSortTyp;
    y0: single;
  begin
    vglFeld := Default(TVglSpeicher);
    kFeld := Default(TVglSpeicher);
    y0 := durchlauf * yFaktor;
    y := lineX;
    for aktLaenge := minFeldLaenge to maxLaenge do
    begin
      vgl := Default(TLongIntSpeicher);
      verschieb := Default(TLongIntSpeicher);
      kosten := Default(TLongIntSpeicher);
      for k := 1 to durchLauf do
      begin
        FuelleZahlenFeld(aktLaenge);
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          TestSortVergl(feld, aktlaenge, sortArt, vglHilf, verschiebHilf);
          kostenHilf := Round(verschiebHilf * fvKosten + vglHilf);
          if jedenAnzeigen then
          begin
            case vergleichsArt of
              vglVerschieb: y := lineX - round(verschiebhilf / yFaktor);
              vglVergleich: y := lineX - round(vglhilf / yFaktor);
              vglKosten: y :=
                  lineX - round((verschiebHilf * fvKosten + vglHilf) / yFaktor);
            end;
            paintbox1.canvas.pixels[xU(aktLaenge), y] := SortFarb[sortArt];
          end;
          vgl[sortArt] := vgl[sortArt] + vglHilf;
          verschieb[sortArt] := verschieb[sortArt] + verschiebHilf;
          kosten[sortArt] := kosten[sortArt] + kostenHilf;
        end;
        checkPause(0);
      end;
      for n := 0 to High(gewaehlt) do
      begin
        sortArt := gewaehlt[n];
        vglHilf := vgl[sortArt];
        vglFeld[sortArt, aktLaenge] := vglHilf;
        verschiebHilf := verschieb[sortArt];
        messung[sortArt, aktLaenge] := verschiebHilf;
        case vergleichsArt of
          vglVerschieb: y := lineX - round(verschiebHilf / y0);
          vglVergleich: y := lineX - round(vglHilf / y0);
          vglKosten: begin
            kostenHilf := kosten[sortArt];
            kFeld[sortArt, aktLaenge] := kostenHilf;
            y := lineX - round(kostenHilf / y0);
          end;
        end;
        paintbox1.canvas.pixels[xU(aktLaenge), y] := SortFarb[sortArt];
      end;
      FortschrittsAnzeigeAndCheck(aktLaenge);
      if abbruch then
        break;
    end;
  end;

begin
  GetGewaehlt;
  Vorbereiten(maxlaenge, durchlauf, yfaktor, jedenAnzeigen);
  messung := Default(TVglSpeicher);
  if vergleichsArt = vglZeiten then AlleZeiten
  else
    AlleVerschiebungen;
  for n := 0 to High(gewaehlt) do
  begin
    sortArt := gewaehlt[n];
    if vergleichsArt = vglZeiten then messung[sortArt, 0] := 1
    else
    begin
      messung[sortArt, 0] := durchLauf;
      vglFeld[sortArt, 0] := durchLauf;
      if vergleichsArt = vglKosten then kFeld[sortArt, 0] := durchlauf;
    end;
  end;
  FortschrittsAnzeigeLoeschen(maxLaenge);
  ButtonsAktivieren(True);
end;

procedure TFormMessung.ButtonSortClick(Sender: TObject);
var
  sortArt: TSortTyp;
begin
  sortArt := TSortTyp((Sender as TButton).tag);
  gewaehltAdd(sortArt);
  if vergleichsArt = vglZeiten then TesteZeiten(sortArt)
  else
    TesteVerschiebungenUndVergleiche(sortArt);
end;

function TFormMessung.FaktorBestimmen(laenge: integer): real;
begin
  if laenge <= 50 then
    Result := 0.25
  else if laenge <= 100 then
    Result := 1
  else
    Result := 10;
  if vergleichsArt = vglZeiten then Result := Result / 4;
  case trackbar1.Position of
    0: Result := Result / 10;
    1: Result := Result / 8;
    2: Result := Result / 5;
    3: Result := Result / 3;
    4: Result := Result / 2;

    6: Result := Result * 2;
    7: Result := Result * 4;
    8: Result := Result * 8;
    9: Result := Result * 14;
    10: Result := Result * 22;
  end;
end;

procedure TFormMessung.FormActivate(Sender: TObject);
begin
  SpinEditDurchLauf.Value := durchlaufVorgabe;
  SpinEditLaenge.Value := paintbox1.Width - offset + minFeldLaenge;
  SpinEditLaenge.MaxValue := maxFeldLaenge;
  SpinEditLaenge.MinValue := minFeldLaenge;
  listArt := zufall;
  yFaktor := 10;
  auswahl := [sMin, sEin, sBinEin, sBubble1, sBubble2, sShake, sComb,
    sShell, sHeap, sQuick, sMerge, sFlash];
  mySortFrame.InitFrame(False, paintBox1);
  gewaehlt := nil;
end;

procedure TFormMessung.FormCreate(Sender: TObject);
begin
  ButtonSortAll.Caption := sprachTab[s_vergleicheVerfahren];
  LabelAnzDurchlauf.Caption := sprachTab[s_AnzahlDurchlauf];
  CheckBoxAlleZeigen.Caption := sprachTab[s_jedenAnzeigen];
  LabelMaxListLength.Caption := sprachTab[s_maxListenlaenge];
  BitBtnSortierverfahren.Caption := sprachTab[s_Sortierverfahren];
  BitBtnListenArt.Caption := sprachTab[s_AuswahListenart];
  BitBtnGraphicNeuzeichnen.Caption := sprachTab[s_Neuzeichnen];
  BitBtnClearDiagramm.Caption := sprachTab[s_DiagrammLoeschen];
  LabelCPUPause.Caption := sprachTab[s_PauseCPU];
  BitBtnAnimationPause.Caption := sprachTab[s_AniAnhalten];
  BitBtnAnimationWeiter.Caption := sprachTab[s_AniFortsetzen];
  BitBtnAnimationEnde.Caption := sprachTab[s_AniAbbrechen];
  BitBtnSchliessen.Caption := sprachTab[s_Schliessen];
  myTimer := TEpikTimer.Create(self);
end;

procedure TFormMessung.FormDestroy(Sender: TObject);
var
  s: TSortTyp;
begin
  myTimer.Free;
  for s := sMin to sLastSort do
  begin
    if theorie[s] <> nil then
      theorie[s].Free;
    if bestF[s] <> nil then
      bestF[s].Free;
  end;
  if F_Liste <> nil then F_Liste.Free;
end;

procedure TFormMessung.FormResize(Sender: TObject);
begin
  paintbox1.Width := panel1.Width - 5;
  Splitter1Moved(nil);
end;

procedure TFormMessung.FortschrittsAnzeigeAndCheck(const aktLang: integer);
begin
  Paintbox1.Canvas.Pixels[xU(aktLang), lineX + offset - 2] := clBlack;
  Paintbox1.Canvas.Pixels[xU(aktLang), lineX + offset - 3] := clBlack;
  checkPause(0);
end;

procedure TFormMessung.FortschrittsAnzeigeLoeschen(mxLang: integer);
begin
  paintbox1.canvas.pen.Color := paintBox1.Color;
  paintbox1.canvas.pen.Width := 2;
  paintbox1.canvas.polyLine([Point(xU(1), lineX + offset - 2), Point(
    xU(mxLang), lineX + offset - 2)]);
  ZeichneMarkierung(mxLang, paintBox1.Color);
  ButtonsAktivieren(True);
end;

procedure TFormMessung.FuelleZahlenFeld(bis: integer);
var
  k, p: integer;
  element: TElementTyp;
begin
  case listArt of
    zufall:
    begin
      randomize;
      for k := 1 to bis do
        feld[k] := random(maxZahl);
    end;
    ohneDoppel:
    begin
      for k := 1 to bis do
        feld[k] := k;
      randomize;
      for k := 1 to bis do
      begin
        p := random(bis) + 1;
        element := feld[k];
        feld[k] := feld[p];
        feld[k] := element;
      end;
    end;
    sortiert: for k := 1 to bis do
        feld[k] := k;
    umgekehrtSortiert: for k := 1 to bis do
        feld[k] := maxFeldLaenge - k;
  end;
end;

function TFormMessung.GetAchsName: char;
begin
  case vergleichsArt of
    vglZeiten: Result := 't';
    vglKosten: Result := 'k';
    else
      Result := 'v';
  end;
end;

procedure TFormMessung.GetGewaehlt;
var
  sortArt: TSortTyp;
  n: integer;
begin
  gewaehlt := nil;
  n := 0;
  for sortArt := sMin to sLastSort do
    if sortArt in auswahl then
    begin
      n := n + 1;
      SetLength(gewaehlt, n);
      gewaehlt[n - 1] := sortArt;
    end;
end;

procedure TFormMessung.GewaehltAdd(aArt: TSortTyp);
var
  n: integer;
begin
  for n := 0 to High(gewaehlt) do
    if gewaehlt[n] = aArt then exit;
  n := Length(gewaehlt);
  SetLength(gewaehlt, n + 1);
  gewaehlt[n] := aArt;
end;

procedure TFormMessung.PaintBox1Paint(Sender: TObject);
var
  h, w, k, xMax: integer;
  y: longint;
  sortArt: TSortTyp;
  y0: real;
  farb: TColor;
  zeigeF: boolean;

  procedure zeichne(daten: TSpeicher);
  begin
    if daten[0] > 0 then
    begin
      y0 := daten[0] * yFaktor;
      while (k < xMax) and (daten[k] = 0) do k := k + 1;
      with paintbox1.canvas do
        while (k <= xMax) do
        begin
          if (daten[k] <> 0) then
            pixels[offset + k, lineX - round(daten[k] / y0)] := farb;
          k := k + 1;
        end;
    end;
  end;

begin
  h := paintBox1.Height;
  w := paintBox1.Width;
  lineX := h - offSet;

  xMax := w - offset;
  k := SpinEditLaenge.Value;
  if k < xMax then xMax := k;

  paintbox1.canvas.pen.color := clWhite;
  paintbox1.canvas.rectangle(0, 0, w, h);

  for sortArt := sMin to sLastSort do
    if (sortArt in auswahl) then
    begin
      farb := SortFarb[sortArt];
      zeigeF := False;
      k := minFeldLaenge + 1;
      case vergleichsArt of
        vglZeiten,
        vglVerschieb: if messung[sortArt, 0] > 0 then
          begin
            zeichne(messung[sortArt]);
            zeigeF := True;
          end;
        vglVergleich: if (vglFeld[sortArt, 0] > 0) then
          begin
            zeichne(vglFeld[sortArt]);
            zeigeF := True;
          end;
        vglKosten: if (kFeld[sortArt, 0] > 0) then
          begin
            zeichne(kFeld[sortArt]);
            zeigeF := True;
          end;
      end;
      if zeigeF then
      begin
        ZeichneFunktion(bestF[sortArt], yFaktor);
        ZeichneFunktion(theorie[sortArt], yFaktor);
      end;
    end;

  with paintbox1.canvas do
  begin
    // Zeichne Achsen
    pen.color := clBlack;
    PolyLine([Point(0, lineX), Point(w, lineX), point(w - 3, lineX - 3),
      Point(w - 3, lineX + 3), point(w, lineX)]);
    TextOut(w - 10, lineX + 4, 'n');

    PolyLine([point(offset, h), point(offSet, 0), point(offset - 3, 3),
      point(offset + 3, 3), point(offset, 0)]);
    TextOut(offSet - 10, 4, GetAchsName);
    // Markiere einen Wert auf Hochachse
    y := 100000;
    while y / yFaktor > lineX do
      y := y div 10;
    h := lineX - round(y / yFaktor);
    PolyLine([point(offset - 5, h), point(offset + 5, h)]);
    TextOut(offset + 6, h - 8, IntToStr(y));
  end;
end;

procedure TFormMessung.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  n, mx, my, dist: integer;
  sortArt, aktSort: TSortTyp;

  procedure CheckDistanz(sortArt: TSortTyp; yWert, my: integer);
  begin
    if Abs(yWert - my) < dist then
    begin
      dist := Abs(yWert - my);
      aktSort := sortArt;
    end;
  end;

begin
  if not mySortFrame.buttonMinSort.Enabled then exit;
  if (button = mbRight) then
  begin
    mx := x - offset;
    if (mx < 2) or (mx > SpinEditLaenge.Value - 2) then
      exit;
    dist := 30000;
    my := Round((lineX - y) * yFaktor);
    aktSort := gewaehlt[0];
    ;
    case vergleichsArt of
      vglZeiten: begin
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          if messung[sortArt, 0] >= 1 then
            CheckDistanz(sortArt, round(messung[sortArt, mx] / messung[sortArt, 0]), my);
        end;
        AusgleichsKurve(messung, aktSort, bestSort_regress);
      end;

      vglVerschieb: begin
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          if messung[sortArt, 0] >= 1 then
            CheckDistanz(sortArt, messung[sortArt, mx], my * messung[sortArt, 0]);
        end;
        AusgleichsKurve(messung, aktSort, bestSort_regress);
      end;

      vglVergleich: begin
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          if vglFeld[sortArt, 0] >= 1 then
            CheckDistanz(sortArt, vglFeld[sortArt, mx], my * vglFeld[sortArt, 0]);
        end;
        AusgleichsKurve(vglFeld, aktSort, bestSort_regress);
      end;

      vglKosten: begin
        for n := 0 to High(gewaehlt) do
        begin
          sortArt := gewaehlt[n];
          if kFeld[sortArt, 0] >= 1 then
            CheckDistanz(sortArt, kFeld[sortArt, mx], my * kFeld[sortArt, 0]);
        end;
        AusgleichsKurve(kFeld, aktSort, bestSort_regress);
      end;
    end;
  end
  else if vergleichsArt <> vglZeiten then
    BitBtnKostenClick(Sender);
end;

procedure TFormMessung.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  s: string;
  r: TRect;
begin
  r.left := offset + 10;
  r.Right := offset + 200;
  r.top := lineX + 1;
  r.bottom := lineX + offset - 1;
  s := IntToStr(x - offset + minFeldLaenge) + '/' +
    IntToStr(Round((lineX - y) * yFaktor));
  with paintbox1.canvas do
  begin
    pen.color := clwhite;
    Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    pen.color := clBlack;
    TextOut(r.left + 1, r.top + 1, s);
  end;
end;

procedure TFormMessung.SetCaptionKosten;
var
  s: string;
begin
  if fvKosten = 0 then fvKosten := 2.0;
  s := ' (1:' + FloatToStr(fvKosten) + ') ';
  Caption := sprachTab[s_Kosten] + s + sprachTab[s_nachListenLaenge];
end;

procedure TFormMessung.SetVergleichsArt(aArt: integer);
var
  t: string;
begin
  vergleichsArt := aArt;
  if vergleichsArt = vglZeiten then self.Caption := sprachTab[s_CaptionSortierzeiten]
  else
  begin
    LabelCPUPause.Visible := False;
    SpinEditRecoverTime.Visible := False;
    t := ' ' + sprachTab[s_nachListenLaenge];
    case aArt of
      vglVerschieb: Caption := sprachTab[s_AnzahlVerschiebungen] + t;
      //'Anzahl der Verschiebungen in Abhängigkeit von der Listenlänge';
      vglVergleich: Caption := sprachTab[s_AnzahlVergleiche] + t;
      //'Anzahl der Vergleiche in Abhängigkeit von der Listenlänge';
      vglKosten: SetCaptionKosten;
    end;
  end;
  BitBtnKosten.Visible := (vergleichsArt = vglKosten);
end;

procedure TFormMessung.Splitter1Moved(Sender: TObject);
var
  neuLang: integer;
begin
  neuLang := paintbox1.Width - offset + minFeldLaenge;
  if neuLang < 0 then
    paintbox1.Width := offset + offset div 2;
  if (minFeldLaenge <= neuLang) and (neuLang <= maxFeldLaenge) then
    SpinEditLaenge.Value := neuLang;
end;

procedure TFormMessung.TesteVerschiebungenUndVergleiche(sortArt: TSortTyp);
var
  k, aktLaenge, maxLaenge, durchlauf: integer;
  vgl, vglHilf, verschieb, verschiebHilf: longword;
  jedenAnzeigen: boolean;
  y0: single;
  farb: TColor;

  function yU(aY: longint): integer; inline;
  begin
    Result := lineX - round(aY / yFaktor);
  end;

begin
  Vorbereiten(maxLaenge, durchLauf, yFaktor, jedenAnzeigen);
  for k := 0 to maxFeldLaenge do
  begin
    messung[sortArt, k] := 0;
    vglFeld[sortArt, k] := 0;
    kFeld[sortArt, k] := 0;
  end;
  y0 := durchlauf * yFaktor;
  farb := SortFarb[sortArt];
  for aktLaenge := minFeldLaenge to maxLaenge do
  begin
    vgl := 0;
    verschieb := 0;
    for k := 1 to durchlauf do
    begin
      FuelleZahlenFeld(aktLaenge);
      TestSortVergl(feld, aktlaenge, sortArt, vglHilf, verschiebHilf);
      vgl := vgl + vglHilf;
      verschieb := verschieb + verschiebHilf;
      if jedenAnzeigen then
        case vergleichsArt of
          vglVerschieb: paintbox1.canvas.pixels[xU(aktLaenge),
              yU(verschiebhilf)] := farb;
          vglVergleich: paintbox1.canvas.pixels[xU(aktLaenge), yU(vglhilf)] := farb;
          vglKosten: paintbox1.canvas.pixels[xU(aktLaenge), lineX -
              round((verschiebhilf * fvKosten + vglHilf) / yFaktor)] := farb;
        end;
    end;
    messung[sortArt, aktLaenge] := verschieb;
    vglFeld[sortArt, aktLaenge] := vgl;
    if vergleichsArt = vglKosten then kFeld[sortArt, aktLaenge] :=
        Round(verschieb * fvKosten) + vgl;
    case vergleichsArt of
      vglVerschieb: paintbox1.canvas.pixels[xU(aktLaenge), lineX -
          round(verschieb / y0)] := farb;
      vglVergleich: paintbox1.canvas.pixels[xU(aktLaenge), lineX -
          round(vgl / y0)] := farb;
      vglKosten: paintbox1.canvas.pixels[xU(aktLaenge), lineX - round(
          (verschieb * fvKosten + vgl) / y0)] := farb;
    end;
    FortschrittsAnzeigeAndCheck(aktLaenge);
    if abbruch then
      break;

  end;
  vglFeld[sortArt, 0] := durchlauf;
  messung[sortArt, 0] := durchlauf;
  if vergleichsArt = vglKosten then kFeld[sortArt, 0] := durchlauf;
  BitBtnGraphicNeuzeichnen.Enabled := True;
  FortschrittsAnzeigeLoeschen(maxLaenge);
end;

procedure TFormMessung.TesteZeiten(sortArt: TSortTyp);
var
  k, aktLaenge, maxLaenge, durchlauf, recoverTime: integer;
  t, tHilf: longword;
  jedenAnzeigen: boolean;
begin
  Vorbereiten(maxlaenge, durchlauf, yfaktor, jedenAnzeigen);
  for k := 0 to maxFeldLaenge do
    messung[sortArt, k] := 0;
  recoverTime := SpinEditRecoverTime.Value;
  for aktLaenge := minFeldLaenge to maxLaenge do
  begin
    t := 0;
    for k := 1 to durchlauf do
    begin
      FuelleZahlenFeld(aktLaenge);
      TestSortZeit(feld, aktlaenge, sortArt, myTimer, thilf);
      t := t + tHilf;
      if jedenAnzeigen then
        paintbox1.canvas.pixels[xU(aktLaenge), lineX - round(thilf / yFaktor)] :=
          SortFarb[sortArt];
    end;
    messung[sortArt, aktLaenge] := t;
    paintbox1.canvas.pixels[xU(aktLaenge), lineX - round(t / (durchlauf * yFaktor))]
    := SortFarb[sortArt];
    FortschrittsAnzeigeAndCheck(aktLaenge);
    if abbruch then
      break;
    sleep(recoverTime);   //Processor in normalen Takt (?)
  end;
  messung[sortArt, 0] := durchlauf;
  FortschrittsAnzeigeLoeschen(maxLaenge);
end;

procedure TFormMessung.TrackBar1Change(Sender: TObject);
var
  neu: real;
  laenge: integer;
begin
  laenge := paintbox1.Width - offset;
  if spineditLaenge.Value < laenge then
    laenge := spineditLaenge.Value;
  neu := FaktorBestimmen(laenge);
  if neu <> yFaktor then
  begin
    yFaktor := neu;
    paintbox1.Repaint;
  end;
end;

procedure TFormMessung.Vorbereiten(out laenge, durchlauf: integer;
  var faktor: real; out jedenAnzeigen: boolean);
var
  hilf: real;
begin
  abbruch := False;
  showSteps := True;
  pause := 0;
  ButtonsAktivieren(False);
  try
    durchLauf := SpinEditDurchLauf.Value;
  finally
    if (listArt = sortiert) or (listArt = umgekehrtSortiert) then
      durchLauf := 1;
    try
      laenge := SpinEditLaenge.Value;
    finally
      hilf := FaktorBestimmen(laenge);
      if hilf <> faktor then
      begin
        faktor := hilf;
        PaintBox1Paint(nil);
      end;
      jedenAnzeigen := CheckBoxAlleZeigen.Checked and (durchlauf > 1);
    end;
  end;
  ZeichneMarkierung(laenge, clRed);
end;

procedure TFormMessung.ZeichneFunktion(aFkt: TFkt; yFak: double);
var
  k, xMax: integer;
  y, yMax: double;
begin
  if (aFkt = nil) or (aFkt.fktx = nil) or (not aFkt.Visible) then
    exit;
  xMax := paintbox1.Width - offset;
  k := SpinEditLaenge.Value;
  if k < xMax then xMax := k;
  yMax := lineX * yFak;

  with paintbox1.canvas do
  begin
    pen.Color := aFkt.color;
    k := minFeldLaenge + 1;
    y := aFkt.FktWert(k);
    moveTo(xU(k), lineX - round(y / yFak));
    while (k <= xMax) do
    begin
      k := k + 1;
      y := aFkt.FktWert(k);
      if y < yMax then
        lineTo(xU(k), lineX - round(y / yFak))
      else
        break;
    end;
  end;
end;

procedure TFormMessung.ZeichneMarkierung(x: integer; farbe: TColor);
begin
  x := xU(x) - 1;
  with Paintbox1.Canvas do
  begin
    pen.Width := 2;
    pen.color := farbe;
    MoveTo(x, lineX + offset - 2);
    LineTo(x, lineX + (offset div 2));
    pen.Color := clBlack;
    pen.Width := 1;
  end;
end;

end.
