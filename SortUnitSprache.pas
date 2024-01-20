unit SortUnitSprache;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
  Modul:   Anpassung an verschiedene Sprachen
  Autor:   H. Niemeyer  (c) 2010 - 2020
  Version: 1.6

  letzte Änderung: 15.10.2020 *)

interface

uses Classes, SysUtils, Forms;

const

  defaultSprachFileName = 'SortDemo.txt';

  s_Sprache = 0;
  s_Version = 1;
  s_OK = 2;
  s_Ja = 3;
  s_Nein = 4;
  s_Abbrechen = 5;
  s_AniStarten = 6;
  s_AniAnhalten = 7;
  s_AniFortsetzen = 8;
  s_AniAbbrechen = 9;
  s_Schliessen = 10;
  s_Sortierverfahren = 11;
  s_AuswahListenart = 12;
  s_Neuzeichnen = 13;
  s_DiagrammLoeschen = 14;
  s_Verzoegerung = 15;
  // SortUnitDefinitionen
  s_inaktiv = 16;
  s_sortiert = 17;
  s_vergleichen = 18;
  s_verschieben = 19;
  s_merken = 20;
  s_einfuegen = 21;
  // SortUnitMain
  s_SortierDemo = 22;
  s_SortierungLinListen = 23;
  s_DemoSortierverfahren = 24;
  s_Zeitmessungen = 25;
  s_AnzahlVergleiche = 26;
  s_AnzahlVerschiebungen = 27;
  s_Kosten = 28;
  s_SortierungStab = 29;
  s_SortierungBand = 30;
  s_DemoSortieralgorithmen = 31;
  s_VergleichZeiten = 32;
  s_BestimmeVerschiebungen = 33;
  s_BestimmeVergleiche = 34;
  s_BestimmeKosten = 35;
  s_BeispieBandsortierung = 36;
  s_ZahlenfeldErzeugen = 37;
  s_ListenArt = 38;
  s_normal = 39;
  s_ohneDoppel = 40;
  s_umgekehrt = 41;
  s_Feldgroesse = 42;
  s_Farbauswahl = 43;
  s_AniSchliessen = 44;
  s_ProgrammBeenden = 45;
  // SortUnitMessungen
  s_nachListenLaenge = 46;
  s_KurveZeichnen = 47;
  s_Ausgleichskurve = 48;
  s_TheorieKurve = 49;
  s_KeineKurve = 50;
  s_vergleicheVerfahren = 51;
  s_AnzahlDurchlauf = 52;
  s_jedenAnzeigen = 53;
  s_maxListenlaenge = 54;
  s_Kostenverteilung = 55;
  // SortUnitMessungen
  s_CaptionSortierzeiten = 56;
  s_PauseCPU = 57;
  // SortUnitBand
  s_DemoBandSortierung = 58;
  s_SplitteZahlenfeld = 59;
  s_maxBandlang = 60;
  s_HeapGroesse = 61;
  s_Ein = 62;
  s_Aus = 63;
  s_Lauf = 64;
  // SortUnitLineDemo
  s_DemoGraphDarstellung = 65;
  s_neueListe = 66;
  // SortUnitListArtDlg
  s_normaleZufallsliste = 67;
  s_ZufallslisteOhneDopplungen = 68;
  s_sortierteListe = 69;
  s_umgekehrteListe = 70;
  // SortZahlenfeldDlg
  s_ZahlenfeldEingeben = 71;
  s_Laden = 72;
  s_Speichern = 73;
  // SortUnitAuswahlDlg
  s_SortierverfahrenWahl = 74;
  s_alleMarkieren = 75;
  s_MarkierungLoeschen = 76;
  // SortUnitFarbwahl_Demo
  s_FarbenBestimmen = 77;
  s_Uebernehmen = 78;
  // SortUnitKostenDlg
  s_KostenfaktorFestlegen = 79;
  s_Vergleiche = 80;
  s_Verschiebungen = 81;
  //FunktionEditorDlg
  s_FehlerAnzeigen = 82;
  s_Funktionseditor = 83;
  //SimpFunktion
  s_KlammerAuf = 84;
  s_KlammerZu = 85;
  s_Operator = 86;
  s_KlammerZahlBezeichner = 87;
  s_NichtErlaubt = 88;
  s_ZahlZuGross = 89;
  s_keineFormel = 90;
  s_FormelNichtInterpretiert = 91;


  lastInfo = 91;

  sprachTab: array[0..lastInfo] of shortstring =
    ('deutsch',
    'Version',
    'OK',
    'Ja',
    'Nein',
    'Abbrechen',
    'Animation starten',
    'anhalten',
    'fortsetzen',
    'Animation abbrechen',
    'Schliessen',
    'Sortierverfahren',
    'Auswahl der Listenart',
    'neu zeichnen',
    'Diagramm löschen',
    'Verzögerung',
    // SortUnitDefinitionen
    'inaktiv',
    'sortiert',
    'vergleichen',
    'verschieben',
    'merken',
    'einfügen',
    // SortUnitMain
    'Sortier-Demo',
    'Sortierung auf linearen Listen',
    'Demo von Sortierverfahren',
    'Zeitmessungen',
    'Anzahl der Vergleiche',
    'Anzahl der Verschiebungen',
    ' "Kosten" der Vergleiche und Verschiebungen',
    'Sortierung von Stäben',
    'Sortierung auf Bändern',
    'Demo zu einigen Sortieralgorithmen',
    'Vergleich der Ausführungszeiten',
    'Bestimme Anzahl der Verschiebungen',
    'Bestimme Anzahl der Vergleichen',
    'Bestimme die "Kosten"',
    'Beispiel einer Bandsortierung',
    'Zahlenfeld erzeugen',
    'ListenArt',
    'normal',
    'ohne Doppel',
    'umgekehrt',
    'Feldgröße',
    'Farbauswahl',
    'Animationen schliessen',
    'Programm beenden',
    // SortUnitVerschiebungen
    'in Abhängigkeit von der Listenlänge (Rechtsklick: Ausgleichsfunktion)',
    'Kurve zeichnen?',
    'Ausgleichskurve für',
    'theoretische Funktion eingeben',
    'Keine Kurve gefunden',
    'vergleiche ausgewählte Verfahren',
    'Anzahl der Durchläufe',
    'jeden anzeigen',
    'max. Listenlänge',
    'Kostenverteilung',
    // SortUnitZeiten
    'Sortierzeiten (mikroSekunden) in Abhängigkeit von der Listenlänge (Rechtsklick: Ausgleichsfunktion)',
    'Pause nach Zeitmessung (damit sich der Prozessor erholen kann)',
    // SortUnitBand
    'Demo zur Sortierung von Bändern',
    'Splitte Zahlenfeld in 2 Bänder',
    'max. Bandlänge:',
    'Heapgröße:',
    'Ein',
    'Aus',
    'Lauf',
    // SortUnitLineDemo
    'Demo der Sortierverfahren mit graphischer Darstellung',
    'neue Liste erstellen',
    // SortUnitListArtDlg
    'normale Zufallsliste',
    'Zufallsliste ohne Dopplungen',
    'sortierte Liste',
    'umgekehrt sortierte Liste',
    // SortZahlenfeldDlg
    'Zahlenfeld eingeben',
    'Liste laden',
    'Liste speichern',
    // SortUnitAuswahlDlg
    'Sortierverfahren auswählen',
    'alle markieren',
    'Markierung löschen',
    // SortUnitFarbwahl_Demo
    'Farben bestimmen',
    'übernehmen',
    // SortUnitKostenDlg
    'Kostenfaktor festlegen',
    'Vergleiche',
    'Verschiebungen',
    //FunktionEditorDlg
    'Fehler anzeigen',
    'Funktionseditor',
    //SimpFunktion
    '( erwartet',
    ') erwartet',
    'Operator erwartet',
    '( oder Zahl oder n erwartet',
    'unzulässige Eingabe',
    'Zahl zu groß',
    'keine Formel angegeben',
    'Formel nicht interpretiert'
    );

procedure LiesSprachdatei(FileName: string);

implementation


procedure LiesSprachdatei(FileName: string);
var
  k: integer;
  s: string;
  f: textFile;
begin
  if FileName = '' then
    FileName := ExtractFilePath(application.ExeName) + defaultSprachFileName;
  if not FileExists(FileName) then
    exit;
  k := 0;
  try
    AssignFile(f, FileName);
    Reset(f);
    while not EOF(f) and (k <= lastInfo) do
    begin
      ReadLn(f, s);
      if pos('//', s) = 0 then
      begin
        sprachTab[k] := s;
        k := k + 1;
      end;
    end;
    Close(f);
    while (k <= lastInfo) do
    begin
      sprachTab[k] := '.. ?';
      k := k + 1;
    end;

  finally

  end;
end;


begin
  LiesSprachdatei('')
end.
