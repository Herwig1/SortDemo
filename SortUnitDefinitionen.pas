unit SortUnitDefinitionen;

 (* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Grundlegende Definitionen und Festlegungen des Projekts
   Autor:   H. Niemeyer  (c) 2010 - 2021, 2023, 2024
   Version: 1.8

   letzte Änderung: 20.01.2024 *)

interface

uses
  LCLIntf,
  Types, SysUtils, Graphics, Forms;

const
  maxFeldLaenge = 1000;
  minFeldLaenge = 2;
  maxDemoAnzahl = 20;    { max. Feldgröße für Demo }
  maxZahl = $7FFF;       { max. Zahl für ganze Zufallszahlen }
  maxDemoZahl = 80;      { max. Zahl für Demo; muß 2-Stellig sein }
  offSet = 20;           { Abstand der Achsen von den Rändern }

  vglZeiten = 0;
  vglVerschieb = 1;
  vglVergleich = 2;
  vglKosten = 3;

type
  TElementTyp = integer;
  TFeld = array[1..maxDemoAnzahl] of TElementTyp;
  TTestFeld = array[0..maxFeldLaenge] of TElementTyp;
  TSpeicher = array[0..maxFeldLaenge] of longword;
  TschrittWeite = array[1..15] of integer;
  TSortTyp = (sMin, sEin, sBinEin, sBubble1, sBubble2, sShake,
    sComb, sShell, sHeap, sQuick, sMerge, sFlash);
  TFarbArray = array[TSortTyp] of TColor;
  TSortierverfahren = set of TSortTyp;
  TListenTyp = (zufall, ohneDoppel, sortiert, umgekehrtSortiert);
  TShellSteps = array[1..15] of integer;
  TDemoColor = (inaktiv, istSortiert, vergleichen, verschiebung,
    PosMarker, MinMarker, marker_QSort, vergleich2_QSort);
  TDemoFarben = array[TDemoColor] of TColor;

const
  DemoFarb: TDemoFarben = (clSilver, clRed, clGreen, clRed, clRed,
    clfuchsia, clfuchsia, clBlue);

const
  SortFarb: TFarbArray = (clBlue, clGreen, clTeal, clRed, clFuchsia, clDkGray,
    clMaroon, clPurple, clNavy, clLime, clSkyBlue, clAqua);
  SortName: array[TSortTyp] of string[15] =
    ('MinSort', 'EinSort', 'binEinSort', 'Bubble1Sort',
    'Bubble2Sort', 'ShakeSort',
    'Comb11Sort', 'ShellSort', 'HeapSort', 'QuickSort',
    'MergeSort', 'FlashSort');

  sLastSort = sFlash;

var
  shellSortSteps: TShellSteps;

  pause: integer;
  abbruch, showSteps: boolean;


procedure InitZahlenFeld(listArt: TListenTyp; maxFeld: integer; var feld: TFeld);

procedure SetShellSteps(max, faktor, step: integer);

procedure WarteMS(ms: cardinal);
procedure CheckPause(ms: cardinal);

function GetTextSizeInPixels(Text: string; Font: TFont): TPoint;
function GetFontSizeFromPixels(Text: string; w, h: integer; aFont: TFont;
  MinSize: integer = 1): integer;

implementation

procedure CheckPause(ms: cardinal);
begin
  if ms = 0 then Application.ProcessMessages
  else
    WarteMS(ms);
  if not showSteps then
    repeat
      Application.ProcessMessages;
    until showSteps or abbruch;
end;

procedure InitZahlenFeld(listArt: TListenTyp; maxFeld: integer; var feld: TFeld);
var
  k, p: integer;
  element: integer;
begin
  case listArt of
    zufall:
    begin
      randomize;
      for k := 1 to maxFeld do
        feld[k] := random(maxDemoZahl) + 1;
    end;
    ohneDoppel:
    begin
      randomize;
      for k := 1 to maxFeld do
        repeat
          element := random(maxDemoZahl) + 1;
          p := 1;
          while (p < k) and (feld[p] <> element) do
            p := p + 1;
          if p = k then
            feld[k] := element;
        until p = k;
    end;
    sortiert: for k := 1 to maxFeld do
        feld[k] := 9 + k;
    umgekehrtSortiert: for k := 1 to maxFeld do
        feld[k] := maxFeld + 10 - k;
  end;
end;

procedure SetShellSteps(max, faktor, step: integer);
var
  k: integer;
begin
  shellSortSteps[max] := 1;
  for k := max - 1 downto 1 do
    shellSortSteps[k] := faktor * shellSortSteps[k + 1] + step;
end;

procedure WarteMS(ms: cardinal);
var
  Count: cardinal;
begin
  if abbruch then
    exit;
  Count := GetTickCount64;
  repeat
    Application.ProcessMessages;
  until (GetTickCount64 - Count > ms) or abbruch;
end;

function GetTextSizeInPixels(Text: string; Font: TFont): TPoint;
var
  TmpBmp: TBitmap;
begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Canvas.Font := Font;
    Result.x := TmpBmp.Canvas.TextWidth(Text);
    Result.y := TmpBmp.Canvas.TextHeight(Text);
  finally
    FreeAndNil(TmpBmp);
  end;
end;

function GetFontSizeFromPixels(Text: string; w, h: integer; aFont: TFont;
  MinSize: integer = 1): integer;
var
  Font: TFont;
  tmpSize: TPoint;
begin
  Font := TFont.Create;
  try
    Font.Assign(aFont);
    // check if MinSize is ok
    Font.Size := MinSize;
    tmpSize := GetTextSizeInPixels(Text, Font);
    if (tmpSize.x > w) or (tmpSize.y > h) then
      Result := -1
    // width at MinSize is smaller than qPixels
    else
    begin
      repeat
        Inc(MinSize);
        Font.Size := MinSize;
        tmpSize := GetTextSizeInPixels(Text, Font);
      until (tmpSize.x > w) or (tmpSize.y > h);
      Result := MinSize - 1;
    end;
  finally
    FreeAndNil(Font);
  end;
end;

end.
