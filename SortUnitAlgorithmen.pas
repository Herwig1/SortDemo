unit SortUnitAlgorithmen;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Ermittlung der Sortierzeiten bzw. der Vergleiche und Verschiebungen
            in Abhängigkeit der Listenlänge
   Autor:   H. Niemeyer  (c) 2010 - 2020, 2023, 2024
   Version: 1.8

   letzte Änderung: 12.01.2024 *)

interface

uses
  LCLIntf, LCLType,
  SortUnitDefinitionen, epiktimer;

const
  maxFeldLaenge = 1000;

{
type  TElementTyp   = integer;
      TTestFeld     = Array[0..maxFeldLaenge] of TElementTyp;
      TschrittWeite = ARRAY[1..15] OF INTEGER;
 }

procedure TestSortZeit(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp;
  aTimer: TEpikTimer; out zeit: longword);
procedure TestSortVergl(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp;
  out vgl, verschieb: longword);

implementation


procedure TestSortZeit(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp;
  aTimer: TEpikTimer; out zeit: longword);

  procedure Einsortieren;
  var
    l, anfang: integer;
  begin
    for anfang := 2 to laenge do
    begin
      feld[0] := feld[anfang];
      l := anfang - 1;
      while feld[0] < feld[l] do
      begin
        feld[l + 1] := feld[l];
        Dec(l);
      end;
      feld[l + 1] := feld[0];
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
        feld[j + 1] := feld[j];
      feld[l] := hilf;
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
      feld[l] := feld[anfang];
      feld[anfang] := hilf;
    end;
  end;   { MinSort }

  procedure Bubble1Sort;
  var
    k, ende: integer;
    hilf: TElementTyp;
  begin
    for ende := 2 to laenge do
      for k := laenge downto ende do
        if feld[k - 1] > feld[k] then
        begin
          hilf := feld[k];
          feld[k] := feld[k - 1];
          feld[k - 1] := hilf;
        end;
  end;   { Bubble1Sort }

  procedure Bubble2Sort;
  var
    k, ende: integer;
    hilf: TElementTyp;
    fertig: boolean;
  begin
    ende := laenge;
    repeat
      ende := ende - 1;
      fertig := True;
      for k := 1 to ende do
        if feld[k] > feld[k + 1] then
        begin
          hilf := feld[k];
          feld[k] := feld[k + 1];
          feld[k + 1] := hilf;
          fertig := False;
        end
    until fertig;
  end;  { Bubble2Sort }

  procedure CombSort;    { s. BYTE 4/91   S. 315 ff }
  const
    schrumpfFaktor = 1.3;
  var
    k, j, weite: integer;
    hilf: TElementTyp;
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
          hilf := feld[k];
          feld[k] := feld[j];
          feld[j] := hilf;
          fertig := False;
        end;
      end
    until fertig and (weite = 1);
  end;  { CombSort }

  procedure ShakeSort;
  var
    j, k, l, r: integer;
    hilf: TElementTyp;
  begin
    l := 2;
    r := laenge;
    k := laenge;
    repeat
      for j := r downto l do
        if feld[j - 1] > feld[j] then
        begin
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
        end;
      l := k + 1;
      for j := l to r do
        if feld[j - 1] > feld[j] then
        begin
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
        end;
      r := k - 1
    until l > r;
  end;   { ShakeSort }

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
          feld[j + k] := feld[j];
          j := j - k;
        end;
        feld[j + k] := hilf;
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
          if feld[j] < feld[j + 1] then
            Inc(j);
        if hilf < feld[j] then
        begin
          feld[i] := feld[j];
          i := j;
          j := 2 * i;
          weiter := (j <= r);
        end
        else
          weiter := False;
      end;
      feld[i] := hilf;
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
      hilf := feld[l];
      feld[l] := feld[r];
      feld[r] := hilf;
      Dec(r);
      Sift;
    end;
  end;  {Heapsort}

  procedure Quicksort;

    procedure Quick1(l, r: integer);
    var
      i, j: integer;
      hilf, help: TElementTyp;
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
            help := feld[i];
            feld[i] := feld[j];
            feld[j] := help;
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
  var
    B: TTestFeld;

    procedure Merge(p, q, r: integer);
    var
      i, j, k, n1: integer;
    begin { Merge }
      n1 := q - p + 1;
      for k := 1 to n1 do
        B[k] := feld[p + k - 1];
      i := 1;
      j := q + 1;
      k := p;
      while ((i <= n1) and (j <= r)) do
      begin
        if (B[i] < feld[j]) then
        begin
          feld[k] := B[i];
          i := i + 1;
        end
        else
        begin
          feld[k] := feld[j];
          j := j + 1;
        end;
        k := k + 1;
      end;
      while (i <= n1) do
      begin
        feld[k] := B[i];
        k := k + 1;
        i := i + 1;
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
        MergeSort(q + 1, r);
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
    c1, c2: double;

    procedure KlassenVorbereiten;
    var
      nmin, nmax, i, k: integer;
      temp: TElementTyp;
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

      nmin := 1;
      nmax := 1;
      for i := 2 to laenge do
      begin
        if feld[i] < feld[nmin] then
          nmin := i;
        if feld[i] > feld[nmax] then
          nmax := i;
      end;
      c1 := (maxKlassenNr - 1) / (feld[nmax] - feld[nmin]);
      c2 := c1 * feld[nmin];
      FillChar(stellenNr, SizeOf(stellenNr), 0);
      for i := 1 to laenge do
      begin
        k := 1 + trunc((c1 * feld[i] - c2));
        stellenNr[k] := stellenNr[k] + 1;
      end;
      for k := 2 to maxKlassenNr do
        stellenNr[k] := stellenNr[k] + stellenNr[k - 1];
      temp := feld[nmax];
      feld[nmax] := feld[1];
      feld[1] := temp;
    end;

    procedure Perm;
    var
      nmove, j, k: integer;
      FLASH, temp: TElementTyp;
    begin
      nmove := 0;
      j := 1;
      k := maxKlassenNr;
      while nmove < (laenge - 1) do
      begin
        while j > stellenNr[k] do
        begin
          j := j + 1;
          k := 1 + Trunc((c1 * feld[j] - c2));
        end;
        FLASH := feld[j];
        while j <> (stellenNr[k] + 1) do
        begin
          k := 1 + Trunc((c1 * FLASH - c2));
          temp := feld[stellenNr[k]];
          feld[stellenNr[k]] := FLASH;
          stellenNr[k] := stellenNr[k] - 1;
          FLASH := temp;
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
            feld[j] := feld[j + 1];
            j := j + 1;
          end;
          feld[j] := temp;
        end;
      end;
    end;

  begin
    KlassenVorbereiten;
    Perm;
    Insert;
  end;

var
  z: extended;
begin { TestSortZeit }
  aTimer.Clear;
  aTimer.Start;
  case sortTyp of
    sMin: MinSort;
    sEin: Einsortieren;
    sBubble1: Bubble1Sort;
    sBubble2: Bubble2Sort;
    sBinEin: BinaeresEinsortieren;
    sComb: CombSort;
    sShake: ShakeSort;
    sShell: ShellSort(shellSortSteps, 15);
    sHeap: HeapSort;
    sQuick: QuickSort;
    sMerge: StartMergeSort;
    sFlash: StartFlashSort;
  end;
  aTimer.Stop;
  z := aTimer.Elapsed;
  zeit := trunc(z * 1000000);
end;



procedure TestSortVergl(feld: TTestFeld; laenge: integer; sortTyp: TSortTyp;
  out vgl, verschieb: longword);

  procedure Einsortieren;
  var
    k, anfang: integer;
  begin
    verschieb := 2 * (laenge - 1);
    for anfang := 2 to laenge do
    begin
      feld[0] := feld[anfang];
      k := anfang - 1;
      while feld[0] < feld[k] do
      begin
        feld[k + 1] := feld[k];
        Dec(k);
        Inc(verschieb);
      end;
      vgl := vgl + anfang - k;
      feld[k + 1] := feld[0];
    end;
  end;  { Einsortieren }

  procedure BinaeresEinsortieren;
  var
    i, m, j, k, l: integer;
    hilf: TElementTyp;
  begin
    verschieb := 2 * (laenge - 1);
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
        Inc(vgl);
      end;
      for j := i - 1 downto l do
        feld[j + 1] := feld[j];
      feld[l] := hilf;
      if i > l then
      begin
        verschieb := verschieb + i - l;
      end;
    end;
  end;   { BinaeresEinsortieren }

  procedure MinSort;
  var
    l, k, anfang: integer;
    hilf: TElementTyp;
  begin
    verschieb := 3 * (laenge - 1);
    vgl := laenge * (laenge - 1) div 2;
    for anfang := 1 to laenge - 1 do
    begin
      l := anfang;
      hilf := feld[anfang];
      for k := anfang + 1 to laenge do
        if feld[k] < hilf then
        begin
          l := k;
          hilf := feld[k];
          Inc(verschieb);
        end;
      feld[l] := feld[anfang];
      feld[anfang] := hilf;
    end;
  end;   { MinSort }

  procedure Bubble1Sort;
  var
    k, ende: integer;
    hilf: TElementTyp;
  begin
    vgl := laenge * (laenge - 1) div 2;
    for ende := 2 to laenge do
      for k := laenge downto ende do
        if feld[k - 1] > feld[k] then
        begin
          hilf := feld[k];
          feld[k] := feld[k - 1];
          feld[k - 1] := hilf;
          verschieb := verschieb + 3;
        end;
  end;   { Bubble1Sort }

  procedure Bubble2Sort;
  var
    k, ende: integer;
    hilf: TElementTyp;
    fertig: boolean;
  begin
    ende := laenge;
    repeat
      ende := ende - 1;
      fertig := True;
      vgl := vgl + ende;
      for k := 1 to ende do
        if feld[k] > feld[k + 1] then
        begin
          hilf := feld[k];
          feld[k] := feld[k + 1];
          feld[k + 1] := hilf;
          fertig := False;
          verschieb := verschieb + 3;
        end
    until fertig;
  end;  { Bubble2Sort }

  procedure CombSort;    { s. BYTE 4/91   S. 315 ff }
  const
    schrumpfFaktor = 1.3;
  var
    k, j, weite: integer;
    hilf: TElementTyp;
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
      vgl := vgl + laenge - weite;
      for k := 1 to laenge - weite do
      begin
        Inc(j);
        if feld[k] > feld[j] then
        begin
          hilf := feld[k];
          feld[k] := feld[j];
          feld[j] := hilf;
          fertig := False;
          verschieb := verschieb + 3;
        end;
      end
    until fertig and (weite = 1);
  end;  { CombSort }

  procedure ShakeSort;
  var
    j, k, l, r: integer;
    hilf: TElementTyp;
  begin
    l := 2;
    r := laenge;
    k := laenge;
    repeat
      for j := r downto l do
        if feld[j - 1] > feld[j] then
        begin
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
          verschieb := verschieb + 3;
        end;
      vgl := vgl + r - l + 1;

      l := k + 1;
      for j := l to r do
        if feld[j - 1] > feld[j] then
        begin
          hilf := feld[j];
          feld[j] := feld[j - 1];
          feld[j - 1] := hilf;
          k := j;
          verschieb := verschieb + 3;
        end;
      vgl := vgl + r - l + 1;
      r := k - 1
    until l > r;
  end;   { ShakeSort }

  procedure ShellSort(step: TShellSteps; maxStep: integer);
  var
    i, j, k, v: integer;
    hilf: TElementTyp;
    m: 1..15;
  begin
    v := 0;
    for m := 1 to maxStep do
    begin
      k := step[m];
      if k < laenge then
        vgl := vgl + laenge - k;
      for i := k + 1 to laenge do
      begin
        hilf := feld[i];
        j := i - k;
        while (j > 0) and (hilf < feld[j]) do
        begin
          feld[j + k] := feld[j];
          j := j - k;
          v := v + 1;
        end;
        feld[j + k] := hilf;
      end;
    end;
    verschieb := 2 * vgl + v;
    vgl := vgl + v;
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
      verschieb := verschieb + 2;
      while weiter do
      begin
        if j < r then
        begin
          Inc(vgl);
          if feld[j] < feld[j + 1] then
            Inc(j);
        end;
        Inc(vgl);
        if hilf < feld[j] then
        begin
          feld[i] := feld[j];
          i := j;
          j := 2 * i;
          weiter := (j <= r);
          Inc(verschieb);
        end
        else
          weiter := False;
      end;
      feld[i] := hilf;
    end;

  begin  {Heapsort}
    l := (laenge div 2) + 1;
    r := laenge;
    verschieb := 3 * (r - 1);
    while l > 1 do
    begin
      Dec(l);
      Sift;
    end;
    while r > 1 do
    begin
      hilf := feld[l];
      feld[l] := feld[r];
      feld[r] := hilf;
      Dec(r);
      Sift;
    end;
  end;  {Heapsort}

  procedure Quicksort;

    procedure Quick1(l, r: integer);
    var
      i, j: integer;
      hilf, help: TElementTyp;
    begin
      i := l;
      j := r;
      hilf := feld[(l + r) div 2];
      Inc(verschieb);
      repeat
        while feld[i] < hilf do
          Inc(i);
        while hilf < feld[j] do
          Dec(j);
        if i <= j then
        begin
          if i < j then
          begin
            help := feld[i];
            feld[i] := feld[j];
            feld[j] := help;
            verschieb := verschieb + 3;
          end;
          Inc(i);
          Dec(j);
        end
      until i > j;
      vgl := vgl + i - l + r - j + 2;
      if l < j then
        Quick1(l, j);
      if i < r then
        Quick1(i, r);
    end;

  begin  {Quicksort}
    Quick1(1, laenge);
  end;  {Quicksort}


  procedure StartMergeSort;
  var
    B: TTestFeld;

    procedure Merge(p, q, r: integer);
    var
      i, j, k, n1: integer;
    begin { Merge }
      n1 := q - p + 1;
      for k := 1 to n1 do
        B[k] := feld[p + k - 1];
      verschieb := verschieb + n1;
      i := 1;
      j := q + 1;
      k := p;
      while ((i <= n1) and (j <= r)) do
      begin
        vgl := vgl + 1;
        verschieb := verschieb + 1;
        if (B[i] < feld[j]) then
        begin
          feld[k] := B[i];
          i := i + 1;
        end
        else
        begin
          feld[k] := feld[j];
          j := j + 1;
        end;
        k := k + 1;
      end;
      while (i <= n1) do
      begin
        verschieb := verschieb + 1;
        feld[k] := B[i];
        k := k + 1;
        i := i + 1;
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
        MergeSort(q + 1, r);
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
    c1, c2: double;

    procedure KlassenVorbereiten;
    var
      nmin, nmax, i, k: integer;
      temp: TElementTyp;
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

      nmin := 1;
      nmax := 1;
      for i := 2 to laenge do
      begin
        if feld[i] < feld[nmin] then
          nmin := i;
        if feld[i] > feld[nmax] then
          nmax := i;
        vgl := vgl + 2;
      end;
      c1 := (maxKlassenNr - 1) / (feld[nmax] - feld[nmin]);
      c2 := c1 * feld[nmin];
      FillChar(stellenNr, SizeOf(stellenNr), 0);
      for i := 1 to laenge do
      begin
        k := 1 + trunc((c1 * feld[i] - c2));
        stellenNr[k] := stellenNr[k] + 1;
      end;
      for k := 2 to maxKlassenNr do
        stellenNr[k] := stellenNr[k] + stellenNr[k - 1];
      temp := feld[nmax];
      feld[nmax] := feld[1];
      feld[1] := temp;
      verschieb := verschieb + 3;
    end;

    procedure Perm;
    var
      nmove, j, k: integer;
      FLASH, temp: TElementTyp;
    begin
      nmove := 0;
      j := 1;
      k := maxKlassenNr;
      while nmove < (laenge - 1) do
      begin
        while j > stellenNr[k] do
        begin
          j := j + 1;
          k := 1 + Trunc((c1 * feld[j] - c2));
        end;
        FLASH := feld[j];
        verschieb := verschieb + 1;
        while j <> (stellenNr[k] + 1) do
        begin
          k := 1 + Trunc((c1 * FLASH - c2));
          temp := feld[stellenNr[k]];
          feld[stellenNr[k]] := FLASH;
          stellenNr[k] := stellenNr[k] - 1;
          FLASH := temp;
          verschieb := verschieb + 3;
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
        vgl := vgl + 1;
        if feld[i + 1] < feld[i] then
        begin
          temp := feld[i];
          verschieb := verschieb + 1;
          j := i;
          vgl := vgl + 1;
          while feld[j + 1] < temp do
          begin
            feld[j] := feld[j + 1];
            j := j + 1;
            verschieb := verschieb + 1;
            vgl := vgl + 1;
          end;
          feld[j] := temp;
          verschieb := verschieb + 1;
        end;
      end;
    end;

  begin
    KlassenVorbereiten;
    Perm;
    Insert;
  end;

begin { Vergleiche und/oder Verschiebungen }
  vgl := 0;
  verschieb := 0;
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
    sFlash: StartFlashSort;
  end;
end;

end.
