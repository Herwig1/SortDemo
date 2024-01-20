unit SortUnitAusgleich;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Berechnung einer Ausgleichsfunktion zu den ermittelten Sortierzeiten
            in Abhängigkeit zur Listenlänge
   Autor:   H. Niemeyer  (c) 2010 - 2021, 2023 , 2024
   Version: 1.8

   letzte Änderung: 12.01.2024 *)

interface

uses SysUtils, ExtCtrls, SortUnitDefinitionen;

const
  lin_regress = 1;
  log_regress = 2;
  pot_regress = 4;
  nLog_regress = 8;
  poly_regress1 = 16;
  poly_regress2 = 32;
  exp_regress = 64;
  sqr_regress = 128;
  best_regress = lin_regress + log_regress + pot_regress + nLog_regress +
    poly_regress1 + poly_regress2 + exp_regress + sqr_regress;
  bestSort_regress = lin_regress + log_regress + pot_regress +
    nLog_regress + poly_regress1 + poly_regress2 + exp_regress;

  fehlerZahl = -1.0E32;
  _Ln_max_Real = 88;

  regressHint = 'm*n+c / c + a*Ln(n) / a*n^b / a*n*Ln(n)+c / a*n^2+b*n+c / a*Exp(b*n)';


type
  TRegrData = array[0..3] of double;  { 0 - güte, 1 - m, 2 - c, 3 - frei }

procedure AusgleichFunktion(aktf: TSpeicher; var regTyp: integer;
  out m, c, d: double; out fktStr: string);
procedure ZeichneAusgleich(pb: TPaintbox; regTyp: integer; yFak, m, c, d: double);

implementation

const
  do_lin_regress = 0;
{  do_log_regress = 1;
  do_pot_regress = 2;
  do_nLog_regress = 3;
  do_poly_regress1 = 4;
  do_poly_regress2 = 5;
  do_exp_regress = 6;
  do_sqr_regress = 7; }
  Last_RegressTyp = 7;

var
  f: TSpeicher;
  laenge, start, endX: integer;
  keinErr: boolean;

function GetLaenge(var f: TSpeicher): integer;
var
  k: integer;
begin
  k := minFeldLaenge;
  while (k < maxFeldLaenge) and ((f[k] = 0) or (f[k + 1] = 0)) do
    k := k + 1;
  start := k;
  while (k <= maxFeldLaenge) and (f[k] <> 0) do
    k := k + 1;
  endX := k - 1;
  if k <= start + 2 then
    Result := 0
  else
    Result := k - start;
end;


procedure MakeRegressDataPoly1(var rDat: TRegrData);    //  m*x^2 + c*x
var
  i, anz, lauf: integer;
  rd: TRegrData;
  hilfX, hilfY, realAnz, sumX, sumX2, sumY, sumY2, sumXY, t: extended;
begin
  rd := Default(TRegrData);
  rd[0] := -2;
  rDat := rd;
  anz := 0;
  i := start;
  lauf := f[0];
  sumX := 0.0;
  sumX2 := 0.0;
  sumY := 0.0;
  sumY2 := 0.0;
  sumXY := 0.0;
  while keinErr and (i <= endX) do
  begin
    hilfx := i;
    hilfy := f[i] / i;
    sumX := sumX + hilfX;
    sumY := sumY + hilfY;
    sumY2 := sumY2 + Sqr(hilfY);
    sumX2 := sumX2 + Sqr(hilfX);
    sumXY := sumXY + hilfX * hilfY;
    Inc(anz);
    Inc(i);
  end;
  if (anz < 2) and not keinErr then
    EXIT;
  realAnz := anz;
  hilfY := (realAnz * sumXY - sumX * sumY) / lauf;
  hilfX := realAnz * sumX2 - Sqr(sumX);
  rd[1]{m} := hilfY / hilfX;
  rd[2]{c} := (sumY / lauf - rd[1]{m} * sumX) / realAnz;
  t := (realAnz * sumY2 - Sqr(sumY)) / Sqr(lauf);
  if (t = 0) and (hilfY = 0) then
    rd[0] := 1
  else
    rd[0] := Abs(hilfY) / Sqrt(Abs(hilfX * t));
  rDat := rd;
end;


procedure MakeRegressDataPoly2(var rDat: TRegrData);    //a*x^2 +b*x + c
var
  i, anz: integer;
  rd: TRegrData;
  realAnz, y1, y2, y3, hilfA, hilfB, hilfC, sumA, sumB, sumC: extended;
begin
  rd := Default(TRegrData);
  rd[0] := -2;
  rDat := rd;
  anz := 0;
  i := start; //lauf:=f[0];
  sumA := 0.0;
  sumB := 0.0;
  sumC := 0.0;
  while (i <= endX - 2) do
  begin
    y1 := f[i];
    y2 := f[i + 1];
    y3 := f[i + 2];
    hilfA := (y3 - 2 * y2 + y1) / 2;
    hilfB := y2 - y1 - (2 * i + 1) * hilfA;
    hilfC := y1 - (hilfA * i * i + hilfB * i);
    sumA := sumA + hilfA;
    sumB := sumB + hilfB;
    sumC := sumC + hilfC;
    Inc(anz);
    Inc(i);
  end;
  if (anz < 2) then
    EXIT;
  realAnz := anz * f[0]; //anz*lauf
  rd[0] := 1;
  rd[1]{a} := sumA / realAnz;
  rd[2]{b} := sumB / realAnz;
  rd[3]{c} := sumC / realAnz;
  rDat := rd;
end;


type
  rfunc = function(r: double): double;


function Identitaet(r: double): double;
begin
  Identitaet := r;
end;

function LnFunc(r: double): double;
begin
  if r > 0 then
    LnFunc := Ln(r)
  else
  begin
    LnFunc := r;
    keinErr := False;
  end;
end;

function nLnFunc(r: double): double;
begin
  if r > 0 then
    nLnFunc := r * Ln(r)
  else
  begin
    nLnFunc := r;
    keinErr := False;
  end;
end;

function Wurzel(r: double): double;
begin
  Wurzel := sqrt(r);
end;

procedure MakeRegressData(rTyp: word; out rDat: TRegrData);
var
  i, anz, lauf: integer;
  xFunc, yFunc: rFunc;
  rd: TRegrData;
  hilfX, hilfY, realAnz, sumX, sumX2, sumY, sumY2, sumXY, t: extended;
begin
  rd := Default(TRegrData);
  rd[0] := -2;
  rDat := rd;
  if laenge <= 1 then
    EXIT;
  keinErr := True;
  if rTyp = poly_regress1 then
  begin
    MakeRegressDataPoly1(rDat);
    exit;
  end;
  if rTyp = poly_regress2 then
  begin
    MakeRegressDataPoly2(rDat);
    exit;
  end;
  case rTyp of
    lin_regress:
    begin
      xFunc := @Identitaet;
      yFunc := @Identitaet;
    end;
    log_regress:
    begin
      xFunc := @LnFunc;
      yFunc := @Identitaet;
    end;
    exp_regress:
    begin
      xFunc := @Identitaet;
      yFunc := @LnFunc;
    end;
    pot_regress:
    begin
      xFunc := @LnFunc;
      yFunc := @LnFunc;
    end;
    nLog_regress:
    begin
      xFunc := @nLnFunc;
      yFunc := @Identitaet;
    end;
    sqr_regress:
    begin
      xFunc := @Identitaet;
      yFunc := @Wurzel;
    end;
    else
      EXIT;
  end;
  anz := 0;
  i := start;
  lauf := f[0];
  sumX := 0.0;
  sumX2 := 0.0;
  sumY := 0.0;
  sumY2 := 0.0;
  sumXY := 0.0;
  while keinErr and (i <= endX) do
  begin
    hilfx := xFunc(i);
    hilfy := yFunc(f[i] / lauf);
    if keinErr then
    begin
      sumX := sumX + hilfX;
      sumY := sumY + hilfY;
      sumY2 := sumY2 + Sqr(hilfY);
      sumX2 := sumX2 + Sqr(hilfX);
      sumXY := sumXY + hilfX * hilfY;
      Inc(anz);
    end;
    Inc(i);
  end;
  if (anz < 2) and not keinErr then
    EXIT;

  realAnz := anz;
  hilfY := realAnz * sumXY - sumX * sumY;
  hilfX := realAnz * sumX2 - Sqr(sumX);
  rd[1]{m} := hilfY / hilfX;
  if rTyp = sqr_regress then
    rd[1]{m} := sqr(rd[1]);
  rd[2]{c} := (sumY - rd[1]{m} * sumX) / realAnz;

  t := realAnz * sumY2 - Sqr(sumY);
  if (t = 0) and (hilfY = 0) then
    rd[0] := 1
  else
    rd[0] := Abs(hilfY) / Sqrt(Abs(hilfX * t));
  if (rTyp and (exp_regress or pot_regress)) <> 0 then
    rd[2] := Exp(rd[2]);
  rDat := rd;
end;


procedure GetLinRegressData(var rDat: TRegrData);
begin
  MakeRegressData(lin_regress, rdat);
end;

procedure GetLogRegressData(var rDat: TRegrData);
begin
  MakeRegressData(log_regress, rdat);
end;

procedure GetExpRegressData(var rDat: TRegrData);
begin
  MakeRegressData(exp_regress, rdat);
end;

procedure GetPotRegressData(var rDat: TRegrData);
begin
  MakeRegressData(pot_regress, rdat);
end;

procedure GetNLogRegressData(var rDat: TRegrData);
begin
  MakeRegressData(nlog_regress, rdat);
end;

procedure GetSqrRegressData(var rDat: TRegrData);
begin
  MakeRegressData(sqr_regress, rdat);
end;

function AbweichungsQuadrat(ftyp: word; rd: TRegrData): double;
var
  qSum, exp_x, m, c, d, y0: double;
  i: integer;
begin
  keinErr := (laenge <> 0);
  qSum := 0;
  m := rd[1];
  c := rd[2];
  d := rd[3];
  if (m <= 0) and ((fTyp and (log_regress + nlog_regress)) <> 0) then
    keinErr := False;
  if keinErr then
  begin
    i := start;
    y0 := f[0];
    case fTyp of
      lin_regress: while i <= endX do
        begin
          qSum := qSum + Sqr(m * i + c - f[i] / y0);
          Inc(i);
        end;
      log_regress: while i <= endX do
        begin
          qSum := qSum + Sqr(m * Ln(i) + c - f[i] / y0);
          Inc(i);
        end;
      exp_regress: while keinErr and (i <= endX) do
        begin
          exp_x := m * i;
          if Abs(exp_x) < _Ln_max_Real then
            qSum := qSum + Sqr(EXP(exp_x) * c - f[i] / y0)
          else
            keinErr := False;
          Inc(i);
        end;
      pot_regress: while i <= endX do
        begin
          qSum := qSum + Sqr(Exp(m * Ln(i)) * c - f[i] / y0);
          Inc(i);
        end;
      nlog_regress: while i <= endX do
        begin
          qSum := qSum + Sqr(m * i * Ln(i) + c - f[i] / y0);
          Inc(i);
        end;
      poly_regress1,
      poly_regress2: if Abs(d) < 0.000001 then
          while i <= endX do
          begin
            qSum := qSum + Sqr(m * i * i + c * i - f[i] / y0);
            Inc(i);
          end
        else
          while i <= endX do
          begin
            qSum := qSum + Sqr(m * i * i + c * i + d - f[i] / y0);
            Inc(i);
          end;
      sqr_regress: while i <= endX do
        begin
          qSum := qSum + Sqr(m * i * i + c - f[i] / y0);
          Inc(i);
        end;
      else
        keinErr := False;
    end; {case}
  end;
  if keinErr then
    AbweichungsQuadrat := qSum
  else
    AbweichungsQuadrat := Abs(FehlerZahl);
end;

procedure GetBestRegressData(var ftyp: word; out rDat: TRegrData);
var
  k, t: integer;
  abw, testabw: double;
  regress: word;
  rd: TRegrData;
begin
  t := fTyp;
  rDat[0] := -2;
  abw := Abs(fehlerzahl);
  for k := do_lin_regress to Last_RegressTyp do
  begin
    regress := 1 shl k;
    if (t and regress) <> 0 then
    begin
      MakeRegressData(regress, rd);
      if (-2 < rd[0]) then
      begin
        testabw := AbweichungsQuadrat(regress, rd);
        if abw > testabw then
        begin
          rdat := rd;
          abw := testAbw;
          fTyp := regress;
        end;
      end;
    end;
  end;
end;



procedure AusgleichFunktion(aktf: TSpeicher; var regTyp: integer;
  out m, c, d: double; out fktStr: string);
var
  fRegData: TRegrData;
  fTyp: word;
  fStr: string;
  sm, sc: string; //String[40];

  function NormRealZahl(r: double): string;
  begin
    Result := FloatToStrF(r, ffGeneral, 5, 2);
  end;

begin
  f := aktf;
  laenge := GetLaenge(f);
  fTyp := regTyp;
  GetBestRegressData(ftyp, fRegData);
  m := fRegData[1];
  c := fRegData[2];
  d := fRegData[3];
  regTyp := fTyp;
  sm := NormRealZahl(fRegData[1]);   {m}
  sc := NormRealZahl(Abs(fRegData[2]));
  if fRegData[2] >= 0 then
    sc := ' + ' + sc
  else
    sc := ' - ' + sc;   {c}
  case fTyp of
    lin_regress: fstr := sm + '*n' + sc;
    log_regress: fstr := sc + ' + ' + sm + '* Ln(n)';
    pot_regress: fstr := sc + '* n^' + sm;
    nlog_regress: fstr := sm + '* n*Ln(n)' + sc;
    poly_regress1: fstr := sm + '* n^2' + sc + '* n';
    poly_regress2: fstr := sm + '* n^2' + sc + '* n + ' + NormRealZahl(d);
    exp_regress: fstr := sc + '* Exp(' + sm + '*n)';
    sqr_regress: fstr := sm + '* n^2' + sc;
    else
      fstr := '???'
  end;
  fktStr := fStr;
end;

procedure ZeichneAusgleich(pb: TPaintbox; regTyp: integer; yFak, m, c, d: double);
var
  x, y, y0: integer;
begin
  y0 := pb.Height - offset;
  case regTyp of
    lin_regress: with pb.canvas do
      begin
        y := Round((m * minFeldLaenge + c) / yFak);
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round((m * x + c) / yFak));
      end;
    log_regress: with pb.canvas do
      begin
        y := Round((c + m * ln(minFeldLaenge)) / yFak);
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round((c + m * ln(x)) / yFak));
      end;
    exp_regress: with pb.canvas do
      begin
        c := c / yFak;
        y := Round(c * exp(m * minFeldLaenge));
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round(c * exp(m * x)));
      end;
    pot_regress: with pb.canvas do
      begin
        c := c / yFak;
        y := Round(c * exp(m * ln(minFeldLaenge)));
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round(c * exp(m * ln(x))));
      end;

    nlog_regress: with pb.canvas do
      begin
        y := Round((c + m * minFeldLaenge * ln(minFeldLaenge)) / yFak);
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round((m * x * ln(x) + c) / yFak));
      end;
    poly_regress1,
    poly_regress2: with pb.canvas do
      begin
        y := Round((m * minFeldLaenge * minFeldLaenge + c * minFeldLaenge + d) / yFak);
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round((m * x * x + c * x + d) / yFak));
      end;
    sqr_regress: with pb.canvas do
      begin
        y := Round((m * minFeldLaenge * minFeldLaenge + c) / yFak);
        moveTo(offset + minFeldLaenge, y0 - y);
        for x := minFeldLaenge + 1 to laenge do
          LineTo(offset + x, y0 - Round((m * x * x + c) / yFak));
      end;
  end;
end;


end.
