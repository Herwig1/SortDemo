unit SimpFunktion;

{ UNIT          : Funktionen : Scannen, Berechnen
  Version       : 0.4.1   letzte Änderung: 12.01.2024
  Autor         : H. Niemeyer
 }

interface

uses
  LCLIntf, LCLType,
  SysUtils, Messages, Classes, Graphics, Controls, ColorBox,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
  SortUnitSprache;

const
  {globale Zahlen für Steuerung }
  _max_Real = 1.7e38;
  smartFloatMax: single = 1E6;
  smartFloatMin: single = 1E-6;
  _Ln_max_Real = 88;
  _p_unendlich = 1.69e38;
  _m_unendlich = -1.69e38;
  zahl_ndef = 1.2345678901e37;

  { mathematische Konstanten }
  zahl_e = 2.178281828;
  zahl_pi = 3.141592654;
  zahl_ln10 = 2.302585093;


type
  symTyp = (udef, oldfx, istZahl, formVar, ident, lklammer, rklammer,
    plus, minus, mal, durch, pot,
    expf, lnf, sqrtf, ffmax, fstrEnd, userfkt);

  TUpdateMode = (umAuto, umManual);
  TDiagrammName = string[10];


const
  fnam: array[lklammer..ffmax] of string //TDiagrammName
  = ('(', ')', '+', '-', '*', '/', '^',
    'EXP', 'LN', 'SQRT', '?');

type
  PFkt_Term = ^TFkt_Term;

  TFkt_Term = record
    Next: PFkt_Term;
    seen: byte;
    case cwert: symTyp of
      udef: (udli, udre: PFkt_Term);
      istZahl: (zwert: single);
      ident: (vwert: TDiagrammName);

      plus, minus, mal, durch, pot,
      expf, lnf, sqrtf, ffmax: (li, re: PFkt_Term);
  end;

  PFkt_List_Element = ^TFkt_List_Element;

  TFkt_List_Element = record
    nam: TDiagrammName;
    Next: PFkt_List_Element;
    fkt: PFkt_Term
  end;

  TFktListe = class(TObject)
  private
    parL: PFkt_Term;
    fktL: PFkt_List_Element;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Merke(fx: PFkt_Term; fname: TDiagrammName);
    procedure Loesch(var fx: PFkt_Term);
    procedure ClearUnUsed;
    function Zahl(r: single): PFkt_Term;
    function MakeNode(op1: PFkt_Term; code: symTyp; op2: PFkt_Term): PFkt_Term;
  end;

  TFkt = class(TComponent)
  private
    Liste: TFktListe;
    fx: PFkt_Term;
    fFormel: string;
    fDiagrammName: TDiagrammName;
    fvisible: boolean;
    fScanResult: byte;

    function FindDiagrammName(Value: TDiagrammName): PFkt_List_Element;
    function WertIntPot(bas: single; ex: integer): single;
  protected
    procedure SetDiagrammName(Value: TDiagrammName); virtual;
    procedure SetFormel(const Value: string); virtual;

  published
    property ScanResult: byte read fScanResult;
    property Formel: string read fFormel write SetFormel;
    property DiagrammName: TDiagrammName read fDiagrammName write SetDiagrammName;
    property Visible: boolean read fvisible write fvisible default True;
  public
    geaendert: boolean;
    ScanErrPos: integer;
    calcErr: boolean;
    color: Tcolor;
    property fktx: PFkt_Term read fx write fx;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EingabeFktStr; virtual;
    procedure MakeFkt(fName: TDiagrammName; const fktStr: string); virtual;
    function Zahl_Str(r: single): string; virtual;
    function GetFktStr: string; virtual;
    procedure GetFktUPNStr(var s: string); virtual;
    procedure ScanFkt(const fktStr: string; out ok: boolean);
    function FktWert(x: single): single;
    function CalcFktWert(op: PFkt_Term; x: single): single; virtual;
    function Vb(op1: PFkt_Term; code: symTyp; op2: PFkt_Term): PFkt_Term;
  end;

type
  TFunktionEditor = class(TForm)
    PanelEingabe: TPanel;
    LabelFehler: TLabel;
    EditFormel: TEdit;
    PanelRechts: TPanel;
    ButtonFehler: TButton;
    ButtonCurLeft: TButton;
    ButtonCurRight: TButton;
    ButtonCurHome: TButton;
    ButtonCurEnde: TButton;
    Panel1: TPanel;
    PanelLeft: TPanel;
    ListBoxSysFkt: TListBox;
    PanelTasten: TPanel;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    ButtonMal: TButton;
    ButtonDurch: TButton;
    ButtonHoch: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Buttonklzu: TButton;
    ButtonKlauf: TButton;
    ButtonX: TButton;
    Button0: TButton;
    Buttonkomma: TButton;
    ButtonDel: TButton;
    ButtonDelBk: TButton;
    Panel2: TPanel;
    BitBtnCancel: TBitBtn;
    BitBtnOk: TBitBtn;
    ColorBox1: TColorBox;
    procedure ButtonZahlClick(Sender: TObject);
    procedure ListBoxSysFktClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditFormelChange(Sender: TObject);
    procedure ButtonFehlerClick(Sender: TObject);
    procedure ButtonSpezialClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    neuf: TFkt;
    fehler: integer;
  public
    { Public-Deklarationen }
    function ExecuteDlg(afkt: TFKT): boolean;
  end;


var
  F_Liste: TFktListe;
  FunktionEditor: TFunktionEditor;

function ScanErrMsg(nr: byte): string;

implementation

{$R *.lfm}

var
  xVar, noOp, z0, z1, z2, zm1, noFkt: PFkt_Term;

function ScanErrMsg(nr: byte): string;
begin
  case nr of
    1: ScanErrMsg := sprachTab[s_KlammerAuf];  // '( erwartet';
    2: ScanErrMsg := sprachTab[s_KlammerZu];   // ') erwartet';
    3: ScanErrMsg := sprachTab[s_Operator];    //'Operator erwartet';
    4: ScanErrMsg := sprachTab[s_KlammerZahlBezeichner];
    //'( oder Zahl oder n erwartet';
    5: ScanErrMsg := sprachTab[s_NichtErlaubt]; //'unzulässige Eingabe';

    7: ScanErrMsg := sprachTab[s_ZahlZuGross];  //'Zahl zu groß';
    8: ScanErrMsg := sprachTab[s_keineFormel];  //'keine Formel angegeben';
    else
      ScanErrMsg := sprachTab[s_FormelNichtInterpretiert];
      //'Formel nicht interpretiert';
  end;
end;  { GetScanErrMsg }




function TFktListe.MakeNode(op1: PFkt_Term; code: symTyp; op2: PFkt_Term): PFkt_Term;
var
  p: PFkt_Term;
begin
  New(p);
  FillChar(p^, SizeOf(p^), #0);
  p^.cwert := code;
  p^.li := op1;
  p^.re := op2;
  MakeNode := p;
end;

function TFktListe.Zahl(r: single): PFkt_Term;
var
  p, hilf: PFkt_Term;
begin
  hilf := parL;
  while hilf^.Next^.cwert = istZahl do
  begin
    hilf := hilf^.Next;
    if hilf^.zwert = r then
    begin
      Zahl := hilf;
      EXIT;
    end;
  end;
  New(p);
  FillChar(p^, SizeOf(p^), #0);
  p^.cwert := istZahl;
  p^.Next := hilf^.Next;
  p^.seen := 1;
  p^.zwert := r;
  hilf^.Next := p;
  Zahl := p;
end;   { Zahl }


constructor TFktListe.Create;
var
  p: PFkt_Term;
begin
  inherited Create;
  noOp := MakeNode(nil, udef, nil);
  noFkt := MakeNode(noOp, ffmax, noOp);
  New(xVar);
  FillChar(xVar^, SizeOf(xVar^), #0);
  xVar^.cwert := ident;
  xVar^.vwert := 'n';
  xVar^.Next := noFkt;
  noOp^.Next := xVar;
  parL := noOp;
  zm1 := Zahl(-1);
  z0 := Zahl(0);
  z1 := Zahl(1);
  z2 := Zahl(2);

  p := parL;
  repeat
    p^.seen := 2;
    p := p^.Next
  until p = nil;
  fktL := nil;
end;  { InitFkt }

destructor TFktListe.Destroy;
var
  p: PFkt_Term;
  h: PFkt_List_Element;
begin
  while parL <> nil do
  begin
    p := parL;
    parL := parL^.Next;
    Dispose(p);
  end;
  noOp := nil;
  z0 := nil;
  z1 := nil;
  z2 := nil;
  zm1 := nil;
  noFkt := nil;
  xVar := nil;
  while fktL <> nil do
  begin
    h := fktL;
    fktL := fktL^.Next;
    DisPose(h);
  end;
  inherited Destroy;
end;  { ExitFkt }

procedure TFktListe.Merke(fx: PFkt_Term; fname: TDiagrammName);
var
  neufx: PFkt_List_Element;
  weiter: boolean;
  { i     : BYTE;  }
begin
  neufx := fktL; {nil;}
  weiter := True;
  while (neufx <> nil) and weiter do
  begin
    weiter := neufx^.nam <> fName;
    if weiter then
      neufx := neufx^.Next;
  end;
  fx^.seen := 2;
  { FOR i:=1 TO Length(fname) DO fname[i]:=UpCase(fname[i]); }
  if neufx = nil then
  begin
    New(neufx);
    neufx^.Next := fktL;
    neufx^.nam := fname;
    fktL := neufx;
  end;
  neufx^.fkt := fx;
end;  { Merke }


procedure TFktListe.Loesch(var fx: PFkt_Term);
var
  p, h: PFkt_Term;
  fl, fh: PFkt_List_Element;

  procedure Mark(p: PFkt_Term);
  begin
    case p^.seen of
      0: Inc(p^.seen);
      1: EXIT
    end;
    if p^.cwert > ident then
    begin
      Mark(p^.li);
      Mark(p^.re);
    end;
  end;

begin   { Loesch }
  if (fx <> nil) and (fx <> noFkt) and (fx <> parL) and (fktL <> nil) then
  begin
    fl := fktL;  { lösche Fkt. aus der Funktionsliste }
    if fktL^.fkt = fx then
      fktL := fktL^.Next
    else
    begin
      while (fl^.Next <> nil) and (fl^.Next^.fkt <> fx) do
        fl := fl^.Next;
      if (fl^.Next = nil) then
        exit;
      fh := fl^.Next;
      fl^.Next := fh^.Next;
      fl := fh;
    end;
    DisPose(fl);
    if (fx <> noFkt) and (fx <> xVar) and (fx <> z0) and (fx <> z1) and
      (fx <> z2) and (fx <> zm1) then
    begin
      fx^.seen := 0;
      fx := nil;
    end;
  end;
  p := z2;              { setze parameter auf ungültig/nicht verwendet }
  while p <> noFkt do
  begin
    p^.seen := p^.seen and 2;
    p := p^.Next;
  end;
  fl := fktL;            { setze parameter auf gültig/verwendet }
  while fl <> nil do
  begin
    if fl^.fkt <> nil then
      Mark(fl^.fkt);
    fl := fl^.Next;
  end;

  p := z2;              { lösche alle ungültigen parameter }
  while p^.Next <> nil do
    if p^.Next^.seen > 0 then
      p := p^.Next
    else
    begin
      h := p^.Next;
      p^.Next := h^.Next;
      DisPose(h);
    end;
end;    { Loesch }

procedure TFktListe.ClearUnUsed;
begin
  Loesch(parL);
end;



{ %%%%%%%%%%%%%%%%%%%% TFkt %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% }
constructor TFkt.Create(aOwner: TComponent);
var
  fName: TDiagrammName;
  k: word;
begin
  inherited Create(aOwner);
  if F_Liste = nil then
    F_Liste := TFktListe.Create;
  liste := F_Liste;
  { fScanResult:=0;}
  fx := xVar;
  fFormel := 'n';
  fName := 'f';
  k := 1;
  while FindDiagrammName(fName) <> nil do
  begin
    fName := 'f' + IntToStr(k);
    Inc(k);
  end;
  SetDiagrammName(fName);
  liste.Merke(fx, fName);
{  fvisible:=false;
   color:=clBlue; }
end;

destructor TFkt.Destroy;
begin
  if Liste <> nil then
  begin
    Liste.Loesch(fx);
    Liste := nil;
  end;
  inherited Destroy;
end;

procedure TFkt.Assign(Source: TPersistent);
var
  s: TFkt;
begin
  if (Source <> nil) and (Source is TFkt) then
  begin
    s := (Source as TFkt);
    Liste := s.Liste;
    fFormel := s.formel;
    color := s.color;
    fx := s.fx;
    fScanResult := 0;
  end;
end;

procedure TFkt.EingabeFktStr;
var
  FktEditor: TFunktionEditor;
begin
  FktEditor := TFunktionEditor.Create(nil);
  FktEditor.ExecuteDlg(self);
  FktEditor.Free;
end;

function TFkt.FindDiagrammName(Value: TDiagrammName): PFkt_List_Element;
var
  weiter: boolean;
begin
  Result := liste.fktL;
  weiter := True;
  while (Result <> nil) and weiter do
  begin
    weiter := (CompareText(Value, Result^.nam) <> 0);
    if weiter then
      Result := Result^.Next;
  end;
end;


procedure TFkt.SetDiagrammName(Value: TDiagrammName);
var
  namefx: PFkt_List_Element;
  neu: TDiagrammName;
begin
  if (liste <> nil) and (Value <> DiagrammName) then
  begin
    if (findDiagrammName(Value) = nil) then
    begin
      neu := Value;
      neu := AnsiUpperCase(neu);
      namefx := findDiagrammName(diagrammName);
      if namefx <> nil then
        namefx^.nam := neu
      else if (fx <> nil) and (fx <> nofkt) then
        liste.Merke(fx, neu);
      fDiagrammName := Value;
    end;
  end;
end;

procedure TFkt.SetFormel(const Value: string);
var
  hilf: PFkt_Term;
  ok: boolean;
  neu: TDiagrammName;
begin
  if ((Value <> fFormel) or (fScanResult <> 0)) and (liste <> nil) and (Value <> '') then
  begin
    hilf := fx;
    ScanFkt(Value, ok);
    if ok then
    begin
      ok := (FindDiagrammName(DiagrammName) <> nil);
      neu := DiagrammName;
      neu := AnsiUpperCase(neu);
      liste.Merke(fx, neu);
      fFormel := Value;
      if ok then
        liste.Loesch(hilf);
      geaendert := True;
    end
    else
      fx := hilf;
  end;
end;


procedure TFkt.MakeFkt(fName: TDiagrammName; const fktStr: string);
var
  ok: boolean;
begin  {MakeFkt}
  ScanFkt(fktStr, ok);
  if ok then
    Liste.Merke(fx, fname);
end;

function TFkt.Zahl_Str(r: single): string;
var
  zStr: string; //STRING[20];
  sign: string[1];
begin
  if r < 0 then
  begin
    sign := '-';
    r := -r;
  end
  else
    sign := '';
  if r = pi then
    zStr := 'pi'
  else if r = zahl_e then
    zStr := 'e'
  else
    zStr := FloatToStrF(r, ffgeneral, 7, 0);{RealFloatString(r);}
  Zahl_Str := sign + zStr;
end;


function TFkt.GetFktStr: string;

  procedure PriFkt(fx: PFkt_Term);
  var
    code: symTyp;

    procedure PriZweig(b: boolean; code: SymTyp; op: PFkt_Term);
    var
      klammer: boolean;
    begin
      klammer := (code > pot) or ((op^.cwert >= plus) and
        (((code >= minus) and (op^.cwert <= minus) and b) or
        ((code >= mal) and (op^.cwert < code))));
      if klammer then
        Result := Result + '(';
      PriFkt(op);
      if klammer then
        Result := Result + ')';
    end;

  begin
    if (fx = nil) or (fx = noOp) then
      EXIT;
    { IF fx=xVar THEN BEGIN s:=s+'x';EXIT END; }
    case fx^.cwert of
      udef: EXIT;
      istZahl: Result := Result + Zahl_Str(fx^.zwert);
      ident: Result := Result + fx^.vwert;
      ffmax: Result := Result + '?';
      else
      begin
        code := fx^.cwert;            { normale Darstellung }
        if code <= lKlammer then
          Result := Result + '(';
        if code < expf then
          PriZweig(False, code, fx^.li);
        if code < ffmax then
          Result := Result + fnam[code]
        else
          Result := Result + '?';
        PriZweig(True, code, fx^.re);
        if code <= lKlammer then
          Result := Result + ')';
      end
    end;
  end;

begin
  Result := '';
  PriFkt(fx);
  while Pos('-1*', Result) > 0 do
    system.Delete(Result, Pos('-1*', Result) + 1, 2);
  while Pos('+-', Result) > 0 do
    system.Delete(Result, Pos('+-', Result), 1);
end;

procedure TFkt.GetFktUPNStr(var s: string);

  procedure PriFkt(fx: PFkt_Term);
  begin
    if (fx = nil) or (fx = noOp) then
      EXIT;
    {  IF fx=xVar THEN BEGIN s:=s+' x';EXIT END; }
    case fx^.cwert of
      udef: EXIT;
      istZahl: s := s + ' ' + Zahl_Str(fx^.zwert);
      ident: s := s + ' ' + fx^.vwert;
      ffmax: s := s + ' ?';
      else
      begin                     { umgekehrt polnische Notation - UPN }
        PriFkt(fx^.li);
        PriFkt(fx^.re);
        if fx^.cwert < ffmax then
          s := s + ' ' + fnam[fx^.cwert]
        else
          s := s + ' ?';
      end
    end;
  end;

begin
  s := '';
  PriFkt(fx);
end;


function TFkt.Vb(op1: PFkt_Term; code: symTyp; op2: PFkt_Term): PFkt_Term;

  function Member(op: PFkt_Term): PFkt_Term;
  var
    hilf: PFkt_Term;

    function Gleich(op1, op2: PFkt_Term): boolean;
    begin
      if op1 = op2 then
      begin
        Gleich := True;
        EXIT;
      end;
      if (op1^.cwert <> op2^.cwert) or (op1^.cwert <= ident) then
        Gleich := False
      else
        Gleich := (op1^.re = op2^.re) and (op1^.li = op2^.li);
    end;

  begin { Member }
    hilf := xVar;
    while hilf^.Next^.cwert <= op^.cwert do
    begin
      hilf := hilf^.Next;
      if Gleich(op, hilf) then
      begin
        Dispose(op);
        Member := hilf;
        EXIT;
      end;
    end;
    op^.Next := hilf^.Next;
    op^.seen := 1;
    hilf^.Next := op;
    Member := op;
  end;  { Member }

begin
  Vb := Member(liste.MakeNode(op1, code, op2));
end;


{ *********************** Funktionsterm ermitteln ************************** }

procedure TFkt.ScanFkt(const fktStr: string; out ok: boolean);
var
  chPos, oldChPos: byte;
  ch, ch_: char;
  sym: symTyp;
  wert: single;
  maxLang: integer;

  procedure Err(nr: byte);
  begin
    ch := #0;
    chPos := 255;
    sym := ffmax;
    if not OK then
      EXIT;
    ScanErrPos := oldchPos;
    fScanResult := nr;
    ok := False;
  end;  { Err }

  procedure GetCh;
  begin
    if chPos < maxLang then
    begin
      Inc(chPos);
      ch_ := fktStr[chPos];
      ch := UpCase(ch_);
    end
    else
    begin
      ch := #0;
      chPos := 255;
    end;
  end;

  procedure Identifier;
  var
    buf, buf1: string;
  begin
    buf := '';
    buf1 := '';
    repeat
      buf := buf + ch;
      buf1 := buf1 + ch_;
      GetCh
    until ((ch < '0') or ((ch > '9') and (ch < 'A')) or (ch > 'Z')) and (ch <> '''');
    if not Ok then
      EXIT;
    if buf = 'N' then
    begin
      sym := ident;
      EXIT;
    end;
    if buf = 'PI' then
    begin
      sym := istZahl;
      wert := pi;
      EXIT;
    end;
    sym := expf;
    while (sym < ffmax) and (buf <> fnam[sym]) do
      Inc(sym);
    if sym = ffmax then
    begin
      if buf = 'E' then
        if ch = '^' then
        begin
          sym := expf;
          GetCh;
          EXIT;
        end
        else
        begin
          sym := istZahl;
          wert := zahl_e;
          EXIT;
        end
      else
        Err(5);
    end;
  end;   { Identifier }

  procedure Number;
  var
    hstr: string; //STRING[20];
    npos: integer;
  begin
    sym := istZahl;
    hstr := Copy(fktstr, chPos, maxLang - Pred(chPos));
    if Pos(',', hstr) > 0 then
      hStr[Pos(',', hstr)] := '.';
    try
      Val(hstr, wert, npos);
    finally
      if npos > 0 then
      begin
        hstr := Copy(hStr, 1, npos - 1);
        VAL(hstr, wert, npos);
      end;
      {Err(7); Exit }
    end;

    Inc(chPos, Length(hStr) - 1);
    GetCh;
  end;  { Number }

  procedure MakeSym(s: symTyp);
  begin
    sym := s;
    GetCh;
  end;

  procedure GetSym;
  begin
    while ch = ' ' do
      GetCh;
    oldChPos := chPos;
    if ('A' <= ch) and (ch <= 'Z') then
      Identifier
    else
    if ('0' <= ch) and (ch <= '9') then
      Number
    else
      case ch of
        '(': MakeSym(lklammer);
        ')': MakeSym(rklammer);
        '*': MakeSym(mal);
        '+': MakeSym(plus);
        '-': MakeSym(minus);
        '/': MakeSym(durch);
        '^': MakeSym(pot);
        #0: sym := fStrEnd;
        else
          Err(5)
      end;
  end;   { GetSym }



  procedure Expression(out expr: PFkt_Term);
  var
    addop: symTyp;
    temp: PFkt_Term;

    procedure Term(out prod: PFkt_Term);
    var
      pSym: symTyp;
      temp: PFkt_Term;

      procedure FuncTerm(out fterm: PFkt_Term);
      var
        fsym: symTyp;
        temp: PFkt_Term;

        procedure Faktor(out fakt: PFkt_Term);
        begin
          case sym of
            ident: fakt := xVar;
            istZahl: fakt := liste.Zahl(wert);
            lklammer:
            begin
              GetSym;
              Expression(fakt);
              if sym <> rklammer then
                Err(2);
            end;
            minus:
            begin
              GetSym;
              if sym = istZahl then
                fakt := liste.Zahl(-wert)
              else
                Err(4);
            end;
            else
            begin
              fakt := noOp;
              Err(4);
            end;
          end;
          GetSym;
          if sym < lklammer then
            Err(3);
        end;  { Faktor }

      begin    { FuncTerm }
        if sym < expf then
          Faktor(fTerm)
        else
        begin
          fTerm := noOp;
          if (sym = fStrEnd) or (sym = ffmax) then
            Err(4);
        end;
        while (pot <= sym) and (sym < ffmax) do
        begin
          fSym := sym;
          GetSym;
          if fsym > pot then
            if sym <> lklammer then
            begin
              temp := noOp;
              Err(1);
            end;
          if sym < expf then
            Faktor(temp)
          else
            FuncTerm(temp);
          fTerm := Vb(fTerm, fSym, temp);
        end;
      end;   { FuncTerm }

    begin  {Term}
      FuncTerm(prod);
      while (sym = mal) or (sym = durch) do
      begin
        pSym := sym;
        GetSym;
        Functerm(temp);
        prod := Vb(prod, pSym, temp);
      end;
    end;   {Term}

    procedure Test(var addOp: symTyp);
    begin
      if (addOp = minus) and (sym = istZahl) then
      begin
        addOp := plus;
        wert := -wert;
      end;
    end;

  begin   {Expression}
    if (sym = plus) or (sym = minus) then
    begin
      addop := sym;
      GetSym;
      Test(addOp);
    end
    else
      addop := plus;
    Term(expr);
    if addop = minus then
      expr := Vb(zm1, mal, expr);
    while (sym = plus) or (sym = minus) do
    begin
      addop := sym;
      GetSym;
      Test(addOp);
      Term(temp);
      expr := Vb(expr, addop, temp);
    end;
  end;   {Expression}

begin  {ScanFkt}
  fx := nil;
  chPos := 0;
  ch := ' ';
  ok := True;
  ScanErrPos := 0;
  fScanResult := 0;
  maxLang := Length(fktStr);
  if fktStr <> '' then
  begin
    Liste.ClearUnused;
    GetSym;
    Expression(fx);
    if not ok then
    begin
      Liste.ClearUnused;
      fx := noFkt;
    end;
  end
  else
  begin
    fx := noFkt;
    fScanResult := 8;
    ok := False;
  end;
end; {ScanFkt}


{ ************  Berechne Funktionswert von fx an der Stelle x ************* }
function TFkt.WertIntPot(bas: single; ex: integer): single;
var
  i: integer;
  r: single;
begin
  r := 1.0;
  for i := 1 to Abs(ex) do
    r := r * bas;
  if (ex < 0) then
  begin
    try
      r := 1 / r
    except
      r := zahl_ndef;
    end;
  end;
  WertIntPot := r;
end;



function TFkt.CalcFktWert(op: PFkt_Term; x: single): single;
var
  error: boolean;

  function Berechne(op: PFkt_Term): single;
  var
    erg1, erg2: single;
  begin
    Result := 0;
    erg1 := 0;
    if op = nil then
    begin
      error := True;
      exit;
    end;
    if op = xVar then
      Berechne := x
    else if op^.cwert = istZahl then
      Berechne := op^.zwert
    else
    begin
      erg2 := Berechne(op^.re);
      if error then
        EXIT;
      if op^.cwert <= pot then
      begin
        erg1 := Berechne(op^.li);
        if error then
          EXIT;
        case op^.cwert of
          plus: erg1 := erg1 + erg2;
          minus: erg1 := erg1 - erg2;
          mal: erg1 := erg1 * erg2;
          durch:
          try
            erg1 := erg1 / erg2
          except
            error := True;
          end;
          pot: if Trunc(erg2) = erg2 then
              erg1 := WertIntPot(erg1, Trunc(erg2))
            else
            try
              erg1 := Exp(erg2 * Ln(erg1))
            except
              error := True;
            end;
          else
            error := True
        end;
      end
      else
        case op^.cwert of
          expf: erg1 := Exp(erg2);
          lnf:
          try
            erg1 := Ln(erg2)
          except
            error := True;
          end;
          sqrtf:
          try
            erg1 := sqrt(erg2)
          except
            error := True;
          end;
          else
            error := True;
        end;
      Berechne := erg1;
    end;
  end;

begin
  error := False;
  CalcFktWert := Berechne(op);
  if error then
    CalcFktWert := zahl_ndef;
end;

function TFkt.FktWert(x: single): single;
begin
  if (fx <> nil) and (fx <> nofkt) then
    FktWert := CalcFktWert(fx, x)
  else
    FktWert := zahl_ndef;
end;



(*************** TFunktionEditor *************)

procedure TFunktionEditor.FormCreate(Sender: TObject);
begin
  neuF := TFkt.Create(nil);
  editFormel.Text := neuf.Formel;
  ButtonFehler.Caption := sprachTab[s_FehlerAnzeigen];
  BitBtnOK.Caption := sprachTab[s_OK];
  BitBtnCancel.Caption := sprachTab[s_Abbrechen];
end;

procedure TFunktionEditor.FormActivate(Sender: TObject);
begin
  labelFehler.Visible := False;
  editFormel.SetFocus;
end;

procedure TFunktionEditor.FormDestroy(Sender: TObject);
begin
  F_Liste.ClearUnused;
  neuF.Free;
  neuF := nil;
end;


procedure TFunktionEditor.ButtonZahlClick(Sender: TObject);
begin
  with editFormel do
  begin
    SelText := (Sender as TButton).Caption;
    SetFocus;
    selstart := selstart + sellength;
  end;
end;

procedure TFunktionEditor.ListBoxSysFktClick(Sender: TObject);
begin
  with editFormel do
  begin
    seltext := listboxSysFkt.items[listboxSysFkt.ItemIndex] + '()';
    SetFocus;
    selstart := selstart + sellength - 1;
  end;
end;



procedure TFunktionEditor.EditFormelChange(Sender: TObject);
var
  formel1: string;
  keinFehler: boolean;
begin
  labelFehler.Visible := False;
  formel1 := editFormel.Text;
  neuf.ScanFkt(formel1, keinFehler);
  if keinFehler then
  begin
    neuf.fFormel := formel1;
    fehler := fehler and 1;
  end
  else
    fehler := fehler or 2;
  bitbtnOk.Enabled := fehler = 0;
  buttonFehler.Enabled := (fehler and 2) = 2;
end;

procedure TFunktionEditor.ButtonFehlerClick(Sender: TObject);
begin
  if neuf.ScanResult <> 0 then
  begin
    with editFormel do
    begin
      selstart := neuf.ScanErrPos - 1;
      selLength := 1;
      SetFocus;
    end;
    labelFehler.Caption := ScanErrMsg(neuf.ScanResult);
    labelFehler.Visible := True;
  end;
end;

procedure TFunktionEditor.ButtonSpezialClick(Sender: TObject);
var
  virtKeyCode: word;
begin
  virtKeyCode := vk_End;  { Warnung Delphi 3.0 }
  case (Sender as TButton).tag of
    0: virtKeyCode := vk_Delete;
    1: virtKeyCode := vk_Back;
    2: virtKeyCode := vk_Left;
    3: virtKeyCode := vk_Right;
    4: virtKeyCode := vk_Home;
    5: virtKeyCode := vk_End;
  end;
  editFormel.SetFocus;
  postMessage(editFormel.handle, wm_keyDown, virtKeyCode, 0);
end;


function TFunktionEditor.ExecuteDlg(afkt: TFKT): boolean;
begin
  if aFkt = nil then
  begin
    Result := False;
    exit;
  end;
  ColorBox1.Selected := aFkt.color;
  editFormel.Text := aFkt.Formel;
  editFormel.selstart := 0;
  editFormel.selLength := Length(editFormel.Text);
  Result := False;
  if ShowModal = mrOk then
  begin
    neuF.fx := nil;
    aFkt.Formel := neuF.fFormel;
    aFkt.color := ColorBox1.Selected;
    Result := True;
  end;
end;


end.
