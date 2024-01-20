unit SortZahlenfeldDlg;

 (* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Zahlenfeld Dialog
   Autor:   H. Niemeyer  (c) 2010 - 2017
   Version: 1.6

   letzte Änderung: 22.08.2017 *)

interface

uses SysUtils, Classes, Graphics, Forms, Controls,
  Buttons, Grids, Dialogs,
  SortUnitDefinitionen, SortUnitSprache;

type
  TZahlenfeldDlg = class(TForm)
    StringGrid1: TStringGrid;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    StringGrid2: TStringGrid;
    BitBtnLoad: TBitBtn;
    BitBtnSave: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormResize(Sender: TObject);
    procedure BitBtnLoadClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    zAnz, stringGridW, heapGr: integer;
    zfeld: TFeld;
    split, feldDef: boolean;
    procedure VerteileZahlenFeld;
  public
    { Public declarations }
    procedure SetAnzahl(n: integer);
    function GetAnzahl: integer;
    procedure SetHeap(n: integer);
    function GetHeap: integer;
    procedure SetZahlen(zahlenfeld: TFeld);
    procedure GetZahlen(var zahlenfeld: TFeld);
    procedure SetWeite(w: integer);
    procedure SetSplit(aSplit: boolean);
    function GetSplit: boolean;
  end;

var
  ZahlenfeldDlg: TZahlenfeldDlg;

implementation

{$R *.lfm}

procedure TZahlenfeldDlg.BitBtnLoadClick(Sender: TObject);
var
  k, n, p: integer;
  sList: TStringList;
begin
  openDialog1 := TOpenDialog.Create(self);
  openDialog1.Title := sprachTab[s_Laden];
  openDialog1.DefaultExt := 'list';
  if openDialog1.Execute then
  begin
    sList := TStringList.Create;
    sList.LoadFromFile(OpenDialog1.FileName);
    try
      n := StrToInt(sList[0]);
      zAnz := n;
      p := StrToInt(sList[1]);
      heapGr := StrToInt(sList[2]);
      stringGrid1.ColCount := n;
      for k := 0 to n - 1 do
        stringGrid1.Cells[k, 0] := sList[k + 3];
      if p > 0 then
      begin
        split := True;
        stringGrid2.Visible := split;
        zAnz := n + p;
        stringGrid2.ColCount := p;
        for k := 0 to p - 1 do
          stringGrid2.Cells[k, 0] := sList[n + 3 + k];
      end;
    finally
      sList.Free;
    end;
  end;
  openDialog1.Destroy;
end;

procedure TZahlenfeldDlg.BitBtnSaveClick(Sender: TObject);
var
  k, n, p: integer;
  s: string;
  sList: TStringList;
begin
  SaveDialog1 := TSaveDialog.Create(self);
  SaveDialog1.Title := sprachTab[s_Speichern];
  SaveDialog1.DefaultExt := 'list';
  if SaveDialog1.Execute then
  begin
    p := 0;
    sList := TStringList.Create;
    n := stringGrid1.ColCount;
    sList.Add(IntToStr(n));
    if split then
    begin
      p := stringGrid2.ColCount;
      sList.Add(IntToStr(p));
    end
    else
      sList.Add('0');
    if heapGr > 0 then
      sList.Add(IntToStr(heapGr))
    else
      sList.Add('0');
    for k := 0 to n - 1 do
    begin
      s := stringGrid1.Cells[k, 0];
      sList.Append(s);
    end;
    if split then
    begin
      for k := 0 to p - 1 do
      begin
        s := stringGrid2.Cells[k, 0];
        sList.Append(s);
      end;
    end;
    sList.SaveToFile(SaveDialog1.FileName);
    sList.Free;
  end;
  SaveDialog1.Destroy;
end;

procedure TZahlenfeldDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  n, k, j: integer;
begin
  if modalResult <> mrOk then
    exit;
  k := 0;
  j := zAnz - zAnz div 2;
  try
    if split then
    begin
      for n := 0 to j - 1 do
      begin
        k := n;
        zFeld[n + 1] := StrToInt(StringGrid1.Cells[n, 0]);
      end;
      for n := j to zAnz - 1 do
      begin
        k := n;
        zFeld[n + 1] := StrToInt(StringGrid2.Cells[n - j, 0]);
      end;
    end
    else
      for n := 0 to zAnz - 1 do
      begin
        k := n;
        zFeld[n + 1] := StrToInt(StringGrid1.Cells[n, 0]);
      end;
  except
    if split and (k > j) then
      StringGrid2.col := k - j
    else
      StringGrid1.col := k;
    CanClose := False;
  end;
end;

procedure TZahlenfeldDlg.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_ZahlenfeldEingeben];
  BitBtnLoad.Caption := sprachTab[s_Laden];
  BitBtnSave.Caption := sprachTab[s_Speichern];
  BitBtnOk.Caption := sprachTab[s_OK];
  BitBtnCancel.Caption := sprachTab[s_Abbrechen];
end;

procedure TZahlenfeldDlg.FormResize(Sender: TObject);
begin
  stringGrid1.Width := stringGridW - 2 * stringGrid1.Left;
  stringGrid2.Width := stringGridW - 2 * stringGrid1.Left;
  BitBtnCancel.Left := clientWidth - 6 - BitBtnCancel.Width;
  BitBtnOk.left := BitBtnCancel.Left - 6 - BitBtnOk.Width;
end;

function TZahlenfeldDlg.GetAnzahl: integer;
begin
  GetAnzahl := zAnz;
end;

function TZahlenfeldDlg.GetHeap: integer;
begin
  GetHeap := heapGr;
end;

function TZahlenfeldDlg.GetSplit: boolean;
begin
  GetSplit := split;
end;

procedure TZahlenfeldDlg.GetZahlen(var zahlenfeld: TFeld);
var
  n: integer;
begin
  for n := 1 to zAnz do
    zahlenfeld[n] := zFeld[n];
end;

procedure TZahlenfeldDlg.SetAnzahl(n: integer);
begin
  zAnz := n;
  if split then
  begin
    stringGrid1.ColCount := n - (n div 2);
    stringGrid2.ColCount := n div 2;
  end
  else
    stringGrid1.ColCount := n;
end;

procedure TZahlenfeldDlg.SetHeap(n: integer);
begin
  heapGr := n;
end;

procedure TZahlenfeldDlg.SetSplit(aSplit: boolean);
begin
  if split = aSplit then
    exit;
  split := aSplit;
  stringGrid2.Visible := split;
  SetAnzahl(zAnz);
  VerteileZahlenFeld;
end;

procedure TZahlenfeldDlg.SetWeite(w: integer);
var
  k, j: integer;
begin
  if zAnz = 0 then
    exit;
  if split then
    j := zAnz - zAnz div 2
  else
    j := zAnz;
  k := j * (stringGrid1.DefaultColWidth + 1) + 1;
  if w > k + 22 then
    w := k + 22;
  stringGridW := w;
  k := 4 * (bitbtnOk.Width + 6) + 10;
  if w < k then
    w := k;
  Width := w;
end;

procedure TZahlenfeldDlg.SetZahlen(zahlenfeld: TFeld);
var
  n: integer;
begin
  feldDef := True;
  for n := 1 to zAnz do
    zFeld[n] := zahlenfeld[n];
  VerteileZahlenFeld;
end;

procedure TZahlenfeldDlg.VerteileZahlenFeld;
var
  n, k: integer;
begin
  if feldDef then
    if split then
    begin
      k := zAnz - zAnz div 2;
      for n := 1 to k do
        StringGrid1.Cells[n - 1, 0] := IntToStr(zFeld[n]);
      for n := k + 1 to zAnz do
        StringGrid2.Cells[n - k - 1, 0] := IntToStr(zFeld[n]);
    end
    else
      for n := 1 to zAnz do
        StringGrid1.Cells[n - 1, 0] := IntToStr(zFeld[n]);
end;

end.
