unit SortUnitKostenDlg;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Dialog zur Abschätzung der "Kosten" durch Verschiebungen und Vergleiche
   Autor:   H. Niemeyer  (c) 2010 - 2015
   Version: 1.5

   letzte Änderung: 17.11.2015 *)

interface

uses SysUtils, Forms, StdCtrls, Buttons, ExtCtrls,
  SortUnitSprache;

type

  { TKostenDlg }

  TKostenDlg = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    LabelCerschieb: TLabel;
    LabelVgl: TLabel;
    LabelDPkt1: TLabel;
    LabelVerschieb: TLabel;
    EditVgl: TEdit;
    EditVerschieb: TEdit;
    LabelDPkt2: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure EditVerschiebChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    altfaktor, neu: single;
  public
    { Public declarations }
    function GetFaktor: single;
    procedure Start(aFaktor: single);
  end;

var
  KostenDlg: TKostenDlg;

implementation

{$R *.lfm}

procedure TKostenDlg.EditVerschiebChange(Sender: TObject);
begin
  try
    neu := StrToFloat(EditVerschieb.Text);
    if neu > 0 then
      BitBtnOk.Enabled := True;
  except
    neu := -1;
    BitBtnOk.Enabled := False;
  end;
end;

procedure TKostenDlg.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_KostenfaktorFestlegen];
  LabelVgl.Caption := sprachTab[s_Vergleiche];
  LabelVerschieb.Caption := sprachTab[s_Verschiebungen];
  BitBtnCancel.Caption := sprachTab[s_Abbrechen];
  BitBtnOK.Caption := sprachTab[s_OK];
end;

function TKostenDlg.GetFaktor: single;
begin
  if neu > 0 then
    Result := neu
  else
    Result := altFaktor;
end;

procedure TKostenDlg.Start(aFaktor: single);
begin
  altfaktor := aFaktor;
  EditVerschieb.Text := FloatToStr(altfaktor);
end;


end.
