unit SortUnitListArtDlg;

 (* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Dialog zur Einstellung der Listenart: sortiert, umgekehrt sortiert,
            zufällig mit Dopplungen oder zufällig ohne Dopplungen
   Autor:   H. Niemeyer  (c) 2010 - 2015
   Version: 1.4

   letzte Änderung: 30.10.2015 *)

interface

uses SysUtils, Forms, Buttons, ExtCtrls,
  SortUnitDefinitionen, SortUnitSprache;

type
  TListArtDlg = class(TForm)
    RadioGroup1: TRadioGroup;

    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetListArt: TListenTyp;
    procedure SetMarkierung(listArt: TListenTyp);
  end;

var
  ListArtDlg: TListArtDlg;

implementation

{$R *.lfm}

procedure TListArtDlg.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_AuswahListenart];
  RadioGroup1.Items[0] := sprachTab[s_normal];
  RadioGroup1.Items[1] := sprachTab[s_ohneDoppel];
  RadioGroup1.Items[2] := sprachTab[s_sortiert];
  RadioGroup1.Items[3] := sprachTab[s_umgekehrt];
  BitBtnOk.Caption := sprachTab[s_OK];
  BitBtnCancel.Caption := sprachTab[s_Abbrechen];
end;

function TListArtDlg.GetListArt: TListenTyp;
begin
  Result := TListenTyp(radiogroup1.ItemIndex);
end;

procedure TListArtDlg.SetMarkierung(listArt: TListenTyp);
begin
  radiogroup1.ItemIndex := Ord(listArt);
end;


end.
