unit SortUnitKurveDlg;


interface

uses
  LCLIntf, LCLType,
  SysUtils, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons,
  SortUnitDefinitionen, SortUnitSprache, SortUnitAusgleich;

const
  mrTheorie = mrYesToAll;

type

  { TFormKurveDlg }

  TFormKurveDlg = class(TForm)
    LabelFunktion: TLabel;
    BitBtnJa: TBitBtn;
    BitBtnNein: TBitBtn;
    BitBtnTheorie: TBitBtn;
    LabelKurve: TLabel;
    LabelRegressHint: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure Init(sortart: TSortTyp; const s, fName: string);
  end;

var
  FormKurveDlg: TFormKurveDlg;

implementation

{$R *.lfm}

procedure TFormKurveDlg.Init(sortart: TSortTyp; const s, fName: string);
var
  t: string;
begin
  bitBtnTheorie.Caption := sprachTab[s_TheorieKurve];
  LabelRegressHint.Caption := regressHint;
  t := sortName[sortArt];
  if s <> '???' then
  begin
    Caption := sprachTab[s_Ausgleichskurve] + ' ' + t;
    LabelFunktion.Caption := fName + s;
    LabelKurve.Caption := sprachTab[s_KurveZeichnen];
    bitBtnJa.Caption := sprachTab[s_Ja];
    bitBtnNein.Caption := sprachTab[s_Nein];
  end
  else
  begin
    Caption := t;
    LabelFunktion.Caption := sprachTab[s_KeineKurve];
    LabelKurve.Visible := False;
    bitBtnJa.Caption := sprachTab[s_OK];
    bitBtnNein.Visible := False;
  end;
end;

end.
