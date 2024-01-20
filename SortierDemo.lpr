program SortierDemo;

uses
  Interfaces,
  Forms,
  SortUnitMain {SortFormMain},
  SortUnitDefinitionen in 'SortUnitDefinitionen.pas',
  SortUnitAusgleich in 'SortUnitAusgleich.pas',
  SortUnitLineDemo in 'SortUnitLineDemo.pas' {FormLineDemo},
  SortUnitAuswahlDlg in 'SortUnitAuswahlDlg.pas' {SortAuswahlDlg},
  SortUnitKostenDlg in 'SortUnitKostenDlg.pas' {KostenDlg},
  SortUnitListArtDlg in 'SortUnitListArtDlg.pas' {ListArtDlg},
  SortUnitAlgorithmen in 'SortUnitAlgorithmen.pas',
  SortZahlenfeldDlg in 'SortZahlenfeldDlg.pas' {ZahlenfeldDlg},
  SortUnitFarbwahl_Demo in 'SortUnitFarbwahl_Demo.pas' {FarbwahlDlg2},
  SortUnitMessungen {FormZeitvergleich},
  SortUnitBand in 'SortUnitBand.pas' {FormBandSort},
  SortUnitSprache in 'SortUnitSprache.pas',
  SortUnitFarbwahl in 'SortUnitFarbwahl.pas' {FarbwahlDlg},
  SortUnitKurveDlg in 'SortUnitKurveDlg.pas' {FormKurveDlg},
  SimpFunktion in 'SimpFunktion.pas' {FunktionEditor};

  {$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TSortFormMain, SortFormMain);
  Application.Run;
end.
