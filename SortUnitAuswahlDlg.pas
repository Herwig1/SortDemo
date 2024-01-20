unit SortUnitAuswahlDlg;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Auswahldialog zu den Sortierverfahren
   Autor:   H. Niemeyer  (c) 2010 - 2015, 2024
   Version: 1.5.1

   letzte Änderung: 12.01.2024 *)

interface

uses SysUtils, Graphics, Forms, Controls,
  Buttons, ExtCtrls,
  SortUnitDefinitionen, SortUnitSprache, Classes;

type

  { TSortAuswahlDlg }

  TSortAuswahlDlg = class(TForm)
    PanelCommand: TPanel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    BitBtnAlle: TBitBtn;
    BitBtnKeiner: TBitBtn;
    CheckGroup1: TCheckGroup;
    procedure FormResize(Sender: TObject);
    procedure BitBtnAlleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Vorbereiten(auswahl: TSortierverfahren);
    procedure NeueAuswahl(var auswahl: TSortierverfahren);
  end;

var
  SortAuswahlDlg: TSortAuswahlDlg;

implementation

{$R *.lfm}

procedure TSortAuswahlDlg.BitBtnAlleClick(Sender: TObject);
var
  sortArt: TSortTyp;
begin
  for sortArt := sMin to sLastSort do
    CheckGroup1.Checked[Ord(sortArt)] := ((Sender as TBitBtn).Tag = 0);//True;
end;

procedure TSortAuswahlDlg.FormCreate(Sender: TObject);
begin
  self.Caption := sprachTab[s_SortierverfahrenWahl];
  BitBtnAlle.Caption := sprachTab[s_alleMarkieren];
  BitBtnKeiner.Caption := sprachTab[s_MarkierungLoeschen];
  BitBtnOk.Caption := sprachTab[s_Ok];
  BitBtnCancel.Caption := sprachTab[s_Abbrechen];
end;

procedure TSortAuswahlDlg.FormResize(Sender: TObject);
begin
  BitBtnOk.left := (panelCommand.clientWidth - BitBtnOk.Width) div 2;
  BitBtnCancel.Left := bitBtnOk.left;
end;

procedure TSortAuswahlDlg.NeueAuswahl(var auswahl: TSortierverfahren);
var
  sortArt: TSortTyp;
begin
  auswahl := [];
  for sortArt := sMin to sLastSort do
    if CheckGroup1.Checked[Ord(sortArt)] then
      auswahl := auswahl + [sortArt];
end;

procedure TSortAuswahlDlg.Vorbereiten(auswahl: TSortierverfahren);
var
  sortArt: TSortTyp;
begin
  for sortArt := sMin to sLastSort do
    CheckGroup1.Checked[Ord(sortArt)] := (sortArt in auswahl);
  CheckGroup1.Enabled := True;
end;


end.
