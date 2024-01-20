unit SortUnitMain;

(* Projekt: Sortier-Demo: Demonstration grundlegender Sortierverfahren
   Modul:   Hauptmodul des Projekts
   Autor:   H. Niemeyer  (c) 2010 - 2021, 2024
   Version: 1.8

   letzte Änderung: 20.01.2024 *)

interface

uses
  SysUtils, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus,
  SortUnitSprache, SortUnitDefinitionen,
  SortUnitAni, sortunitMessungen, SortUnitLineDemo, SortUnitBand;

const
  infoStr = 'Copyright by H. Niemeyer 17. Januar 2024';
  infoStrLen = Length(infoStr);
  versionNr = ' 1.8.0';

type

  { TSortFormMain }

  TSortFormMain = class(TForm)
    PanelCommand: TPanel;
    PanelUnten: TPanel;
    BitBtnProgrammEnde: TBitBtn;
    MainMenu1: TMainMenu;
    Lineare1: TMenuItem;
    DemoStbe1: TMenuItem;
    Bandsortierung1: TMenuItem;
    DemovonSortierungen1: TMenuItem;
    Zeitmessungen1: TMenuItem;
    AnzahlderVergleiche1: TMenuItem;
    AnzahlderVerschiebungen1: TMenuItem;
    Kosten1: TMenuItem;
    N1: TMenuItem;
    Beenden1: TMenuItem;
    ImageList1: TImageList;
    PanelLeft: TPanel;
    BitBtnBandSort: TBitBtn;
    BitBtnSortTimes: TBitBtn;
    BitBtnSortVerschieb: TBitBtn;
    BitBtnSortLines: TBitBtn;
    BitBtnSortVergleiche: TBitBtn;
    BitBtnSortKosten: TBitBtn;
    BitBtnAnimationen: TBitBtn;
    PanelVersion: TPanel;
    LabelVersion: TLabel;
    LabelCopyRight: TLabel;
    Bevel1: TBevel;
    procedure Beenden1Click(Sender: TObject);
    procedure BitBtnAnimationenClick(Sender: TObject);
    procedure BitBtnBandSortClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BitBtnMessungenClick(Sender: TObject);
    procedure BitBtnSortLinesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelVersionResize(Sender: TObject);
  private
    { Private-Deklarationen }
    aktAniPos: integer;
    mixStr: string[infoStrLen];
    counter: longint;
    aniFeld: array[1..infoStrLen] of integer;
    procedure HaltIdleHandler(ja: boolean);
    procedure MakeRandomString;
    procedure MyIdleHandler(Sender: TObject; var Done: boolean);
  public
    { Public-Deklarationen }
  end;

var
  SortFormMain: TSortFormMain;

implementation

{$R *.lfm}

const
  warteZeit = 300;
  halteZeit = 10 * warteZeit;

procedure TSortFormMain.Beenden1Click(Sender: TObject);
begin
  Close;
end;

procedure TSortFormMain.BitBtnAnimationenClick(Sender: TObject);
begin
  HaltIdleHandler(True);
  SortFormAni := TSortFormAni.Create(self);
  SortFormAni.Showmodal;
  SortFormAni.Free;
  HaltIdleHandler(False);
end;

procedure TSortFormMain.BitBtnBandSortClick(Sender: TObject);
begin
  HaltIdleHandler(True);
  application.OnIdle := nil;
  FormBandSort := TFormBandSort.Create(self);
  FormBandSort.Showmodal;
  FormBandSort.Free;
  HaltIdleHandler(False);
end;

procedure TSortFormMain.BitBtnMessungenClick(Sender: TObject);
begin
  HaltIdleHandler(True);
  FormMessung := TFormMessung.Create(self);
  FormMessung.SetVergleichsArt(TBitBtn(Sender).tag);
  FormMessung.Showmodal;
  FormMessung.Free;
  HaltIdleHandler(False);
end;

procedure TSortFormMain.BitBtnSortLinesClick(Sender: TObject);
begin
  HaltIdleHandler(True);
  FormLineDemo := TFormLineDemo.Create(self);
  FormLineDemo.ShowModal;
  FormLineDemo.Free;
  HaltIdleHandler(False);
end;

procedure TSortFormMain.FormActivate(Sender: TObject);
begin
  SetShellSteps(15, 3, 1);
  HaltIdleHandler(False);
end;

procedure TSortFormMain.FormCreate(Sender: TObject);
begin
  SortFormMain.Caption := sprachTab[s_SortierDemo];
  LabelVersion.Caption := sprachTab[s_Version] + versionNr;
  Lineare1.Caption := sprachTab[s_SortierungLinListen];
  DemovonSortierungen1.Caption := sprachTab[s_DemoSortierverfahren];
  Zeitmessungen1.Caption := sprachTab[s_Zeitmessungen];
  AnzahlderVergleiche1.Caption := sprachTab[s_AnzahlVergleiche];
  AnzahlderVerschiebungen1.Caption := sprachTab[s_AnzahlVerschiebungen];
  Kosten1.Caption := sprachTab[s_Kosten];
  Beenden1.Caption := sprachTab[s_ProgrammBeenden];
  DemoStbe1.Caption := sprachTab[s_SortierungStab];
  Bandsortierung1.Caption := sprachTab[s_SortierungBand];
  BitBtnAnimationen.Caption := sprachTab[s_DemoSortieralgorithmen];
  BitBtnSortTimes.Caption := sprachTab[s_VergleichZeiten];
  BitBtnSortVerschieb.Caption := sprachTab[s_BestimmeVerschiebungen];
  BitBtnSortVergleiche.Caption := sprachTab[s_BestimmeVergleiche];
  BitBtnSortKosten.Caption := sprachTab[s_BestimmeKosten];
  BitBtnSortLines.Caption := sprachTab[s_SortierungStab];
  BitBtnBandSort.Caption := sprachTab[s_BeispieBandsortierung];
  BitBtnProgrammEnde.Caption := sprachTab[s_ProgrammBeenden];
end;

procedure TSortFormMain.HaltIdleHandler(ja: boolean);
begin
  if ja then
  begin
    application.OnIdle := nil;
    hide;
  end
  else
  begin
    application.OnIdle := @MyIdleHandler;
    Show;
  end;
end;

procedure TSortFormMain.MakeRandomString;
var
  k, n: integer;
  c: ansichar;
begin
  mixStr := infoStr;
  for k := infoStrLen downto 1 do
  begin
    n := random(infoStrLen) + 1;
    aniFeld[k] := n;
    c := mixStr[n];
    mixStr[n] := mixStr[k];
    mixStr[k] := c;
  end;
  aktAniPos := 1;
  LabelCopyRight.font.Color := clBlack;
  LabelCopyRight.Caption := mixStr;
  LabelCopyRight.repaint;
  counter := GetTickCount64;
end;

procedure TSortFormMain.MyIdleHandler(Sender: TObject; var Done: boolean);
var
  diff: longint;
  c: ansichar;
  n: integer;
begin
  done := False;
  diff := GetTickCount64 - counter;
  if diff > HalteZeit then
    MakeRandomString
  else if (aktAniPos <= infoStrLen) and (diff > WarteZeit) then
  begin
    n := aniFeld[aktaniPos];
    c := mixStr[n];
    mixStr[n] := mixStr[aktaniPos];
    mixStr[aktaniPos] := c;
    aktAniPos := aktAniPos + 1;
    if aktAniPos > infoStrLen then
      LabelCopyRight.font.Color := clRed;
    LabelCopyRight.Caption := mixStr;
    LabelCopyRight.repaint;
    counter := GetTickCount64;
  end;
end;

procedure TSortFormMain.PanelVersionResize(Sender: TObject);
var
  w, h, h1, h2: integer;
begin
  w := PanelVersion.clientWidth - 12;
  h := (PanelVersion.ClientHeight div 2) - 4;
  LabelVersion.Top := 0;
  LabelVersion.left := 6;
  LabelVersion.Height := h;
  LabelVersion.Width := w;
  LabelCopyRight.Top := h + 8;
  LabelCopyRight.Left := 6;
  LabelCopyRight.Height := h;
  LabelCopyRight.Width := w;
  h1 := GetFontSizeFromPixels(LabelVersion.Caption, w, h, LabelVersion.Font);
  h2 := GetFontSizeFromPixels(infoStr, w, h, LabelCopyRight.Font);
  LabelCopyRight.Font.Height := h2;
  if h2 < h1 then LabelVersion.Font.Height := (h1 + h2) div 2
  else
    LabelVersion.Font.Height := h1;
end;

end.
