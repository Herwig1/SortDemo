unit SortFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, StdCtrls, Buttons,
  SortUnitDefinitionen, SortUnitFarbwahl_Demo, SortUnitFarbwahl,
  SortUnitSprache;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    BitBtnFarbauswahl: TBitBtn;
    ButtonBinSort: TButton;
    ButtonBubble1Sort: TButton;
    ButtonBubble2Sort: TButton;
    ButtonCombSort: TButton;
    ButtonEinSort: TButton;
    ButtonFlashSort: TButton;
    ButtonHeapSort: TButton;
    ButtonMergeSort: TButton;
    ButtonMinSort: TButton;
    ButtonQuickSort: TButton;
    ButtonShakeSort: TButton;
    ButtonShellSort: TButton;
    PaintBoxSortButtons: TPaintBox;
    PanelSort: TPanel;
    PanelSortWahl: TPanel;
    procedure BitBtnFarbauswahlClick(Sender: TObject);
    procedure PaintBoxSortButtonsPaint(Sender: TObject);
  private
    fDemoSort: boolean;
    fpaintBox: TPaintBox;
  public

    constructor Create(aOwner: TComponent); override;
    procedure SortButtonsAktivieren(ja: boolean);
    procedure InitFrame(aDemoSort: boolean; aOwnerPaintBox: TPaintBox);
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.SortButtonsAktivieren(ja: boolean);
begin
  buttonMinSort.Enabled := ja;
  buttonEinSort.Enabled := ja;
  buttonBinSort.Enabled := ja;
  buttonBubble1Sort.Enabled := ja;
  buttonBubble2Sort.Enabled := ja;
  buttonShakeSort.Enabled := ja;
  buttonShellSort.Enabled := ja;
  buttonCombSort.Enabled := ja;
  buttonHeapSort.Enabled := ja;
  buttonQuickSort.Enabled := ja;
  buttonMergeSort.Enabled := ja;
  buttonFlashSort.Enabled := ja;
  BitBtnFarbauswahl.Enabled := ja;
end;

procedure TFrame1.BitBtnFarbauswahlClick(Sender: TObject);
var
  FarbwahlDlg: TFarbwahlDlg;
  FarbwahlDlg2: TFarbwahlDlg2;
begin
  if fDemoSort then
  begin
    FarbwahlDlg2 := TFarbwahlDlg2.Create(self.Owner);
    FarbwahlDlg2.ShowModal;
    FarbwahlDlg2.Free;
    FarbwahlDlg2 := nil;
  end
  else
  begin
    FarbwahlDlg := TFarbwahlDlg.Create(self);
    if FarbwahlDlg.ShowModal = mrOk then
    begin
      fpaintBox.repaint;
      PaintBoxSortButtonsPaint(Sender);
    end;
    FarbwahlDlg.Free;
    FarbwahlDlg := nil;
  end;
end;

constructor TFrame1.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  BitBtnFarbauswahl.Caption := sprachTab[s_Farbauswahl];
end;

procedure TFrame1.InitFrame(aDemoSort: boolean; aOwnerPaintBox: TPaintBox);
begin
  fDemoSort := aDemoSort;
  fpaintBox := aOwnerPaintBox;
end;

procedure TFrame1.PaintBoxSortButtonsPaint(Sender: TObject);
var
  sortArt: TSortTyp;
  x, y: integer;
  t, s: TButton;
begin
  if fDemoSort then exit;
  PaintBoxSortButtons.canvas.Pen.Width := 4;
  for sortArt := sMin to sLastSort do
  begin
    PaintBoxSortButtons.Canvas.Pen.color := SortFarb[sortArt];
    t := nil;
    s := nil;
    case sortArt of
      sMin: t := ButtonMinSort;
      sEin: t := ButtonEinSort;
      sBinEin: t := ButtonBinSort;
      sBubble1: t := ButtonBubble1Sort;
      sBubble2: t := ButtonBubble2Sort;
      sShake: t := ButtonShakeSort;

      sShell: s := ButtonShellSort;
      sComb: s := ButtonCombSort;
      sHeap: s := ButtonHeapSort;
      sQuick: s := ButtonQuickSort;
      sMerge: s := ButtonMergeSort;
      sFlash: s := ButtonFlashSort;
    end;
    if t <> nil then
    begin
      x := t.left - 4;
      y := t.top;
    end
    else
    begin
      x := s.left + S.Width + 2;
      y := s.top;
    end;
    PaintBoxSortButtons.Canvas.MoveTo(x, y);
    PaintBoxSortButtons.Canvas.LineTo(x, y + ButtonMinSort.Height);
  end;
end;

end.
