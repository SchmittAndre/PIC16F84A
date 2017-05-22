unit EmulationSettingsForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ProcessorDefine, Math;

type

  { TfrmEmulationSettings }

  TfrmEmulationSettings = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtCrystalFrequency: TEdit;
    cbWatchDogEnabled: TCheckBox;
    edtOperationTime: TEdit;
    edtOperationFrequency: TEdit;
    lbCrystalFrequency: TLabel;
    lbwdte: TLabel;
    lbOperationTime: TLabel;
    lbOperationFrequency: TLabel;
    pnlCrystalFrequency: TPanel;
    pnlCrystalFrequency1: TPanel;
    pnlOperationTime: TPanel;
    pnlOperationFrequency: TPanel;
    pnlSettingsArea: TPanel;
    pnlControl: TPanel;
    procedure edtOperationFrequencyEditingDone(Sender: TObject);
    procedure edtOperationTimeEditingDone(Sender: TObject);
    procedure edtCrystalFrequencyEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    procedure SetCrystalFrequency(AValue: Single);

    procedure LoadEmulationSettings(AEmulationSettings: TEmulationSettings);
    procedure SaveEmulationSettings(AEmulationSettings: TEmulationSettings);

    function FormatFrequency(AFreqeuncy: Single): String;
    function FormatTime(ATime: Single): String;
    function ParseFrequency(AFrequency: String): Single;
    function ParseTime(ATime: String): Single;

  public
    function Execute(AEmulationSettings: TEmulationSettings): Boolean;

  end;

  TFrequencyFormat = (
    ffHz,
    ffkHz,
    ffMHz
  );

  TTimeFormat = (
    tfSeconds,
    tfMilliseconds,
    tfMikroSeconds,
    tfNanoSeconds
  );

const

  FrequncyFormatStrings: array [TFrequencyFormat] of String = (
    'Hz',
    'kHz',
    'MHz'
  );

  TimeFormatStrings: array [TTimeFormat] of String = (
    'sec',
    'ms',
    'µs',
    'ns'
  );

var
  frmEmulationSettings: TfrmEmulationSettings;

implementation

{$R *.lfm}

{ TfrmEmulationSettings }

procedure TfrmEmulationSettings.FormCreate(Sender: TObject);
begin
  AutoSize := True;
end;

procedure TfrmEmulationSettings.SetCrystalFrequency(AValue: Single);
begin
  edtCrystalFrequency.Text := FormatFrequency(AValue);
  edtOperationFrequency.Text := FormatFrequency(AValue / 4);
  edtOperationTime.Text := FormatTime(4 / AValue);
end;

procedure TfrmEmulationSettings.edtCrystalFrequencyEditingDone(Sender: TObject);
begin
  try
    SetCrystalFrequency(ParseFrequency(edtCrystalFrequency.Text));
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmEmulationSettings.edtOperationFrequencyEditingDone(Sender: TObject);
begin
  try
    SetCrystalFrequency(ParseFrequency(edtOperationFrequency.Text) * 4);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmEmulationSettings.edtOperationTimeEditingDone(Sender: TObject);
begin
  try
    SetCrystalFrequency(4 / ParseTime(edtOperationTime.Text));
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmEmulationSettings.LoadEmulationSettings(AEmulationSettings: TEmulationSettings);
begin
  try
    SetCrystalFrequency(AEmulationSettings.CrystalFrequency);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  cbWatchDogEnabled.Checked := AEmulationSettings.WatchDogEnabled;
end;

procedure TfrmEmulationSettings.SaveEmulationSettings(AEmulationSettings: TEmulationSettings);
begin
  AEmulationSettings.CrystalFrequency := ParseFrequency(edtCrystalFrequency.Text);
  AEmulationSettings.WatchDogEnabled := cbWatchDogEnabled.Checked;
end;

function TfrmEmulationSettings.FormatFrequency(AFreqeuncy: Single): String;
var
  Fmt: TFrequencyFormat;
begin
  Fmt := Low(Fmt);
  while (AFreqeuncy >= 1000) and (Fmt <> High(Fmt)) do
  begin
    AFreqeuncy := AFreqeuncy / 1000;
    Inc(Fmt);
  end;
  Result := Format('%g %s', [AFreqeuncy, FrequncyFormatStrings[Fmt]]);
end;

function TfrmEmulationSettings.FormatTime(ATime: Single): String;
var
  Fmt: TTimeFormat;
begin
  Fmt := Low(Fmt);
  while (ATime < 1) and (Fmt <> High(Fmt)) do
  begin
    ATime := ATime * 1000;
    Inc(Fmt);
  end;
  Result := Format('%.3f %s', [ATime, TimeFormatStrings[Fmt]]);
end;

function TfrmEmulationSettings.ParseFrequency(AFrequency: String): Single;
var
  Fmt: TFrequencyFormat;
begin
  for Fmt := High(Fmt) downto Low(Fmt) do
  begin
    if AFrequency.EndsWith(FrequncyFormatStrings[Fmt], True) then
    begin
      SetLength(AFrequency, Length(AFrequency) - FrequncyFormatStrings[Fmt].Length);
      AFrequency := AFrequency.Trim;
      Exit(StrToFloat(AFrequency) * Power(1000, Ord(Fmt)));
    end;
  end;
  raise Exception.Create('Please specify a valid type! (Hz, kHz, MHz)');
end;

function TfrmEmulationSettings.ParseTime(ATime: String): Single;
var
  Fmt: TTimeFormat;
begin
  for Fmt := High(Fmt) downto Low(Fmt) do
  begin
    if ATime.EndsWith(TimeFormatStrings[Fmt], True) then
    begin
      SetLength(ATime, Length(ATime) - TimeFormatStrings[Fmt].Length);
      ATime := ATime.Trim;
      Exit(StrToFloat(ATime) / Power(1000, Ord(Fmt)));
    end;
  end;
  raise Exception.Create('Please specify a valid type! (sec, ms, µs, ns)');
end;

function TfrmEmulationSettings.Execute(AEmulationSettings: TEmulationSettings): Boolean;
var
  CanExit: Boolean;
begin
  LoadEmulationSettings(AEmulationSettings);
  repeat
    CanExit := True;
    Result := ShowModal = mrOK;
    if Result then
      try
        SaveEmulationSettings(AEmulationSettings);

      except
        on E: Exception do
        begin
          MessageDlg('Error', E.Message, mtError, [mbOK], 0);
          CanExit := False;
        end;
      end;
  until CanExit;
end;

end.

