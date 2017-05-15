unit ButtonPanelDefine;

interface

uses
  Lists, PinDefine, Controls, Classes, Graphics, Delegates;

type

  { TClickButton }

  TClickButton = class
  public
    type

      TMode = (bmHold, bmMonostable, bmToggle);

  private
    FPin: TPin;
    FControl: TCustomControl;

    FLeft: Integer;
    FTop: Integer;
    FWidth: Cardinal;
    FHeight: Cardinal;

    FMode: TMode;
    FInverted: Boolean;

    FHovered: Boolean;
    FClicked: Boolean;

    function GetBounds: TRect;
    procedure SetClicked(AValue: Boolean);

    procedure SetHeight(AValue: Cardinal);
    procedure SetHovered(AValue: Boolean);
    procedure SetInverted(AValue: Boolean);
    procedure SetLeft(AValue: Integer);
    procedure SetMode(AValue: TMode);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Cardinal);

    procedure InitDrawFace;
    procedure InitDrawEdge(ALight: Boolean);

    procedure NotifyChanges;

  public
    constructor Create(AControl: TCustomControl; APin: TPin); virtual;

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Cardinal read FWidth write SetWidth;
    property Height: Cardinal read FHeight write SetHeight;

    property Mode: TMode read FMode write SetMode;
    property Inverted: Boolean read FInverted write SetInverted;

    property Clicked: Boolean read FClicked write SetClicked;
    property Hovered: Boolean read FHovered write SetHovered;

    property Bounds: TRect read GetBounds;

    procedure Draw;

  end;

  { TButtonPanel }

  TButtonPanel = class
  public
    type

      TButtonCount = 1 .. 10;

  public

    type TButtonPanelNotifyEvent = procedure (Sender: TButtonPanel) of object; var
      // The width of the Button-Panel changed
      OnWidthChange: TDelegate1<TButtonPanelNotifyEvent>;
      // The height of the Button-Panel changed
      OnHeightChange: TDelegate1<TButtonPanelNotifyEvent>;

  private
    FButtonHeight: Integer;
    FButtonInverted: Boolean;
    FButtonMode: TClickButton.TMode;
    FButtonWidth: Integer;
    FControl: TCustomControl;

    FPinArray: TPinArray;

    FButtons: TObjectArray<TClickButton>;
    FReverseOrder: Boolean;

    FUpdateCounter: Cardinal;
    FTriedGenerate: Boolean;

    FHoveredButton: TClickButton;
    FClickedButton: TClickButton;

    function GetButtonCount: TButtonCount;
    function GetDisplayHeight: Integer;
    function GetDisplayWidth: Integer;
    procedure SetButtonCount(AValue: TButtonCount);
    procedure SetButtonHeight(AValue: Integer);
    procedure SetButtonInverted(AValue: Boolean);
    procedure SetButtonMode(AValue: TClickButton.TMode);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetReverseOrder(AValue: Boolean);

    function FindButton(X, Y: Integer): TClickButton;

    procedure GenerateButtons;

  public
    constructor Create(AControl: TCustomControl; APinArray: TPinArray);
    destructor Destroy; override;

    procedure MouseDown(X, Y: Integer; AButton: TMouseButton);
    procedure MouseMove(X, Y: Integer);
    procedure MouseUp(X, Y: Integer; AButton: TMouseButton);
    procedure MouseLeave;

    procedure Draw;

    procedure BeginUpdate;
    procedure EndUpdate;

    property PinArray: TPinArray read FPinArray;

    property ButtonCount: TButtonCount read GetButtonCount write SetButtonCount;

    property ButtonMode: TClickButton.TMode read FButtonMode write SetButtonMode;
    property ButtonInverted: Boolean read FButtonInverted write SetButtonInverted;

    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;

    property DisplayWidth: Integer read GetDisplayWidth;
    property DisplayHeight: Integer read GetDisplayHeight;

    property ReverseOrder: Boolean read FReverseOrder write SetReverseOrder;

  end;

implementation

{ TClickButton }

procedure TClickButton.SetHeight(AValue: Cardinal);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  NotifyChanges;
end;

procedure TClickButton.SetHovered(AValue: Boolean);
begin
  if FHovered = AValue then
    Exit;
  FHovered := AValue;
  NotifyChanges;
end;

function TClickButton.GetBounds: TRect;
begin
  Result := TRect.Create(Point(Left, Top), Width, Height);
end;

procedure TClickButton.SetClicked(AValue: Boolean);
begin
  if FClicked = AValue then
    Exit;
  FClicked := AValue;
  NotifyChanges;
  case Mode of
    bmHold:
      FPin.State := AValue <> Inverted;
    bmMonostable:
    if Clicked then
    begin
      FPin.State := not Inverted;
      FPin.State := Inverted;
    end;
    bmToggle:
    if Clicked then
      FPin.State := not FPin.State;
  end;

end;

procedure TClickButton.SetInverted(AValue: Boolean);
begin
  if FInverted = AValue then
    Exit;
  FInverted := AValue;
  NotifyChanges;
  FPin.State := AValue <> Inverted;
end;

procedure TClickButton.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
  NotifyChanges;
end;

procedure TClickButton.SetMode(AValue: TMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
end;

procedure TClickButton.SetTop(AValue: Integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;
  NotifyChanges;
end;

procedure TClickButton.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  NotifyChanges;
end;

procedure TClickButton.InitDrawFace;
begin
  FControl.Canvas.Pen.Style := psClear;
  FControl.Canvas.Brush.Style := bsSolid;
  if Hovered then
    FControl.Canvas.Brush.Color := $33DD33
  else
    FControl.Canvas.Brush.Color := $22AA22;
end;

procedure TClickButton.InitDrawEdge(ALight: Boolean);
begin
  FControl.Canvas.Pen.Style := psSolid;
  FControl.Canvas.Pen.Width := 5;
  if ALight then
    FControl.Canvas.Pen.Color := $33FF44
  else
    FControl.Canvas.Pen.Color := $117711;
end;

procedure TClickButton.NotifyChanges;
begin
  FControl.Invalidate;
end;

constructor TClickButton.Create(AControl: TCustomControl; APin: TPin);
begin
  FControl := AControl;
  FPin := APin;
  FPin.Direction := pdWrite;
end;

procedure TClickButton.Draw;
begin
  InitDrawFace;
  FControl.Canvas.Rectangle(TRect.Create(Point(Left, Top), Width, Height));

  InitDrawEdge(Clicked <> Inverted);
  FControl.Canvas.MoveTo(Left, Top + Height);
  FControl.Canvas.LineTo(Left + Width, Top + Height);
  FControl.Canvas.LineTo(Left + Width, Top);

  InitDrawEdge(Clicked = Inverted);
  FControl.Canvas.MoveTo(Left, Top + Height);
  FControl.Canvas.LineTo(Left, Top);
  FControl.Canvas.LineTo(Left + Width, Top);
end;

{ TButtonPanel }

procedure TButtonPanel.GenerateButtons;
var
  Button: TClickButton;
  I: Integer;
begin
  if FUpdateCounter > 0 then
  begin
    FTriedGenerate := True;
    Exit;
  end;
  FButtons.DelAll;
  for I := 0 to ButtonCount - 1 do
  begin
    Button := TClickButton.Create(FControl, FPinArray.Pins[I]);
    if ReverseOrder then
      Button.Left := 5 + I * (ButtonWidth + 10)
    else
      Button.Left := 5 + (ButtonCount - 1 - I) * (ButtonWidth + 10);
    Button.Top := 5;
    Button.Width := ButtonWidth;
    Button.Height := ButtonHeight;
    Button.Mode := ButtonMode;
    Button.Inverted := ButtonInverted;
    FButtons.Add(Button);
  end;
end;

function TButtonPanel.GetButtonCount: TButtonCount;
begin
  Result := FPinArray.Count;
end;

function TButtonPanel.GetDisplayHeight: Integer;
begin
  Result := ButtonHeight + 10;
end;

function TButtonPanel.GetDisplayWidth: Integer;
begin
  Result := (ButtonWidth + 10) * ButtonCount;
end;

procedure TButtonPanel.SetButtonCount(AValue: TButtonCount);
begin
  if ButtonCount = AValue then
    Exit;
  FPinArray.Count := AValue;
  GenerateButtons;
end;

procedure TButtonPanel.SetButtonHeight(AValue: Integer);
begin
  if FButtonHeight = AValue then
    Exit;
  FButtonHeight := AValue;
  GenerateButtons;
end;

procedure TButtonPanel.SetButtonInverted(AValue: Boolean);
var
  Button: TClickButton;
begin
  if FButtonInverted = AValue then
    Exit;
  FButtonInverted := AValue;
  for Button in FButtons do
    Button.Inverted := AValue;
end;

procedure TButtonPanel.SetButtonMode(AValue: TClickButton.TMode);
var
  Button: TClickButton;
begin
  if FButtonMode = AValue then
    Exit;
  FButtonMode := AValue;
  for Button in FButtons do
    Button.Mode := AValue;
end;

procedure TButtonPanel.SetButtonWidth(AValue: Integer);
begin
  if FButtonWidth = AValue then
    Exit;
  FButtonWidth := AValue;
  GenerateButtons;
end;

procedure TButtonPanel.SetReverseOrder(AValue: Boolean);
begin
  if FReverseOrder = AValue then
    Exit;
  FReverseOrder := AValue;
  GenerateButtons;
end;

function TButtonPanel.FindButton(X, Y: Integer): TClickButton;
var
  Button: TClickButton;
begin
  for Button in FButtons do
    if Button.Bounds.Contains(Point(X, Y)) then
      Exit(Button);
  Result := nil;
end;

constructor TButtonPanel.Create(AControl: TCustomControl; APinArray: TPinArray);
begin
  FControl := AControl;
  FPinArray := APinArray;
  FButtons := TObjectArray<TClickButton>.Create;
  FButtonHeight := 50;
  FButtonWidth := 50;
  FButtonMode := bmHold;
  ButtonCount := 4;
end;

destructor TButtonPanel.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

procedure TButtonPanel.MouseDown(X, Y: Integer; AButton: TMouseButton);
begin
  FClickedButton := FindButton(X, Y);
  if Assigned(FClickedButton) then
    FClickedButton.Clicked := True;
end;

procedure TButtonPanel.MouseMove(X, Y: Integer);
var
  Button: TClickButton;
begin
  Button := FindButton(X, Y);
  if FHoveredButton <> Button then
  begin
    if Assigned(FHoveredButton) then
      FHoveredButton.Hovered := False;
    if Assigned(Button) then
      Button.Hovered := True;
    FHoveredButton := Button;
  end;
end;

procedure TButtonPanel.MouseUp(X, Y: Integer; AButton: TMouseButton);
begin
  if Assigned(FClickedButton) then
  begin
    FClickedButton.Clicked := False;
    FClickedButton := nil;
  end;
end;

procedure TButtonPanel.MouseLeave;
begin
  if Assigned(FHoveredButton) then
  begin
    FHoveredButton.Hovered := False;
    FHoveredButton := nil;
  end;
end;

procedure TButtonPanel.Draw;
var
  Button: TClickButton;
begin
  for Button in FButtons do
    Button.Draw;
end;

procedure TButtonPanel.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TButtonPanel.EndUpdate;
begin
  Dec(FUpdateCounter);
  if (FUpdateCounter = 0) and FTriedGenerate then
  begin
    FTriedGenerate := False;
    GenerateButtons;
  end;
end;

end.

