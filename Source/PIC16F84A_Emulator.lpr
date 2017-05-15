program PIC16F84A_Emulator;

uses
  //LazUTF8,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, ProcessorDefine, PeripheralFormDefine, LEDDefine, PinDefine, ProcessorFormDefine, LEDArraySettingsForm,
  PeripheralButtonPanel, ButtonPanelDefine
  { you can add units after this };

{$R *.res}

begin
  {$IFDEF DEBUG}
  GlobalSkipIfNoLeaks := True;
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmLEDArraySettings, frmLEDArraySettings);
  Application.Run;
end.

