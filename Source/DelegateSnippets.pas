{$IF DEFINED(HEADER)}
  {$IF     DEFINED(DELEGATE8)}TDelegate8
  {$ELSEIF DEFINED(DELEGATE7)}TDelegate7
  {$ELSEIF DEFINED(DELEGATE6)}TDelegate6
  {$ELSEIF DEFINED(DELEGATE5)}TDelegate5
  {$ELSEIF DEFINED(DELEGATE4)}TDelegate4
  {$ELSEIF DEFINED(DELEGATE3)}TDelegate3
  {$ELSEIF DEFINED(DELEGATE2)}TDelegate2
  {$ELSEIF DEFINED(DELEGATE1)}TDelegate1
  {$ENDIF}
{$ELSEIF DEFINED(PARAMLIST)}
   {$IFDEF DELEGATE1} E1{$ENDIF}
   {$IFDEF DELEGATE2},E2{$ENDIF}
   {$IFDEF DELEGATE3},E3{$ENDIF}
   {$IFDEF DELEGATE4},E4{$ENDIF}
   {$IFDEF DELEGATE5},E5{$ENDIF}
   {$IFDEF DELEGATE6},E6{$ENDIF}
   {$IFDEF DELEGATE7},E7{$ENDIF}
   {$IFDEF DELEGATE8},E8{$ENDIF}
{$ENDIF}
