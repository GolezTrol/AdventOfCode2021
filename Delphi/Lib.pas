unit Lib;

interface

uses
  WinApi.Windows, System.UITypes, Vcl.Graphics, SysUtils, Classes;

type TIntArray = TArray<Int64>;

procedure FgColor(AColor: TColor);
function ToIntArray(Strings: TStrings): TIntArray;
function Load(FileName: String): TIntArray;
procedure Validate(Result: String; Expected: String);

implementation

procedure FgColor(AColor: TColor);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
  case AColor of
    clWhite: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY);
    clRed: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_RED or FOREGROUND_INTENSITY);
    clGreen: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_GREEN or FOREGROUND_INTENSITY);
    clBlue: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_BLUE or FOREGROUND_INTENSITY);
    clMaroon: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
    clPurple: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY);
    clAqua: SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY);
  end;
end;

function ToIntArray(Strings: TStrings): TIntArray;
begin
  SetLength(Result, Strings.Count);
  for var i := Low(Result) to High(Result) do
    Result[i] := Strings[i].ToInt64;
end;

function Load(FileName: String): TIntArray;
begin
  var Input := TStringList.Create;
  try
    Input.LoadFromFile(FileName);
    Result := ToIntArray(Input);
  finally
    Input.Free;
  end;
end;

procedure Validate(Result: String; Expected: String);
begin
  if Expected = '' then
  begin
    FgColor(clWhite);
    WriteLn('Result is ', Result)
  end
  else if Result = Expected then
  begin
    FgColor(clGreen);
    WriteLn('Result is ', Result, ' as expected')
  end
  else
  begin
    FgColor(clRed);
    WriteLn('Expected ', Expected, ', got ', Result);
  end;

  FgColor(clDefault);
end;

end.
