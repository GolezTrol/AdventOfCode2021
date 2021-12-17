unit Lib;

interface

uses
  WinApi.Windows, System.UITypes, Vcl.Graphics, SysUtils, Classes;

type TIntArray = TArray<Int64>;
type TStringArray = TArray<String>;

procedure ClearScreen;
procedure FgColor(AColor: TColor);
procedure CursorPos(const X, Y: SmallInt);
function ToIntArray(Strings: TStrings): TIntArray;
function Load(FileName: String): TIntArray;
function LoadStrings(FileName: String): TStringArray;
procedure Validate(Result: String; Expected: String);
procedure ValidateNr(Result: Int64; Expected: Int64);


implementation

procedure ClearScreen;
begin
  Write(#$1B'[2J');
end;

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

procedure CursorPos(const X, Y: SmallInt);
var
  Coord: TCoord;
begin
  Coord.X := X; Coord.Y := Y;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
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

function LoadStrings(FileName: String): TStringArray;
begin
  var Input := TStringList.Create;
  try
    Input.LoadFromFile(FileName);
    Result := Input.ToStringArray;
  finally
    Input.Free;
  end;
end;

procedure ValidateNr(Result: Int64; Expected: Int64);
begin
  if Result = Expected then
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
