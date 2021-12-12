program Day8;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  for var Input in Inputs do
  begin
    var Raw := Input.Split([' | ']);
    var Displays := Raw[1].Split([' ']);
    for var Display in Displays do
      if Length(Display) in [2,3,4,7] then
        Inc(Result);
  end;
end;

// The correct and normalized signals for each of the digits.
// This is what the outputs should be translated to.
const Signals: array[0..9] of string = (
  // Code     // Mask            // D  Length
  'abcefg',   // 'abc efg',      // 0  6
  'cf',       // '  c  f ',      // 1  2  aaa
  'acdeg',    // 'a cde g',      // 2  5 b   c
  'acdfg',    // 'a cd fg',      // 3  5  ddd
  'bcdf',     // ' bcd f ',      // 4  4 e   f
  'abdfg',    // 'ab d fg',      // 5  5  ggg
  'abdefg',   // 'ab defg',      // 6  6
  'acf',      // 'a c  f ',      // 7  3
  'abcdefg',  // 'abcdefg',      // 8  7
  'abcdfg');  // 'abcd fg');     // 9  6

function Diff(const Larger, Smaller: String): String;
begin
  // Returns a string with the characters in Larger that do not exist in Smaller.
  for var c in Larger do
    if Pos(c, Smaller) = 0 then
      Result := Result + c;
end;

function IsInAll(const C: Char; All: TStringArray): Boolean;
begin
  // Returns if the given character occurs in all of the strings
  for var One in All do
    if Pos(C, One) = 0 then
      Exit(False);

  Exit(True);
end;

// Digit:  1, 7, 4, 2, 3, 5, 0, 6, 9, 8
// Length: 2, 3, 4, 5, 5, 5, 6, 6, 6, 8
function FindMapping(const SortedSignals: TStringArray): String;
begin
  Result := StringOfChar(' ', 10);
  var Chars := '';
  // The one of 7 that's not in 1, is a.
  var cf := SortedSignals[0]; // digit 1
  var acf := SortedSignals[1]; // digit 7
  var a := Diff(acf, cf);
  Assert(Length(a) = 1, 'Expected 1 char diff between 1 and 7');
  Result[1] := a[1]; // a

  // The two of 4 that are not in 1 are b and d
  var bd := Diff(SortedSignals[2], cf);
  Assert(Length(bd) = 2, 'Expected 2 char diff between 4 and 1');
  // The one of bd that is in all 5-long signals, is d. The other is b.
  if IsInAll(bd[1], Copy(SortedSignals, 3, 3)) then
  begin
    Result[4] := bd[1]; // d
    Result[2] := bd[2]; // b
  end
  else
  begin
    Result[2] := bd[1]; // b
    Result[4] := bd[2]; // d
  end;

  // The 8 minus abcdf (= bd + 7) is eg
  var eg := Diff(SortedSignals[9], SortedSignals[1] + bd);
  // The one in eg that is in all numbers that are 6 long, is g
  var SixLongs := Copy(SortedSignals, 6, 3);
  if IsInAll(eg[1], SixLongs) then
  begin
    Result[7] := eg[1]; // g
    Result[5] := eg[2]; // e
  end else
  begin
    Result[5] := eg[1]; // e
    Result[7] := eg[2]; // g
  end;
  // The one in cf (1) that is in all numbers that are 6 long, is f
  if IsInAll(cf[1], SixLongs) then
  begin
    Result[6] := cf[1]; // f
    Result[3] := cf[2]; // c
  end else
  begin
    Result[3] := cf[1]; // c
    Result[6] := cf[2]; // f
  end;

  // Reverse the mapping
  // I don't want the Signal character d to be in mapping position a,
  // but mapping character a to be in position d.
  // That makes it easier to look up the characters in the output strings.
  var Mapping := Result;
  for var m := 1 to Length(Mapping) do
    Result[1 + Ord(Mapping[m]) - Ord('a')] := Char(Ord('a') + m - 1);
end;

function Normalize(const s: String): String;
begin
  // Sort characters in a string alphabetically
  var Chars := s.ToCharArray;
  TArray.Sort<Char>(Chars);
  SetLength(Result, Length(Chars));
  SetString(Result, PChar(@Chars[0]), Length(Chars));
end;

function Map(const Output, Mapping: String): String;
begin
  // The mapping is a string that contains the right character in the position
  // of the wrong character. So when the Output contains a d, look in position
  // d to find which character it should map to.
  SetLength(Result, Length(Output));
  for var o := 1 to Length(Output) do
    Result[o] := Mapping[1+Ord(Output[o])-Ord('a')];
end;

function Translate(const Outputs: TStringArray; const Mapping: String): Integer;
begin
  Result := 0;
  for var Output in Outputs do
  begin
    // Map and normalize (sort alphabetically),
    var Code := Normalize(Map(Output, Mapping));

    Result := Result * 10;
    // Look up the digit in the mapping table and add them to the result
    for var i := Low(Signals) to High(Signals) do
    begin
      if Signals[i] = Code then
      begin
        Result := Result + i;
        Break;
      end;
    end;
  end;
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  for var Input in Inputs do
  begin
    var Raw := Input.Split([' | ']);
    // Get all signals, order their chars in alphabetical order
    var Signals := Raw[0].Split([' ']);
    for var i := 0 to High(Signals) do
      Signals[i] := Normalize(Signals[i]);

    // Sort the signals by length, shortest (1, 7, 4) first.
    TArray.Sort<String>(Signals, TDelegatedComparer<String>.Construct(
      function(const A, B: String): Integer
      begin
        Result := Length(A) - Length(B);
        if Result = 0 then
          Result := CompareText(A, B);
      end));

    // Find the mapping by deduction
    var Mapping := FindMapping(Signals);

    // Translate the output strings to numbers using the found mapping
    var Outputs := Raw[1].Split([' ']);
    Inc(Result, Translate(Outputs, Mapping));
  end;
end;

var
  Input: TStringArray;
  Result: Int64;
begin
  // Unit tests
  Assert(Diff('abc', 'ac') = 'b', 'Diff test');
  Assert(Map('cda', 'gdfecab') = 'feg', 'Map test');
  Assert(Normalize('cda') = 'acd', 'Normalize test');

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day8.test.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 26);
  Result := Solve2(Input);
  ValidateNr(Result, 61229);

  WriteLn(#10'Final');
  Input := LoadStrings('Day8.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 548);

  var s := TStopwatch.StartNew;
  const Iterations = 0;
  // for var i := 1 to Iterations do
  Result := Solve2(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 1074888);

  WriteLn(#10'Hit it');
  ReadLn;
end.
