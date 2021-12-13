program Day12;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;


  TCave = class
    Name: String;
    Large: Boolean;
    Visited: Boolean;
    Paths: TArray<TCave>;
    Visits: TArray<Integer>;
  end;

  TCaveSystem = class
    Caves: TObjectDictionary<String, TCave>;
    S, E: TCave;
  public
    constructor Create;
    procedure AddPath(Path: String);
    procedure Reset;
  end;

function Solve1(const Inputs: TStringArray): BigInt;

begin
  var cs := TCaveSystem.Create;
  for var Input in Inputs do
    cs.AddPath(Input);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
end;

var
  Input: TStringArray;
  Result: Int64;

{ TCaveSystem }

procedure TCaveSystem.AddPath(Path: String);
  function GetCave(Name: String): TCave;
  begin
    if not Caves.TryGetValue(Name, Result) then
    begin
      Result := TCave.Create;
      Result.Name := Name;
      Result.Large := UpperCase(Name) = Name;
    end;
    if Name = 'start' then
      s := Result;
    if Name = 'end' then
      e := Result;
  end;
  procedure Connect(const A, B: TCave);
  begin
    // No paths to start or from end
    if (B <> s) and (A <> e) then
    begin
      SetLength(A.Paths, Length(A.Paths)+1);
      A.Paths[High(A.Paths)] := B;
    end;
  end;
begin
  var p := Path.Split(['-']);
  var c1 := GetCave(p[0]);
  var c2 := GetCave(p[1]);
  Connect(c1, c2);
  Connect(c2, c1);
end;

constructor TCaveSystem.Create;
begin
  Caves := TObjectDictionary<String, TCave>.Create;
end;

procedure TCaveSystem.Reset;
begin
  for var Cave in Caves.Values do
  begin
    Cave.Visited := False;
    SetLength(Cave.Visits, 0);
  end;
end;

begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day12.test.txt');
  Result := Solve1(Input);

  ValidateNr(Result, 10);
  Result := Solve2(Input);
  ValidateNr(Result, 0);

  WriteLn(#10'Final');
  Input := LoadStrings('Day12.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 0);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  // for var i := 1 to Iterations do
  Result := Solve2(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 0);

  WriteLn(#10'Hit it');
  ReadLn;
end.
