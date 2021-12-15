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
    Connections: TArray<TCave>;
    Visits: Integer;
  end;

  TDummy = Boolean;
  TCaveList = TList<TCave>;
  TPath = record
    Caves: TArray<TCave>;
    function ToString: String; overload;
    class function ToString(const Caves: TArray<TCave>): String; overload; static;
    class function Create(Caves: TCaveList): TPath; static;
  end;
  TPaths = TDictionary<String, TPath>;

  TCaveSystem = class
    Caves: TObjectDictionary<String, TCave>;
    S, E: TCave;
  public
    constructor Create;
    procedure AddPath(Path: String);
  end;

  TPathFinder = class
  strict private
    s, e, x: TCave;
    MaxSmallCaveVisits: Integer;
    procedure FindPathsFrom(const From: TCave; const Path: TCaveList; const Paths: TPaths);
  public
    function FindPaths: TPaths;
    constructor Create(CaveSystem: TCaveSystem; MaxSmallCaveVisits: Integer);
  end;

function Solve(const Inputs: TStringArray; const MaxSmallCaveVisits: Integer): BigInt;
begin
  var cs := TCaveSystem.Create;
  for var Input in Inputs do
    cs.AddPath(Input);

  var PathFinder := TPathFinder.Create(cs, MaxSmallCaveVisits);

  var Paths := PathFinder.FindPaths;

  Result := Paths.Count;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 1);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 2);
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
      Caves.Add(Name, Result)
    end;
    if Name = 'start' then
      s := Result;
    if Name = 'end' then
      e := Result;
  end;
  procedure Connect(const A, B: TCave);
  begin
    // No paths to start or from end? Doesn't matter.
    //if (B <> s) and (A <> e) then
    begin
      SetLength(A.Connections, Length(A.Connections)+1);
      A.Connections[High(A.Connections)] := B;
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

{ TPath }

function TPath.ToString: String;
begin
  Result := ToString(Caves);
end;

class function TPath.Create(Caves: TCaveList): TPath;
begin
  SetLength(Result.Caves, Caves.Count);
  for var i := 0 to Caves.Count - 1 do
    Result.Caves[i] := Caves[i];
end;

class function TPath.ToString(const Caves: TArray<TCave>): String;
begin
  Result := '';
  for var Cave in Caves do
    Result := Result + '-' + Cave.Name;
  if Result <> '' then
    Delete(Result, 1, 1);
end;

{ TPathFinder }

constructor TPathFinder.Create(CaveSystem: TCaveSystem;
  MaxSmallCaveVisits: Integer);
begin
  s := CaveSystem.S;
  e := CaveSystem.E;
  Self.MaxSmallCaveVisits := MaxSmallCaveVisits;
end;

function TPathFinder.FindPaths: TPaths;
begin
  var Path := TCaveList.Create;
  Result := TDictionary<String, TPath>.Create;
  FindPathsFrom(s, Path, Result);
  Assert(Path.Count = 0, 'Backtracked all the way');
end;

procedure TPathFinder.FindPathsFrom(
  const From: TCave;
  const Path: TCaveList;
  const Paths: TPaths);
begin
  // Already been in this small cave too often? Bail.
  if (From.Visits = MaxSmallCaveVisits) and not From.Large then
    Exit;

  // Small cave, already visited, and not the one that can be visited more than once
  if (From.Visits = 1) and Assigned(x) and (From <> x) and not From.Large then
    Exit;

  // Do not go back to start
  if (From = s) and (From.Visits = 1) then
    Exit;

  // Valid target. Add to path.
  Path.Add(From);

  // If it was the end cave, save the path
  if From = e then
  begin
    var FoundPath := TPath.Create(Path);
    var Key := FoundPath.ToString;
    if Paths.ContainsKey(Key) then
      WriteLn('( ', Key, ' )')
    else
    begin
      Paths.Add(Key, FoundPath);
      WriteLn('+ ', Key);
    end;
  end
  else
  begin
    // Search onwards towards all caves that can be reached from here
    Inc(From.Visits);
    if (From.Visits > 1) and not From.Large then
      x := From;

    for var Connection in From.Connections do
      FindPathsFrom(Connection, Path, Paths);
    // Backtrack. Reset visited flag (only relevant for small caves)
    Dec(From.Visits);
    if x = From then
      x := nil;

  end;

  Path.Delete(Path.Count - 1);
end;

begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day12.test.txt');
  Result := Solve1(Input);

  ValidateNr(Result, 10);
  Result := Solve2(Input);
  ValidateNr(Result, 36);

  WriteLn(#10'Final');
  Input := LoadStrings('Day12.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 4411);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  // for var i := 1 to Iterations do
  Result := Solve2(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 136767);

  WriteLn(#10'Hit it');
  ReadLn;
end.
