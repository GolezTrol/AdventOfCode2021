program Day21;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  System.Math.Vectors,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;
  Int = BestInt;

  TPlayer = record
    Score: BigInt;
    Track: Integer;
  end;
  TGame = record
    Player: array[Boolean] of TPlayer;
  end;

function Play(const Start1, Start2: Integer): BigInt;
begin
  var Game: TGame := Default(TGame);
  var Player: Boolean;
  var Roll: Integer := 0;
  Game.Player[False].Track := Start1 - 1;
  Game.Player[True].Track := Start2 - 1;
  Player := True; // Player2/true is 'active'. Player 1/false will take the first turn.
  var Die := 2;
  repeat
    Player := not Player;
    var Move := Die * 3;
    Inc(Game.Player[Player].Track, Move);
    Game.Player[Player].Track := Game.Player[Player].Track mod 10;
    Inc(Game.Player[Player].Score, Game.Player[Player].Track+1);
    Inc(Die, 3);
    Inc(Roll, 3);
  until Game.Player[Player].Score >= 1000;

  Result := Roll * Game.Player[not Player].Score;
end;

// There is an infinite number of states a game can be in.
// A state is the position and score of either player, and who's turn it is.
// This can be encoded in an array index. Some of those will never be used, but
// it's a relatively simple solution.

// GetIndex calculates the index in the state array, based on the state parameters
function GetIndex(Pos1, Pos2, Score1, Score2, Turn: Integer): Integer; inline;
begin
  //ppppPPPPsssssSSSSSt
  Result := Pos1;
  Result := (Result shl 4) or Pos2;
  Result := (Result shl 5) or Score1;
  Result := (Result shl 5) or Score2;
  Result := (Result shl 1) or Turn;
end;
// GetState is the reverse of GetIndex
procedure GetState(const Index: Integer; out Pos1, Pos2, Score1, Score2, Turn: Integer); inline;
begin
  //ppppPPPPsssssSSSSSt
  Turn := Index and 1;
  Score2 := (Index shr 1) and 31;
  Score1 := (Index shr 6) and 31;
  Pos2 := (Index shr 11) and 15;
  Pos1 := (Index shr 15) and 15;
end;
// GetNextIndex plays a turn. It decodes the given index, applies a turn for
// the number of eyes specified, and returns the new index.
// Also returns the winner, if any, so the new index can be marked 'final'
// without having to decode it every time.
function GetNextIndex(const Index: Integer; const Eyes: Integer; out Winner: Integer): Integer;
begin
  Winner := 0;
  var Pos1, Pos2, Score1, Score2, Turn: Integer;
  GetState(Index, Pos1, Pos2, Score1, Score2, Turn);
  if Turn = 0 then
  begin
    Turn := 1;
    Inc(Pos1, Eyes);
    if Pos1 > 10 then
      Dec(Pos1, 10);
    Inc(Score1, Pos1);
    if Score1 >= 21 then
      Winner := 1;
  end
  else
  begin
    Turn := 0;
    Inc(Pos2, Eyes);
    if Pos2 > 10 then
      Dec(Pos2, 10);
    Inc(Score2, Pos2);
    if Score2 >= 21 then
      Winner := 2;
  end;
  Result := GetIndex(Pos1, Pos2, Score1, Score2, Turn);
end;

const
  // (Premature) optimization. The 3 dice rolls only result in 7 different outcomes
  // Pre-calculate how many times each of the number of eyes occurs.
                                         // 3, 4, 5, 6, 7, 8, 9
  EyeProbability: array[3..9] of Integer = (1, 3, 6, 7, 6, 3, 1);

type
  // Per index, keep track of how many universes are currently in that state,
  // and if a winner is known already.
  TGameState = record
    Universes: BigInt;
    Winner: Integer;
  end;

// Unit test for index encoding/decoding
procedure ValidateIndex();
begin
  var Index := GetIndex(10, 9, 21, 20, 1);
  var p1, p2, s1, s2, t: Integer;
  GetState(Index, p1, p2, s1, s2, t);
  ValidateNr(p1, 10);
  ValidateNr(p2, 9);
  ValidateNr(s1, 21);
  ValidateNr(s2, 20);
  ValidateNr(t, 1);
end;

// Unit test for playing a turn
procedure ValidateNextIndex();
begin
  var Index := GetIndex(10, 9, 21, 20, 0);
  var W: Integer;
  var Next := GetNextIndex(Index, 3, W);
  var p1, p2, s1, s2, t: Integer;
  GetState(Next, p1, p2, s1, s2, t);
  ValidateNr(p1, 3);
  ValidateNr(p2, 9);
  ValidateNr(s1, 24);
  ValidateNr(s2, 20);
  ValidateNr(t, 1);
  ValidateNr(W, 1);
end;

function Play2(const Start1, Start2: Integer): BigInt;
begin
  // I chose an array. A dictionary might work as well or better.
  // Iterating the array is simpler, but it's 500K items, while there are at
  // most around 20K states in use at the same time.
  var GameStates: TArray<TGameState>;
  SetLength(GameStates, 1 shl 19); // The game state is encoded in 19 bits

  // Initialize first universe
  var Index := GetIndex(Start1, Start2, 0, 0, 0);
  GameStates[Index].Universes := 1;

  for var Turn := 0 to 41 do
  begin
    var Count := 0;

    // Always at least 1 point per turn, so no need for more turns than this.
    // Die fast and hard, rather than waiting for an infinite loop after a mistake
    if Turn = 41 then
      raise Exception.Create('Too many turns');

    for var i := Low(GameStates) to High(GameStates) do
    begin
      // Are there universes in which the game is in the current state, and the
      // game has no winner yet.
      if (GameStates[i].Universes > 0) and (GameStates[i].Winner = 0) then
      begin
        Inc(Count); // Remember we played somewhere during this iteration

        // The 27 dice rolls, condenced to 7 different amounts of eyes
        for var Eyes := Low(EyeProbability) to High(EyeProbability) do
        begin
          var Winner: Integer;

          // Calculate what state we get in when we throw <eyes> in the current game
          var NewIndex := GetNextIndex(i, Eyes, Winner);

          // Increment the number of universes in the new state,
          // so - if 14 universes are playing the current state,
          //    - and Eyes is 4, which has a probability of 3
          //    - There will be 14 * 3 more universes in the new state
          GameStates[NewIndex].Universes :=
              GameStates[NewIndex].Universes
              + GameStates[i].Universes * EyeProbability[Eyes];

          //if Turn = 0 then
          //begin
          //  WriteLn('index ', NewIndex, ' prob ', Probability, ' uni ', GameStates[NewIndex].Universes);
          //end;

          // Remember if that state has a winner. Saves decoding.
          GameStates[NewIndex].Winner := Winner;
        end;
        // From the current state we went to all different new states
        // There are now 0 universes left in the current state.
        GameStates[i].Universes := 0;
      end;
    end;

    WriteLn('Turn ', Turn, '. Count ', Count);

    if Count = 0 then
      Break;
  end;

  // Determine how many universes the winning player has (puzzle outcome)
  var W1, W2: BigInt;
  W1 := 0;
  W2 := 0;

  for var i := Low(GameStates) to High(GameStates) do
  begin
    if GameStates[i].Winner = 1 then W1 := W1 + GameStates[i].Universes;
    if GameStates[i].Winner = 2 then W2 := W2 + GameStates[i].Universes;
  end;

  if W1 < W2 then
    Exit(W2);

  Exit(W1);
end;

begin
  WriteLn('Index to gamestate and back');
  ValidateIndex;
  ValidateNextIndex;

  WriteLn('Test');
  //ReadLn;exit;

  //Player 1 starting position: 4
  //Player 2 starting position: 8
  ValidateNr(Play(4, 8), 739785);
  ValidateNr(Play2(4, 8), 444356092776315);

  WriteLn('Actual');
  // Player 1 starting position: 10
  // Player 2 starting position: 8
  ValidateNr(Play(10, 8), 752247);
  var s := TStopwatch.StartNew;
  ValidateNr(Play2(10, 8), 221109915584112);
  WriteLn(s.ElapsedMilliseconds, ' ms');

  WriteLn(#10'Hit it');
  ReadLn;
end.
