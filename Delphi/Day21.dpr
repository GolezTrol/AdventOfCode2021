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

  TSpace = record
    x: Integer;
  end;
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

begin
  WriteLn('Test');

  //Player 1 starting position: 4
  //Player 2 starting position: 8
  ValidateNr(Play(4, 8), 739785);

  WriteLn('Actual');
  // Player 1 starting position: 10
  // Player 2 starting position: 8
  ValidateNr(Play(10, 8), 0);

  WriteLn(#10'Hit it');
  ReadLn;
end.
