open Reprocessing;

let windowSize = (600., 600.);
let playerSize = (130., 30.);
let ballRadius = 20.;
let pVelocity = 450.;
let ballVelIncrement = 50.;

type winner = Player1 | Player2 | None;

type pongState = {
  started: bool,
  score: (int, int),
  player1Pos: (float, float),
  player2Pos: (float, float),
  ballPos: (float, float),
  player1V: float,
  player2V: float,
  ballV: (float, float),
  time: float,
  font: Reprocessing.fontT
};

let initialState = (env) => {
  let (windowW, windowH) = windowSize;
  let (playerW, playerH) = playerSize;
  let playersInitialPosX = (windowW /. 2.) -. (playerW /. 2.);
  let ballInitialSize = (
    (windowW /. 2.),
    (windowH /. 2.)
  );
  {
    started: false,
    score: (0, 0),
    time: 0.1,
    player1Pos: (playersInitialPosX, windowH -. playerH -. 5.),
    player2Pos: (playersInitialPosX, 5.),
    ballPos: ballInitialSize,
    player1V: 0.,
    player2V: 0.,
    ballV: (Utils.randomf(~min=(-200.), ~max=200.), 200.),
    font: Draw.loadFont(~filename="./assets/fonts/font.fnt", ~isPixel=true, env)
  }
};

let constrainPlayer = (amt) => {
  let (playerW, _) = playerSize;
  let (screenW, _) = windowSize;
  Utils.constrain(~amt, ~low=0.0, ~high=screenW -. playerW)
};

let playerCenter = (playerPos: (float, float)) => {
  let (playerX, _) = playerPos;
  let (sizeX, _) = playerSize;
  playerX +. (sizeX /. 2.)
};

let decrementVelocity = (velocity: float) =>
  if (velocity > 0.) {
    Utils.constrain(~amt=(velocity -. 15.), ~low=(0.), ~high=1000.)
  } else {
    Utils.constrain(~amt=(velocity +. 15.), ~low=(-1000.), ~high=0.)
  }
;

let newXVelOnCollision = (~ballPos: (float, float), ~playerPos: (float, float)) => {
  let (ballX, _) = ballPos;
  let ballCenter = ballX +. (ballRadius /. 2.);
  let playerCenter = playerCenter(playerPos);
  let distance = abs_float(ballCenter -. playerCenter);
  ballCenter <= playerCenter ? -. distance *. 8. : distance *. 8.;
};

let nextBallVelocity = (~ballPos: (float, float), ~ballV: (float, float), ~player1Pos: (float, float), ~player2Pos: (float, float)) => {
  let (windowW, windowH) = windowSize;
  let (ballXV, ballYV) = ballV;
  let (playerW, playerH) = playerSize;
  switch (
    (
      Utils.intersectRectCircle(~rectPos=(0., 0.), ~rectW=1., ~rectH=windowH, ~circlePos=ballPos, ~circleRad=ballRadius),
      Utils.intersectRectCircle(~rectPos=(windowW -. 2., 0.), ~rectW=2., ~rectH=windowH, ~circlePos=ballPos, ~circleRad=ballRadius),
      Utils.intersectRectCircle(~rectPos=player1Pos, ~rectW=playerW, ~rectH=playerH, ~circlePos=ballPos, ~circleRad=ballRadius),
      Utils.intersectRectCircle(~rectPos=player2Pos, ~rectW=playerW, ~rectH=playerH, ~circlePos=ballPos, ~circleRad=ballRadius)
    )
  ) {
    | (true, false, false, false) => (abs_float(ballXV), ballYV)
    | (false, true, false, false) => (-. ballXV, ballYV)
    | (false, false, true, false) => (newXVelOnCollision(~ballPos, ~playerPos=player1Pos), -. ballYV -. ballVelIncrement)
    | (false, false, false, true) => (newXVelOnCollision(~ballPos, ~playerPos=player2Pos), abs_float(ballYV) +. ballVelIncrement)
    | _ => ballV
  }
};

let nextIAVelocity = (~player2Pos: (float, float), ~player2Vel: float, ~ballPos: (float, float)) => {
  let p2center = playerCenter(player2Pos);
  let (ballX, _) = ballPos;
  if (abs_float(ballX -. p2center) > 20.) {
    (p2center < ballX) ? pVelocity : -.pVelocity
  } else {
    (player2Vel != 0.) ? decrementVelocity(player2Vel) : 0.
  }
};

let gameover = (ballY) =>
  if (ballY <= 0.0) {
    Player1
  } else if (ballY >= 600.0) {
    Player2
  } else {
    None
  }
;

let updateScore = (winner: winner, score) => {
  let (score1, score2) = score;
  switch(winner) {
  | Player1 => (score1 + 1, score2)
  | Player2 => (score1, score2 + 1)
  | _ => score
  }
};

let setup = (env) => {
  let (windowW, windowH) = windowSize;
  Env.size(~width=int_of_float(windowW), ~height=int_of_float(windowH), env);
  initialState(env)
};

let draw = ({ font, started, time, player1Pos, player2Pos, player1V, player2V, ballPos, ballV, score } as state, env) => {
  let (playerW, playerH) = playerSize;
  let (player1X, player1Y) = player1Pos;
  let (player2X, player2Y) = player2Pos;
  let (ballX, ballY) = ballPos;
  let (score1, score2) = score;
  let deltaTime = Env.deltaTime(env);
  let (new_ballVX, new_ballVY) as newBallVelocity = nextBallVelocity(~ballPos, ~ballV, ~player1Pos, ~player2Pos);
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Constants.black, env);
  Draw.rectf(~pos=player1Pos, ~width=playerW, ~height=playerH, env);
  Draw.rectf(~pos=player2Pos, ~width=playerW, ~height=playerH, env);
  Draw.ellipsef(~center=ballPos, ~radx=ballRadius, ~rady=ballRadius, env);
  switch ((started, gameover(ballY))) {
  | (true, None) => {
      ...state,
      time: time +. 0.1,
      player1Pos: (constrainPlayer(player1X +. player1V *. deltaTime), player1Y),
      player2Pos: (constrainPlayer(player2X +. player2V *. deltaTime), player2Y),
      ballV: newBallVelocity,
      ballPos: (ballX +. new_ballVX *. deltaTime, ballY +. new_ballVY *. deltaTime),
      player2V: nextIAVelocity(~player2Pos, ~player2Vel=player2V, ~ballPos)
    }
  | (true, winner) => {
      ...initialState(env),
      score: updateScore(winner, score)
    }
  | (false, _) => {
      Draw.pushMatrix(env);
      Draw.text(~font, ~body="Player: " ++ string_of_int(score1), ~pos=((Env.width(env) / 2 - 160), 100), env);
      Draw.text(~font, ~body="CPU: " ++ string_of_int(score2), ~pos=((Env.width(env) / 2) + 40, 100), env);
      Draw.translate(~x=float_of_int(Env.width(env) / 2 - 90), ~y=200. +. (sin(time) *. 2.0), env);
      Draw.text(~font, ~body="Press Space", ~pos=(0, 0), env);
      Draw.popMatrix(env);
      {
        ...state,
        time: time +. 0.1
      }
    }
  }
};

let keyPressed = (state, env) =>
  Events.(
    switch (Env.keyCode(env)) {
    | Space => { ...state, started: true }
    | (Left | A) => { ...state, player1V: -.(pVelocity) }
    | (Right | D) => { ...state, player1V: pVelocity }
    | _ => state
    }
  );

let keyReleased = (state, env) =>
  Events.(
    switch (Env.keyCode(env)) {
    | (Left | A) => { ...state, player1V: state.player1V > 0. ? state.player1V : 0. }
    | (Right | D) => { ...state, player1V: state.player1V < 0. ? state.player1V : 0. }
    | _ => state
  }
);

run(~setup, ~draw, ~keyPressed, ~keyReleased, ());
