/**
 * Simple Chess Engine.
 * August 2018.
 * thomas.frey@almuni.ethz.ch
 * */
export const EMPTY = 0;
export const OUTSIDE = 1;
export const KING = 2;
export const QUEEN = 3;
export const ROOK = 4;
export const BISHOP = 5;
export const KNIGHT = 6;
export const PAWN = 7;

export const NONE = 0;
export const WHITE = 1;
export const BLACK = 2;

const OUTSIDE_SPACE = 2;
const STRIDE = 8 + OUTSIDE_SPACE * 2;

const WHITE_PIECES = [' ', 'X', '♔', '♕', '♖', '♗', '♘', '♙'];
const BLACK_PIECES = [' ', 'X', '♚', '♛', '♜', '♝', '♞', '♟'];
const NOBLES = [ROOK, KNIGHT, BISHOP, QUEEN, KING, BISHOP, KNIGHT, ROOK];
const ROOK_MOVES = [1 + 0 * STRIDE, 0 + 1 * STRIDE, -1 + 0 * STRIDE, 0 - 1 * STRIDE];
const BISHOP_MOVES = [1 + 1 * STRIDE, -1 - 1 * STRIDE, -1 + 1 * STRIDE, 1 - 1 * STRIDE];
const KNIGHT_MOVES: Position[] = [2 + STRIDE, 2 - STRIDE, -2 + STRIDE, -2 - STRIDE, 1 + 2 * STRIDE,
                                  1 - 2 * STRIDE, -1 + 2 * STRIDE, -1 - 2 * STRIDE];
const QUEEN_MOVES = [...BISHOP_MOVES, ...ROOK_MOVES];

export type Piece = number;
export type PlayerColor = number;

const MIN_VALUE = -9007199254740990;
const MAX_VALUE = 9007199254740990;
const PIECE_VALUE = [0, 0, 20000, 9, 5, 3, 3, 1];
const PAWN_RANK_FACTOR = [0, 0.9, 0.95, 1, 1.1, 1.2, 1.3, 1.4];
const PAWN_FILE_FACTOR = [0.9, 1, 1.1, 1.2, 1.2, 1.1, 1, 0.9];

export function makePosition(x: number, y: number): Position {
  return (y + OUTSIDE_SPACE) * STRIDE + OUTSIDE_SPACE + x;
}

export function positionToX(position: Position) {
  return position % STRIDE - OUTSIDE_SPACE;
}

export function positionToY(position: Position) {
  return Math.floor(position / STRIDE) - OUTSIDE_SPACE;
}

function positionToStr(position: Position) {
  return String.fromCharCode(65 + (position % STRIDE - OUTSIDE_SPACE)) +
      (Math.floor(position / STRIDE) - OUTSIDE_SPACE);
}

function isBlack(x: Piece): boolean {
  return (x & 8) > 0;
}

export function opponentColor(player: PlayerColor): PlayerColor {
  return player === WHITE ? BLACK : WHITE;
}

export function makePiece(pieceType: Piece, color: PlayerColor): Piece {
  return pieceType + (color === BLACK ? 8 : 0);
}

/* Strips the color of a Piece */
export function pieceType(x: Piece): Piece {
  return x & 7;
}

export function pieceToStr(x: Piece): string {
  return isBlack(x) ? BLACK_PIECES[pieceType(x)] : WHITE_PIECES[pieceType(x)];
}

function pieceColor(piece: Piece): PlayerColor {
  if (pieceType(piece) < KING) return NONE;
  return isBlack(piece) ? BLACK : WHITE;
}

export type Position = number;
type Direction = number;

export class Move {
  from: Position;
  to: Position;
  priority: number;
  stabilizer: number;
  replacement?: Piece;
  score?: number;

  constructor(from: Position, to: Position, priority: number) {
    this.from = from;
    this.to = to;
    this.priority = priority || 1;
  }

  toString() {
    return positionToStr(this.from) + '->' + positionToStr(this.to);
  }
}

export class MoveLogEntry {
  targetField: Piece;
  castleMove: boolean;
  enPassant: boolean;
  move: Move;
  moveState: number;
  replacement?: Piece;

  constructor(move: Move, lastMoveState: number, targetField: Piece, castleMove: boolean, enPassant: boolean) {
    this.move = move;
    this.targetField = targetField;
    this.castleMove = castleMove;
    this.enPassant = enPassant;
    this.moveState = lastMoveState;
  }
}

export class Board {

  field: Piece[];
  turn: PlayerColor;
  moveStack: MoveLogEntry[];
  bKingPosition: Position;
  wKingPosition: Position;

  moveState: number = 0; // first byte 0..7 white nobles 8..15 black nobles

  constructor() {
    this.field = new Array(STRIDE * STRIDE);
    this.reset();
  }

  reset() {
    this.moveStack = [];
    this.moveState = 0;
    this.field.fill(OUTSIDE, 0, this.field.length);
    this.wKingPosition = makePosition(4, 0);
    this.bKingPosition = makePosition(4, 7);
    for (let i = 0; i < 8; i++) {
      this.setField(i, 0, makePiece(NOBLES[i], WHITE));
      this.setField(i, 1, makePiece(PAWN, WHITE));
      for (let j = 2; j < 6; j++) {
        this.setField(i, j, EMPTY);
      }
      this.setField(i, 6, makePiece(PAWN, BLACK));
      this.setField(i, 7, makePiece(NOBLES[i], BLACK));
    }
    this.turn = WHITE;
  }

  setField(x: number, y: number, value: Piece) {
    this.field[makePosition(x, y)] = value;
  }

  setFieldAtPosition(position: Position, value: Piece) {
    this.field[position] = value;
  }

  getField(x: number, y: number): Piece {
    return this.field[makePosition(x, y)];
  }

  getFieldFromPosition(position: Position): Piece {
    return this.field[position];
  }

  isEmpty(x: number, y: number) {
    return this.getField(x, y) === EMPTY;
  }

  isEmptyAtPosition(position: Position) {
    return this.field[position] === EMPTY;
  }

  colorAt(x: number, y: number): PlayerColor {
    return pieceColor(this.getField(x, y));
  }

  colorAtPosition(podsition: Position): PlayerColor {
    return pieceColor(this.field[podsition]);
  }

  isOpponent(position: Position, player?: PlayerColor) {
    if (player === undefined) {
      player = this.turn;
    }
    return this.colorAtPosition(position) === opponentColor(player);
  }

  isCurrentPlayers(position: Position): boolean {
    return this.colorAtPosition(position) === this.turn;
  }

  private pieceValueAtPosition(position: Position) {
    return PIECE_VALUE[pieceType(this.field[position])];
  }

  performMove(m: Move) {
    const fromX = positionToX(m.from);
    const fromY = positionToY(m.from);
    const toX = positionToX(m.to);
    const toY = positionToY(m.to);

    let castleMove = false;
    if (!this.isCurrentPlayers(m.from)) {
      throw `not this makePiece\'s turn : ${positionToStr(m.from)}, ${pieceToStr(this.getFieldFromPosition(m.from))}`;
    }

    const targetPositionPiece = this.getFieldFromPosition(m.to);
    const lastMoveState = this.moveState;
    const piece = this.getFieldFromPosition(m.from);
    const isPawn = pieceType(piece) === PAWN;
    const black = isBlack(piece);
    const fromRank = black ? 7 - fromY : fromY;
    const toRank = black ? 7 - toY : toY;


    // Check if it is a en passant move
    const enPassant = (isPawn && toX != fromX && pieceType(targetPositionPiece) === EMPTY);

    this.setFieldAtPosition(m.to, piece);
    this.setFieldAtPosition(m.from, EMPTY);

    if (enPassant) {
      this.setField(toX, fromY, EMPTY);
    }

    // Check if it is a castle move
    if (pieceType(piece) === KING) {
      if (black) {
        this.bKingPosition = m.to;
      } else {
        this.wKingPosition = m.to;
      }

      if (fromRank === 0 && (Math.abs(fromX - toX) === 2)) {
        castleMove = true;
        if (fromX - toX > 0) {
          this.setField(3, toY, this.getField(0, toY));
          this.setField(0, fromY, EMPTY);
        } else {
          this.setField(5, toY, this.getField(7, toY));
          this.setField(7, fromY, EMPTY);
        }
      }
    }

    if (fromRank === 0) {
      this.moveState |= 1 << ((black ? 8 : 0) + fromX);
    }
    this.moveStack.push(new MoveLogEntry(m, lastMoveState, targetPositionPiece, castleMove, enPassant));

    // replacement
    if (m.replacement && isPawn && (toRank === 7)) {
      this.switchNoble(toX, toY, m.replacement);
    }
  }

  undo() {
    if (!this.moveStack.length) throw 'Cannot undo nothing';
    let move = this.moveStack.pop();
    let m = move.move;
    if (move.replacement) {
      this.setFieldAtPosition(m.to, makePiece(PAWN, this.colorAtPosition(m.to)));
    }
    const fromY = positionToY(m.from);
    const toX = positionToX(m.to);
    if (move.castleMove) {
      const fromX = positionToX(m.from);
      const toY = positionToY(m.to);
      if (fromX - toX > 0) {
        this.setField(0, toY, this.getField(3, toY));
        this.setField(3, fromY, EMPTY);
      } else {
        this.setField(7, toY, this.getField(5, toY));
        this.setField(5, fromY, EMPTY);
      }
    }

    const piece = this.getFieldFromPosition(m.to);
    if (pieceType(piece) === KING) {
      if (isBlack(piece)) {
        this.bKingPosition = m.from;
      } else {
        this.wKingPosition = m.from;
      }
    }
    this.setFieldAtPosition(m.from, piece);
    this.setFieldAtPosition(m.to, move.targetField);

    if (move.enPassant) {
      this.setField(toX, fromY, makePiece(PAWN, opponentColor(pieceColor(piece))));
    }
    this.moveState = move.moveState;
  }

  getPossibleMoves(position: Position, excludeCastle?: boolean): Move[] {
    let moves: Move[] = [];
    const fig = this.getFieldFromPosition(position);
    const color = pieceColor(fig);
    switch (pieceType(fig)) {
      case BISHOP:
        return this.generateMultiMoves(BISHOP_MOVES, position, color);
      case KING:
        return this.kingMoves(position, color, excludeCastle);
      case KNIGHT:
        return this.knightMoves(position, color);
      case PAWN:
        return this.pawnMoves(position, color);
      case QUEEN:
        return this.generateMultiMoves(QUEEN_MOVES, position, color);
      case ROOK:
        return this.generateMultiMoves(ROOK_MOVES, position, color);
    }
    return moves;
  }

  isLegalMove(m: Move): boolean {
    this.performMove(m);
    let check = this.isCheck();
    this.undo();
    return !check;
  }

  // Checks all possible moves and filters out ones that would bring the king into check.
  getLegalMoves(position: Position): Move[] {
    return this.getPossibleMoves(position).filter(m => this.isLegalMove(m));
  }

  getAllLegalMoves(): Move[] {
    let moves: Move[] = [];
    for (let y = 0; y < 8; y++) {
      for (let x = 0; x < 8; x++) {
        const position = makePosition(x, y);
        if (this.isCurrentPlayers(position)) {
          const legalMoves = this.getLegalMoves(position);
          if (legalMoves.length) {
            moves.push(...legalMoves);
          }
        }
      }
    }
    return moves;
  }

  // ignores en-passant, supposed to be used to check if the king is attacked
  private isAttackedBy(position, attackerColor: PlayerColor): boolean {

    const direction = attackerColor == WHITE ? STRIDE : -STRIDE;
    const attackerPawn = makePiece(PAWN, attackerColor);

    if ((this.getFieldFromPosition(position - direction - 1) === attackerPawn) ||
        (this.getFieldFromPosition(position - direction + 1) === attackerPawn)) {
      return true;
    }

    const attackerKnight = makePiece(KNIGHT, attackerColor);
    for (let i in KNIGHT_MOVES) {
      if (this.field[position + KNIGHT_MOVES[i]] === attackerKnight) {
        return true;
      }
    }

    if (this.reachableQueenKingOrPiece(ROOK_MOVES, position, attackerColor, ROOK)) return true;
    return this.reachableQueenKingOrPiece(BISHOP_MOVES, position, attackerColor, BISHOP);
  }

  private reachableQueenKingOrPiece(directions: Direction[], position: Position, attackerColor: PlayerColor,
                                    wantedPiece: Piece): boolean {
    const attackerKing = makePiece(KING, attackerColor);
    const attackerQueen = makePiece(QUEEN, attackerColor);
    const attackerPiece = makePiece(wantedPiece, attackerColor);

    for (let i = 0; i < directions.length; i++) {
      let t = position + directions[i];
      let piece = this.field[t];

      if (piece === attackerKing) {
        return true;
      }
      while (piece === EMPTY) {
        t += directions[i];
        piece = this.field[t];
      }
      if (piece === attackerQueen || piece === attackerPiece) {
        return true;
      }
    }
    return false;
  }

  private knightMoves(position: Position, player: PlayerColor): Move[] {
    let moves: Move[] = [];
    const opponent = opponentColor(player);
    for (let i in KNIGHT_MOVES) {
      let toPos = position + KNIGHT_MOVES[i];
      let targetPiece = this.field[toPos];
      if (targetPiece === EMPTY) {
        moves.push(new Move(position, toPos, 1));
      } else if (pieceColor(targetPiece) === opponent) {
        moves.push(new Move(position, toPos, this.pieceValueAtPosition(toPos)));
      }
    }
    return moves;
  }

  private pawnMoves(position: Position, player: PlayerColor): Move[] {
    let moves: Move[] = [];
    let y = positionToY(position);
    let x = positionToX(position);
    const white = player === WHITE;
    const direction = white ? STRIDE : -STRIDE;
    const untouched = white ? y === 1 : y === 6;
    const checkEnPassant = (white) && y === 4 || !white && y === 3;

    if (checkEnPassant) {
      const previousMove = this.moveStack[this.moveStack.length - 1].move;
      if (positionToY(previousMove.to) === y
          && (positionToX(previousMove.to) === x - 1 || positionToX(previousMove.to) === x + 1) &&
          this.getFieldFromPosition(previousMove.to) === makePiece(PAWN, opponentColor(player))) {
        moves.push(new Move(position, previousMove.to + direction, 2));
      }
    }

    if (untouched && this.isEmptyAtPosition(position + direction) && this.isEmptyAtPosition(position + 2 * direction)) {
      moves.push(new Move(position, position + 2 * direction, 1));
    }
    let t = position + direction;

    const pushMove = (m: Move) => {
      if (positionToY(m.to) === 0 || positionToY(m.to) === 7) {
        m.replacement = makePiece(QUEEN, player);
        let move2 = new Move(m.from, m.to, m.priority);
        move2.replacement = makePiece(KNIGHT, player);
        moves.push(move2);
      }
      moves.push(m);
    };
    if (this.isEmptyAtPosition(t)) {
      pushMove(new Move(position, t, 1));
    }
    if (this.isOpponent(t - 1, player)) {
      pushMove(new Move(position, t - 1, this.pieceValueAtPosition(t - 1)));
    }
    if (this.isOpponent(t + 1, player)) {
      pushMove(new Move(position, t + 1, this.pieceValueAtPosition(t + 1)));
    }
    return moves;
  }

  private kingMoves(position, player: PlayerColor, excludeCastle?: boolean) {
    let moves: Move[] = [];
    for (let i = 0; i < QUEEN_MOVES.length; i++) {
      let t = position + QUEEN_MOVES[i];
      if (this.isEmptyAtPosition(t)) {
        moves.push(new Move(position, t, 1));
      } else if (this.isOpponent(t, player)) {
        moves.push(new Move(position, t, this.pieceValueAtPosition(t)));
      }
    }

    if (excludeCastle) return moves;

    const row = player === WHITE ? 0 : 7;
    const black = player !== WHITE;
    // king never moved?

    if (((this.moveState & 1 << (black ? 8 : 0) + 4) === 0) && !this.isCheck()) {
      //sanity check
      if (this.getField(4, row) !== makePiece(KING, player)) {
        throw 'magic happened to the king';
      }

      const emptyAndSafe = (position: Position): boolean => {
        return this.isEmptyAtPosition(position) && !this.isAttackedBy(position, opponentColor(player));
      };

      let rook = makePiece(ROOK, player);
      // Long side
      let p0 = makePosition(0, row);
      let p1 = p0 + 1;
      let p2 = p0 + 2;
      let p3 = p0 + 3;

      if (this.getFieldFromPosition(p0) == rook && ((this.moveState & 1 << (black ? 8 : 0) + 0) === 0)
          && this.isEmptyAtPosition(p1) && emptyAndSafe(p2) && emptyAndSafe(p3)) {
        moves.push(new Move(position, position - 2, 1));
      }
      // Short side
      let p5 = makePosition(5, row);
      let p6 = p5 + 1;
      let p7 = p5 + 2;
      if (this.getFieldFromPosition(p7) == rook && ((this.moveState & 1 << (black ? 8 : 0) + 7) === 0)
          && emptyAndSafe(p5) && emptyAndSafe(p6)) {
        moves.push(new Move(position, position + 2, 1));
      }
    }
    return moves;
  }

  private generateMultiMoves(directions, position: Position, player: PlayerColor) {
    let moves: Move[] = [];
    for (let i = 0; i < directions.length; i++) {
      let t = position + directions[i];

      while (this.isEmptyAtPosition(t)) {
        moves.push(new Move(position, t, 1));
        t += directions[i];
      }
      if (this.isOpponent(t, player)) {
        moves.push(new Move(position, t, this.pieceValueAtPosition(t)));
      }
    }
    return moves;
  }

  isCheck(): boolean {
    let kingPos = (this.turn === WHITE) ? this.wKingPosition : this.bKingPosition;
    return this.isAttackedBy(kingPos, (this.turn === WHITE) ? BLACK : WHITE);
  }

  isCheckMate(): boolean {
    return this.isCheck() && (this.getAllLegalMoves().length === 0);
  }

  isStaleMate(): boolean {
    return this.getAllLegalMoves().length === 0;
  }

  positionScore(): number {
    let score = 0;
    for (let y = 0; y < 8; y++) {
      for (let x = 0; x < 8; x++) {
        const field = this.getField(x, y);
        const kind = pieceType(field);
        // only non-empty;
        if (kind >= KING) {
          const color = pieceColor(field);
          const signum = color === WHITE ? 1 : -1;
          if (kind === PAWN) {
            const rank = color === WHITE ? y : 7 - y;
            score += signum * PIECE_VALUE[kind] * PAWN_RANK_FACTOR[rank] * PAWN_FILE_FACTOR[x];
          } else {
            score += signum * PIECE_VALUE[kind];
          }
        }
      }
    }
    return score;
  }

  bestMove2(maxDepth: number, white: boolean): Move {
    let bestMove = null;
    let scoredMoves = [];
    // https://en.wikipedia.org/wiki/Negamax
    const negaMax = (depth: number, alpha: number, beta: number, color: number) => {
      let legalMoves = this.getAllLegalMoves();
      if (depth === maxDepth) {
        scoredMoves = legalMoves;
      }

      if (legalMoves.length === 0 && this.isCheck()) {
        return -color * (white ? -1 : 1) * (MIN_VALUE + (maxDepth - depth));
      } else if (legalMoves.length === 0) {
        return 0;
      } else if (depth === 0) {
        return -color * this.positionScore();
      }

      // ensure that sorting in all browsers is the same to ensure it plays the same move if there is no
      // explicit randomness.
      for (let i = 0; i < legalMoves.length; i++) {
        legalMoves[i].stabilizer = i;
      }
      legalMoves.sort((a, b) => {
        const d = (b.priority - a.priority);
        return d === 0 ? a.stabilizer - b.stabilizer : d;
      });
      let score = MIN_VALUE;
      for (let i = 0; i < legalMoves.length; i++) {
        const move = legalMoves[i];
        this.performMove(move);

        this.internalOtherTurn();
        let thisScore = -negaMax(depth - 1, -beta, -alpha, -color);
        score = Math.max(score, thisScore);

        this.internalOtherTurn();
        this.undo();
        if (depth === maxDepth) {
          move.score = thisScore;
          if (score > alpha) {
            bestMove = move;
          }
        }
        alpha = Math.max(alpha, score);
        if (alpha >= beta) {
          return score;
        }
      }
      return score;
    };
    negaMax(maxDepth, MIN_VALUE, MAX_VALUE, white ? -1 : 1);
    scoredMoves.sort((a, b) => b.score - a.score);
    let candidates = scoredMoves.filter(x => x.score === scoredMoves[0].score);
    return (candidates.length > 1) ? candidates[Math.floor(Math.random() * candidates.length)] : bestMove;
  }

  internalOtherTurn() {
    this.turn = opponentColor(this.turn);
  }

  switchNoble(x: number, y: number, noble: Piece) {
    if (y !== 0 && y !== 7) throw '';
    this.setField(x, y, noble);
    this.moveStack[this.moveStack.length - 1].replacement = noble;
  }

  pieces(): number {
    let c = 0;
    for (let y = 0; y < 8; y++) {
      for (let x = 0; x < 8; x++) {
        if (!this.isEmpty(x, y)) c++;
      }
    }
    return c;
  }
}