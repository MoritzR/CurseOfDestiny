export interface GameSnapshot {
  currentPlayer: string;
  nextCardId: number;
  players: PlayerSnapshot[];
}

export interface PlayerSnapshot {
  playerId: string;
  name: string;
  schicksalsmacht: number;
  hand: CardSnapshot[];
  field: CardSnapshot[];
  graveyard: CardSnapshot[];
  deck: CardSnapshot[];
}

export interface CardSnapshot {
  cardId: number;
  owner: string;
  name: string;
  cost: string;
  cardType: string;
  baseStrength: number | null;
  currentStrength: number | null;
  tags: string[];
  description: string;
  modifications: ModificationSnapshot[];
}

export interface ModificationSnapshot {
  kind: string;
  duration: string;
  amount: number | null;
  description: string;
}

export type Prompt =
  | { tag: 'CommandPrompt'; message: string }
  | { tag: 'ChoicePrompt'; message: string; options: string[] }
  | { tag: 'NumberPrompt'; message: string };

export type ServerMessage =
  | { type: 'Connected'; message: string }
  | { type: 'StateSnapshot'; state: GameSnapshot }
  | { type: 'PromptMessage'; prompt: Prompt }
  | { type: 'NoticeMessage'; message: string }
  | { type: 'ErrorMessage'; message: string };

export type ClientMessage =
  | { type: 'SubmitCommand'; command: string }
  | { type: 'SubmitChoice'; choiceIndex: number }
  | { type: 'SubmitNumber'; number: number };
